MODULE io
  USE netcdf
  USE drifter_mod
  PUBLIC

CONTAINS

SUBROUTINE initialize_in(nvar, fname, ncid, varid, nx, ny)
  IMPLICIT none

  INTEGER ncid
  INTEGER, intent(in) :: nvar
  CHARACTER(*), intent(in) :: fname
  INTEGER, intent(out) :: nx, ny

! Names from rtofs output
  CHARACTER(len=40) :: varnames(nvar)
  INTEGER varid(nvar)
  
! For Netcdf processing
  INTEGER i
  INTEGER retcode

  PRINT *,'entered initialize_in'
!debug:  PRINT *,'nvar, fname, ncid, varid, nx, ny ',nvar, fname, ncid, varid, nx, ny
!! This is the cice_inst variable set -- much more extensive
!  varnames(1) = "TLON"
!  varnames(2) = "TLAT"
!  varnames(3) = "ULON"
!  varnames(4) = "ULAT"
!  varnames(5) = "hi"
!  varnames(6) = "hs"
!  varnames(7) = "Tsfc"
!  varnames(8) = "aice"
!  varnames(9) = "uvel"
!  varnames(10) = "vvel"
!  varnames(11) = "fswdn"
!  varnames(12) = "flwdn"
!  varnames(13) = "snow"
!  varnames(14) = "snow_ai"
!  varnames(15) = "rain_ai"
!  varnames(16) = "sst"
!  varnames(17) = "uocn"
!  varnames(18) = "vocn"
!  varnames(19) = "meltt"
!  varnames(20) = "meltb"
!  varnames(21) = "meltl"
!  varnames(22) = "strength"
!  varnames(23) = "divu"
!  varnames(24) = "Tair"
!debug: 
!  PRINT *,"named all 24 cice_inst variables"
!RG: make file type an input parameter 2ds_ice:
  varnames(1) = "Longitude"
  varnames(2) = "Latitude"
  varnames(3) = "ice_coverage"
  varnames(4) = "ice_temperature"
  varnames(5) = "ice_thickness"
  varnames(6) = "ice_uvelocity"
  varnames(7) = "ice_vvelocity"
!debug: PRINT *,"named all 7 2ds_ice variables"
  

!debug:  PRINT *,"fname = ",fname
  retcode = nf90_open(fname, NF90_NOWRITE, ncid)
  CALL check(retcode)
!debug: PRINT *,"opened fname",fname

  DO i = 1, nvar
    !debug: PRINT *,i,varnames(i)
    retcode = nf90_inq_varid(ncid, varnames(i), varid(i))
    CALL check(retcode)
  ENDDO

  nx = 4500
  ny = 3298

  !debug: 
  PRINT *,'leaving initialize_in'


RETURN
END subroutine initialize_in

!----------------------------------------------------------------
SUBROUTINE initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                        allvars, ulon, ulat, dx, dy, rot, &
                        outdimids, ncid_out, varid_out, nvar_out, nbuoy)

  USE drifter_mod
  IMPLICIT none

  INTEGER, intent(in) :: nx, ny
  CHARACTER(90), intent(in) :: fname, drift_name, outname
  INTEGER :: nvar, ncid, varid(nvar)
  INTEGER, intent(out) :: outdimids(1)
  INTEGER nvar_out, nbuoy
  INTEGER ncid_out, ncid_drift
  INTEGER varid_out(nvar_out), varid_driftic(nvar_out)
  
  REAL, intent(inout) :: allvars(nx, ny, nvar)
  REAL, intent(inout) :: ulat(nx, ny), ulon(nx, ny)
  REAL, intent(out)   :: dx(nx, ny), dy(nx, ny), rot(nx, ny)

! Locals:
!  INTEGER i, j, k

! Allocate space for variables and initialize the netcdf reading

! Forcing / velocities
  !Get first set of data and construct the local metric for drifting
  !debug: PRINT *,' calling read ',nx, ny, nvar, ncid
  CALL read(nx, ny, nvar, ncid, varid, allvars)
  !debug: PRINT *,'done with first read'

!cice_inst:
!  ulat = allvars(:,:,4)
!  ulon = allvars(:,:,3)
!2ds_ice:
  ulat = allvars(:,:,2)
  ulon = allvars(:,:,1)
  !debug: PRINT *,'about to call metric', MAXVAL(ulat), MAXVAL(ulon)
  CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  !debug: PRINT *,'computed the local metric'

!  !----------- Initialize buoys, this should be a read in 
!  CALL initialize_drifter(drift_name, ncid_drift, varid_driftin, nvar_out, buoys)
  nbuoy = 89700
!  CALL close_out(ncid_drift)

! Initialize Output -- need definite sizes
  !debug: 
  PRINT *,'about to initialize_out'
  CALL initialize_out(outname, ncid_out, varid_out, nvar_out, nbuoy, outdimids)
  !debug: 
  PRINT *,'initialized the output'

END SUBROUTINE initial_read

!----------------------------------------------------------------
SUBROUTINE read(nx, ny, nvars, ncid, varid, allvars)
  IMPLICIT none
  INTEGER, intent(in)    :: nvars
  INTEGER, intent(in)    :: nx, ny, ncid, varid(nvars)
  REAL, intent(inout)    :: allvars(nx, ny, nvars)
  
  INTEGER i, retcode
  
  !debug: PRINT *,'entered read',nx, ny, ncid
  !got nx, ny from the .nc file, in initialize_in

  DO i = 1, nvars
    !debug: PRINT *,i,"calling nf90 get var"

    retcode = nf90_get_var(ncid, varid(i), allvars(:,:,i) )
    CALL check(retcode)
    PRINT *,i, MAXVAL(allvars(:,:,i)), MINVAL(allvars(:,:,i))
  ENDDO

  !debug: PRINT *,'leaving read',nx, ny, ncid
    
  RETURN
END

!----------------------------------------------------------------
!utility for netcdf
SUBROUTINE check(status)
  IMPLICIT none
  INTEGER, intent(in) :: status
  IF (status /= nf90_noerr) THEN
    PRINT *,nf90_strerror(status)
    STOP "erredout"
  ENDIF
  RETURN
END subroutine check


!----------------------------------------------------------------
SUBROUTINE initialize_out(fname, ncid, varid, nvar, nbuoy, dimids)
  IMPLICIT none
  CHARACTER(*), intent(in) :: fname
  INTEGER, intent(in) :: nvar
  INTEGER, intent(out) :: varid(nvar)
  INTEGER ncid, nbuoy

  CHARACTER(20) varnames(nvar)
  INTEGER, intent(inout) :: dimids(1)

  INTEGER i, retcode
  INTEGER x_dimid
!names: 
  varnames(1) = "Initial_Latitude"
  varnames(2) = "Initial_Longitude"
  varnames(3) = "Final_Latitude"
  varnames(4) = "Final_Longitude"
  varnames(5) = "Drift_Distance"
  varnames(6) = "Drift_Bearing"
!debug:  PRINT *,'assigned varnames'
  
! open
  retcode = nf90_create(fname, NF90_NOCLOBBER, ncid)
  CALL check(retcode)
!debug:  PRINT *,'back from check = ',retcode
  
! dimensionalize
  retcode = nf90_def_dim(ncid, "nbuoy", nbuoy, x_dimid)
  CALL check(retcode)
  dimids = (/ x_dimid /)
!debug:  PRINT *,'back from dimid'

! assign varid to varnames
  DO i = 1, nvar
!debug:    PRINT *,'assigning varname ',i,trim(varnames(i))
    retcode = nf90_def_var(ncid, trim(varnames(i)), NF90_REAL, dimids, varid(i))
    CALL check(retcode)
  ENDDO

  retcode = nf90_enddef(ncid)
  CALL check(retcode)


  RETURN
END subroutine initialize_out

!----------------------------------------------------------------
SUBROUTINE outvars(ncid, varid, nvar, buoys, nbuoy)
  IMPLICIT none

  INTEGER ncid, nvar
  INTEGER varid(nvar)
  INTEGER nbuoy
  TYPE(drifter) :: buoys(nbuoy)

  INTEGER retcode
  REAL, allocatable :: var(:,:)
  INTEGER i, k
  REAL distance, bear

!Note that netcdf dimensions are in C order, not fortran
  ALLOCATE(var(nbuoy, nvar))
  distance = 0.
  bear = 0.

  !debug: PRINT *,'entered outvars'

  DO k = 1, nbuoy
    var(k,1) = buoys(k)%ilat
    var(k,2) = buoys(k)%ilon
    var(k,3) = buoys(k)%clat
    var(k,4) = buoys(k)%clon
    CALL bearing(var(k,1), var(k,2), var(k,3), var(k,4), distance, bear)
    var(k,5) = distance
    var(k,6) = bear
  ENDDO

  !debug: PRINT *,'about to try to put vars'
  !RG: separate initial write -- just ilat, ilon, from later writes, clat, clon, distance, bear
  DO i = 1, nvar

    retcode = nf90_put_var(ncid, varid(i), var(:,i) )
    CALL check(retcode)
  ENDDO

  DEALLOCATE(var)

  !debug: PRINT *,'leaving outvar'

  RETURN
END subroutine outvars

!----------------------------------------------------------------
SUBROUTINE close_out(ncid)
  IMPLICIT none
  INTEGER ncid
  INTEGER retcode

  retcode = nf90_close(ncid)
  CALL check(retcode)

  RETURN
END subroutine close_out

!----------------------------------------------------------------
! Write out results -- drift distance and direction
SUBROUTINE writeout(ncid_out, varid_out, nvar_out, buoys, nbuoy, close)
  USE drifter_mod

  IMPLICIT none

  INTEGER, intent(in) ::  ncid_out, nvar_out, nbuoy
  INTEGER, intent(in) ::  varid_out(nvar_out)

  TYPE(drifter) :: buoys(nbuoy)
  LOGICAL, intent(in) :: close

  !debug: PRINT *,'calling outvars, buoy1%x = ',buoys(1)%x
  CALL outvars(ncid_out, varid_out, nvar_out, buoys, nbuoy )

  IF (close) THEN
    CALL close_out(ncid_out)
  ENDIF

END SUBROUTINE writeout

END module io
