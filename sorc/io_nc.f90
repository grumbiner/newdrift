MODULE io
  USE netcdf
  USE drifter_mod
  PUBLIC

CONTAINS

SUBROUTINE initialize_in(fname, ncid, varid)
  IMPLICIT none

  CHARACTER(*) fname
  INTEGER ncid

  INTEGER nx, ny
! Dimensions from cice_inst output of rtofs
  PARAMETER (nx = 4500)
  PARAMETER (ny = 3297)

! Names from rtofs output
  INTEGER nvar
  PARAMETER (nvar = 24)
  CHARACTER(len=90) :: varnames(nvar)
  INTEGER varid(nvar)
  
! For Netcdf processing
  INTEGER i
  INTEGER retcode

  varnames(1) = "TLON"
  varnames(2) = "TLAT"
  varnames(3) = "ULON"
  varnames(4) = "ULAT"
  varnames(5) = "hi"
  varnames(6) = "hs"
  varnames(7) = "Tsfc"
  varnames(8) = "aice"
  varnames(9) = "uvel"
  varnames(10) = "vvel"
  varnames(11) = "fswdn"
  varnames(12) = "flwdn"
  varnames(13) = "snow"
  varnames(14) = "snow_ai"
  varnames(15) = "rain_ai"
  varnames(16) = "sst"
  varnames(17) = "uocn"
  varnames(18) = "vocn"
  varnames(19) = "meltt"
  varnames(20) = "meltb"
  varnames(21) = "meltl"
  varnames(22) = "strength"
  varnames(23) = "divu"
  varnames(24) = "Tair"
!Debug:
  PRINT *,"named all 24 variables"

  retcode = nf90_open(fname, NF90_NOWRITE, ncid)
  CALL check(retcode)
!Debug:
  PRINT *,"opened fname",fname

  DO i = 1, nvar
    PRINT *,i,varnames(i)
    retcode = nf90_inq_varid(ncid, varnames(i), varid(i))
    CALL check(retcode)
  ENDDO
!Debug:
  PRINT *,"done assigning varids"

RETURN
END subroutine initialize_in

SUBROUTINE read(nx, ny, nvars, ncid, varid, allvars)
  IMPLICIT none
  INTEGER, intent(in) :: nx, ny, nvars, ncid, varid(nvars)
  REAL, intent(out)   :: allvars(nx, ny, nvars)
  
  INTEGER i, retcode
  
  !debug PRINT *,'entered read'

  DO i = 1, nvars
    !debug 
    PRINT *,i,"calling nf90 get var"

    retcode = nf90_get_var(ncid, varid(i), allvars(:,:,i) )
    CALL check(retcode)
    PRINT *,i, MAXVAL(allvars(:,:,i)), MINVAL(allvars(:,:,i))
  ENDDO
  !debug:
  PRINT *,'leaving read'
    
  RETURN
END
  

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


SUBROUTINE initialize_out(fname, ncid, varid, nvar, nx, ny, dimids)
  IMPLICIT none
  CHARACTER(*) fname
  INTEGER ncid, nvar, nx, ny
  INTEGER varid(nvar)
  CHARACTER(90) varnames(nvar)
  INTEGER dimids(2)

  INTEGER i, retcode
  INTEGER x_dimid, y_dimid
!names: 
  varnames(1) = "Initial_Latitude"
  varnames(2) = "Initial_Longitude"
  varnames(3) = "Final_Latitude"
  varnames(4) = "Final_Longitude"
  varnames(5) = "Drift_Distance"
  varnames(6) = "Drift_Bearing"
  
! open
  retcode = nf90_create(fname, NF90_NOCLOBBER, ncid)
  CALL check(retcode)
! dimensionalize
  retcode = nf90_def_dim(ncid, "nx", nx, x_dimid)
  CALL check(retcode)
  retcode = nf90_def_dim(ncid, "ny", ny, y_dimid)
  CALL check(retcode)
  dimids = (/ y_dimid, x_dimid /)
  !debug PRINT *,'initialize dimids ',dimids, x_dimid, y_dimid

! assign varid to varnames
  DO i = 1, nvar
    retcode = nf90_def_var(ncid, varnames(i), NF90_REAL, dimids, varid(i))
    CALL check(retcode)
  ENDDO

  retcode = nf90_enddef(ncid)
  CALL check(retcode)


  RETURN
END subroutine initialize_out

SUBROUTINE outvars(ncid, varid, nvar, buoys, imax, jmax)
  IMPLICIT none
  INTEGER ncid, nvar
  INTEGER varid(nvar)
  INTEGER imax, jmax
  CLASS(drifter) :: buoys(imax, jmax)

  INTEGER retcode
  REAL, allocatable :: var(:,:,:)
  INTEGER i,j
  REAL distance, bear

!Note that netcdf dimensions are in C order, not fortran
  ALLOCATE(var(jmax, imax, nvar))
  distance = 0.
  bear = 0.

  DO j = 1, jmax
  DO i = 1, imax
    var(j,i,1) = buoys(i,j)%ilat
    var(j,i,2) = buoys(i,j)%ilon
    var(j,i,3) = buoys(i,j)%clat
    !debug PRINT *,'i,j,clat ',i,j,var(i,j,3)
    var(j,i,4) = buoys(i,j)%clon
    CALL bearing(var(j,i,1), var(j,i,2), var(j,i,3), var(j,i,4), distance, bear)
    var(j,i,5) = distance
    var(j,i,6) = bear
  ENDDO
  ENDDO

  !debug PRINT *,'about to try to put vars'
  DO i = 1, nvar
    !debug PRINT *,i, varid(i), MAXVAL(var(:,:,i)), MINVAL(var(:,:,i))
    retcode = nf90_put_var(ncid, varid(i), var(:,:,i) )
    !debug PRINT *,retcode
    CALL check(retcode)
  ENDDO

  RETURN
END subroutine outvars

SUBROUTINE close_out(ncid)
  IMPLICIT none
  INTEGER ncid
  INTEGER retcode

  retcode = nf90_close(ncid)
  CALL check(retcode)

  RETURN
END subroutine close_out

END module io
