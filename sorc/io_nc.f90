MODULE io
  USE netcdf
  USE drifter_mod
  PUBLIC

CONTAINS

SUBROUTINE initialize_in(nvar, fname, ncid, varid, nx, ny)
  IMPLICIT none

  CHARACTER(*) fname
  INTEGER ncid, nvar
  INTEGER, intent(out) :: nx, ny

! Names from rtofs output
  CHARACTER(len=40) :: varnames(nvar)
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

  retcode = nf90_open(fname, NF90_NOWRITE, ncid)
  CALL check(retcode)

  DO i = 1, nvar
    !debug: PRINT *,i,varnames(i)
    retcode = nf90_inq_varid(ncid, varnames(i), varid(i))
    CALL check(retcode)
  ENDDO
  nx = 4500
  ny = 3297

  !debug: PRINT *,'leaving initialize_in'

RETURN
END subroutine initialize_in

SUBROUTINE read(nx, ny, nvars, ncid, varid, allvars)
  IMPLICIT none
  INTEGER, intent(in)    :: nvars
  INTEGER, intent(in)    :: nx, ny, ncid, varid(nvars)
  REAL, intent(inout)    :: allvars(nx, ny, nvars)
  
  INTEGER i, retcode
  
  !debug: PRINT *,'entered read',nx, ny, ncid
  !got nx, ny from the .nc file, in initialize_in

  DO i = 1, nvars
    !debug PRINT *,i,"calling nf90 get var"
    retcode = nf90_get_var(ncid, varid(i), allvars(:,:,i) )
    CALL check(retcode)
    PRINT *,i, MAXVAL(allvars(:,:,i)), MINVAL(allvars(:,:,i))
  ENDDO

  !debug: PRINT *,'leaving read',nx, ny, ncid
    
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


SUBROUTINE initialize_out(fname, ncid, varid, nvar, nbuoy, dimids)
  IMPLICIT none
  CHARACTER(*) fname
  INTEGER ncid, nvar, nbuoy
  INTEGER varid(nvar)
  CHARACTER(90) varnames(nvar)
  INTEGER dimids(1)

  INTEGER i, retcode
  INTEGER x_dimid
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
  retcode = nf90_def_dim(ncid, "nbuoy", nbuoy, x_dimid)
  CALL check(retcode)
  dimids = (/ x_dimid /)

! assign varid to varnames
  DO i = 1, nvar
    retcode = nf90_def_var(ncid, varnames(i), NF90_REAL, dimids, varid(i))
    CALL check(retcode)
  ENDDO

  retcode = nf90_enddef(ncid)
  CALL check(retcode)


  RETURN
END subroutine initialize_out

SUBROUTINE outvars(ncid, varid, nvar, buoys, nbuoy)
  IMPLICIT none
  INTEGER ncid, nvar
  INTEGER varid(nvar)
  INTEGER nbuoy
  TYPE(drifter) :: buoys(nbuoy)

  INTEGER retcode
  REAL, allocatable :: var(:,:)
  INTEGER i,j, k
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

  !debug PRINT *,'about to try to put vars'
  DO i = 1, nvar
    retcode = nf90_put_var(ncid, varid(i), var(:,i) )
    CALL check(retcode)
  ENDDO

  DEALLOCATE(var)
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
