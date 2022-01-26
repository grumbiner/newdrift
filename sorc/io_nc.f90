SUBROUTINE initialize(fname, ncid, varid)
  USE netcdf

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

  retcode = nf90_open(fname, NF90_NOWRITE, ncid)
  CALL check(retcode)

  DO i = 1, nvar
    PRINT *,i,varnames(i)
    retcode = nf90_inq_varid(ncid, varnames(i), varid(i))
    CALL check(retcode)
  ENDDO

RETURN
END subroutine initialize 

SUBROUTINE first(ulat, ulon, dx, dy, rot, nx, ny)
  IMPLICIT none

  INTEGER, intent(in) :: nx, ny
  REAL, intent(in)    :: ulat(nx, ny), ulon(nx, ny)
  REAL, intent(out)   :: dx(nx, ny), dy(nx, ny), rot(nx, ny)
! need to find the dx/dy values to the next point (i+1, j+1)
! also rotation between grid orientation and geographic
  dx = ulat
  dy = ulon
  rot = 0

  RETURN
END subroutine first

SUBROUTINE read(nx, ny, nvars, ncid, varid, allvars)
  USE netcdf

  IMPLICIT none
  INTEGER, intent(in) :: nx, ny, nvars, ncid, varid(nvars)
  REAL, intent(out)   :: allvars(nx, ny, nvars)
  
  INTEGER i, retcode
  
  !debug PRINT *,'entered read'

  DO i = 1, nvars
    !debug PRINT *,i,"calling nf90 get var"
    retcode = nf90_get_var(ncid, varid(i), allvars(:,:,i) )
    CALL check(retcode)
    PRINT *,i, MAXVAL(allvars(:,:,i)), MINVAL(allvars(:,:,i))
  ENDDO
    
  RETURN
END
  

!utility for netcdf
SUBROUTINE check(status)
  USE netcdf
  IMPLICIT none
  INTEGER, intent(in) :: status
  IF (status /= nf90_noerr) THEN
    PRINT *,nf90_strerror(status)
    STOP "erredout"
  ENDIF
  RETURN
END subroutine check
