SUBROUTINE initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                        allvars, ulon, ulat, dx, dy, rot, &
                        outdimids, ncid_out, varid_out, nvar_out, nbuoy)

  USE drifter_mod
  USE io
  IMPLICIT none

  INTEGER, intent(in) :: nx, ny
  CHARACTER(90), intent(in) :: fname, drift_name, outname
  INTEGER, intent(inout) :: nvar, ncid, varid(nvar)
  INTEGER, intent(out) :: outdimids(1)
  INTEGER nvar_out, nbuoy
  INTEGER ncid_out, ncid_drift
  INTEGER varid_out(nvar_out), varid_driftic(nvar_out)
  
  REAL, intent(inout) :: allvars(nx, ny, nvar)
  REAL, intent(inout) :: ulat(nx, ny), ulon(nx, ny)
  REAL, intent(out)   :: dx(nx, ny), dy(nx, ny), rot(nx, ny)

! make argument
  TYPE(drifter), allocatable :: buoys(:)

! Locals:
  INTEGER i, j, k, ratio
  INTEGER imax, jmax

! Allocate space for variables and initialize the netcdf reading

! Forcing / velocities
  !Get first set of data and construct the local metric for drifting
  !debug: PRINT *,' calling read ',nx, ny, nvar, ncid
  CALL read(nx, ny, nvar, ncid, varid, allvars)
  !debug: PRINT *,'done with first read'

  ulat = allvars(:,:,4)
  ulon = allvars(:,:,3)
  !debug: PRINT *,'about to call metric', MAXVAL(ulat), MAXVAL(ulon)
  CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  !debug: PRINT *,'computed the local metric'


!  !----------- Initialize buoys, this should be a read in 
!  CALL initialize_drifter(drift_name, ncid_drift, varid_driftin, nvar_out, buoys)
!  CALL close_out(ncid_drift)

  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)
!RG: go to 1d list of points
  ALLOCATE(buoys(imax*jmax))
  !debug: PRINT *,'allocated the buoys'
  k = 0
  DO j = 1, jmax
  DO i = 1, imax
    k = k + 1
    buoys(k)%x = i*ratio
    buoys(k)%y = j*ratio
    buoys(k)%ilat = ulat(i*ratio, j*ratio)
    buoys(k)%ilon = ulon(i*ratio, j*ratio)
    buoys(k)%clat = i*ratio
    buoys(k)%clon = j*ratio
  ENDDO
  ENDDO
  nbuoy = k

! Initialize Output -- need definite sizes
  CALL initialize_out(outname, ncid_out, varid_out, nvar_out, nbuoy, outdimids)
  !debug: PRINT *,'initialized the output'

END SUBROUTINE initial_read
