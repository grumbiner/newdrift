SUBROUTINE initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                        allvars, ulon, ulat, dx, dy, rot, &
                        outdimids, ncid_out, varid_out, nvar_out, imax, jmax)

  USE drifter_mod
  USE io
  IMPLICIT none

  INTEGER, intent(in) :: nx, ny
  CHARACTER(90), intent(in) :: fname, drift_name, outname
  INTEGER, intent(inout) :: nvar, ncid, varid(nvar)
  INTEGER, intent(out) :: outdimids(2)
  INTEGER nvar_out, imax, jmax
  INTEGER ncid_out, ncid_drift
  INTEGER varid_out(nvar_out), varid_driftic(nvar_out)
  
  REAL, intent(inout) :: allvars(nx, ny, nvar)
  REAL, intent(inout) :: ulat(nx, ny), ulon(nx, ny)
  REAL, intent(out)   :: dx(nx, ny), dy(nx, ny), rot(nx, ny)

! make argument
  CLASS(drifter), allocatable :: buoys(:,:)
  ! 1d: CLASS(drifter), allocatable :: buoys(:)

! Locals:
  INTEGER i, j, ratio

! Allocate space for variables and initialize the netcdf reading

! Forcing / velocities
  !Get first set of data and construct the local metric for drifting
  PRINT *,' calling read ',nx, ny, nvar, ncid
  PRINT *,' varid = ',varid
  !PRINT *,'in initial_read allocated(allvars) ',allocated(allvars) 
  !PRINT *,'initial_read: dx, dy, rot ',allocated(dx), allocated(dy), allocated(rot)
  !PRINT *,'initial_read: ulat, ulon ',allocated(ulat), allocated(ulon)
  !IF (.not. allocated(allvars) ) STOP 'allvars allocation was forgotten'

  CALL read(nx, ny, nvar, ncid, varid, allvars)
  PRINT *,'done with first read'

  ulat = allvars(:,:,4)
  ulon = allvars(:,:,3)
  PRINT *,'about to call metric', MAXVAL(ulat), MAXVAL(ulon)
  CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  PRINT *,'computed the local metric'

  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)

!  !----------- Initialize buoys, this should be a read in 
!  CALL initialize_drifter(drift_name, ncid_drift, varid_driftin, nvar_out, buoys)
!  CALL close_out(ncid_drift)
!RG: go to 1d list of points
  ALLOCATE(buoys(imax,jmax))
  PRINT *,'allocated the buoys'
  DO j = 1, jmax
  DO i = 1, imax
    buoys(i,j)%x = i*ratio
    buoys(i,j)%y = j*ratio
    buoys(i,j)%ilat = ulat(i*ratio, j*ratio)
    buoys(i,j)%ilon = ulon(i*ratio, j*ratio)
    buoys(i,j)%clat = i*ratio
    buoys(i,j)%clon = j*ratio
  ENDDO
  ENDDO

! Initialize Output -- need definite sizes
  CALL initialize_out(outname, ncid_out, varid_out, nvar_out, imax, jmax, outdimids)
  PRINT *,'initialized the output'

END SUBROUTINE initial_read
