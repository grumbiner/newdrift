PROGRAM newdrift
  !USE netcdf

  USE drifter_mod
  USE io

  IMPLICIT none

! Netcdf-related
  INTEGER nvar, nvar_out, nvar_drift
  PARAMETER (nvar = 24)
  PARAMETER (nvar_out = 6)
  PARAMETER (nvar_drift = 2)
  INTEGER ncid, varid(nvar)
  INTEGER ncid_out, ncid_drift

  INTEGER varid_out(nvar_out), varid_driftic(nvar_drift)
  INTEGER dimids(1)
  
! Read from input (or argument to main)
  REAL dt, dtout

  REAL, allocatable :: allvars(:,:,:)

! nx*ny large enough to require -frecursive in gfortran if nx,ny specified here
  REAL, allocatable  :: ulat(:,:), ulon(:,:)
  REAL, allocatable  :: dx(:,:), dy(:,:), rot(:,:)
  REAL, allocatable  :: u(:,:), v(:,:)
  REAL, allocatable  :: aice(:,:)

! Utilities for main
  INTEGER i, j, k, imax, jmax, ratio
  INTEGER n, nstep

!For drifter 
  CLASS(drifter), allocatable :: buoys(:)
  INTEGER nbuoy

! Read from .nc file (or argument to main)
  INTEGER nx, ny
!
  CHARACTER(90) fname, drift_name, outname
  LOGICAL close

  !fname      = "cice.nc"
  !drift_name = "drift_in.nc"
  !outname    = "output.nc"
  close = .FALSE.

! Forcing / velocities
  READ (*,*) fname
  CALL initialize_in(nvar, fname, ncid, varid, nx, ny)

! Initialize Output -- need definite sizes
  READ (*,*) outname
  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))

  !debug: PRINT *,'calling initial_read'
  !RG: really initialize_io
  !Get first set of data and construct the local metric for drifting
  CALL initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                    allvars, ulon, ulat, dx, dy, rot, &
                    dimids, ncid_out, varid_out, nvar_out, nbuoy)
  !debug: STOP "done with initial_read"
  !debug: 
  PRINT *, "done with initial_read"

  u    = allvars(:,:,9)
  v    = allvars(:,:,10)
  PRINT *,"ulat, ulon max = ",MAXVAL(ulat), MAXVAL(ulon)
  !debug: CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  PRINT *,"dx, dy max = ",MAXVAL(dx), MAXVAL(dy)

  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)

  ALLOCATE(aice(nx, ny))
  aice = allvars(:,:,8)

! Read in run parameters:
  READ (*,*) dt
  READ (*,*) nstep
  PRINT *,'nstep = ',nstep

!RG: go to 1d list of points
  ALLOCATE(buoys(nbuoy))
  !Zero the buoys:
  DO k = 1, nbuoy
      buoys(k)%x = 0.0
      buoys(k)%y = 0.0
      buoys(k)%ilat = 0.0
      buoys(k)%ilon = 0.0
      buoys(k)%clat = 0.0
      buoys(k)%clon = 0.0
  ENDDO
! Dummy for testing
  k = 0
  DO j = 1, jmax
  DO i = 1, imax
    IF (aice(i*ratio, j*ratio) > 0. .AND. aice(i*ratio,j*ratio) <= 1.0) THEN
      k = k + 1
      buoys(k)%x = i*ratio
      buoys(k)%y = j*ratio
      buoys(k)%ilat = ulat(i*ratio, j*ratio)
      buoys(k)%ilon = ulon(i*ratio, j*ratio)
      buoys(k)%clat = i*ratio
      buoys(k)%clon = j*ratio
    ENDIF
  ENDDO
  !debug: PRINT *,1,j,buoys(1,j)%ilat ,  buoys(1,j)%ilon 
  ENDDO
  !debug: 
  PRINT *,'done in main assigning buoys, nactual = ', k

!debug DO k = 1, nbuoy
  !debug PRINT *,k,buoys(k)%x, buoys(k)%y,  buoys(k)%ilat, buoys(k)%ilon, buoys(k)%clat, buoys(k)%clon
!debug ENDDO
!debug STOP 'debug halt'

!debug CALL outvars(ncid_out, varid_out, nvar_out, buoys, imax, jmax)
!debug CALL close_out(ncid_out)
!debug STOP "debug output"

!----------------------------------------------------------------
! RUN
! run(buoys, u, v, dx, dy, dt, nx, ny, nstep, nvar, ncid, varid, allvars)

! First time step:

  u = allvars(:,:,9)
  v = allvars(:,:,10)
  !debug: PRINT *,'u ',MAXVAL(u), MINVAL(u)
  !debug: PRINT *,'v ',MAXVAL(v), MINVAL(v)
  !debug: PRINT *,'buoys(1)%x',buoys(1)%x
  !debug: 
  PRINT *,'calling run'
  CALL run(buoys, nbuoy, u, v, dx, dy, nx, ny, dt, dtout)

! Iterate
! -- one file with many time steps, many files 1 time step each
  nstep = 1
  DO n = 2, nstep

    CALL read(nx, ny, nvar, ncid, varid, allvars)

    u = allvars(:,:,9)
    v = allvars(:,:,10)
    CALL run(buoys, nbuoy, u, v, dx, dy, nx, ny, dt, dtout)
  ENDDO

!----------------------------------------------------------------
! WRITE Write out results -- drift distance and direction
  close = .TRUE.
  CALL writeout(ncid_out, varid_out, nvar_out, buoys, nbuoy, close)
  !CALL outvars(ncid_out, varid_out, nvar_out, buoys, imax, jmax)
  !CALL close_out(ncid_out)

END program newdrift
