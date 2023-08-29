PROGRAM newdrift
  !USE netcdf

  USE drifter_mod
  USE io

  IMPLICIT none

! Netcdf-related
  INTEGER nvar, nvar_out
  PARAMETER (nvar = 24)
  PARAMETER (nvar_out = 6)
  INTEGER ncid, varid(nvar)
  INTEGER ncid_out, ncid_drift
  INTEGER varid_out(nvar_out), varid_driftic(nvar_out)
  INTEGER dimids(1)
!
  
! Read from input (or argument to main)
  REAL dt, dtout
  PARAMETER (dt = 3600.) !seconds
  PARAMETER (dtout = dt*24) !seconds

  REAL, allocatable :: allvars(:,:,:)

! nx*ny large enough to require -frecursive in gfortran if nx,ny specified here
  REAL, allocatable  :: ulat(:,:), ulon(:,:)
  REAL, allocatable  :: dx(:,:), dy(:,:), rot(:,:)
  REAL, allocatable  :: u(:,:), v(:,:)

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

  fname      = "cice.nc"
  drift_name = "drift_in.nc"
  outname    = "output.nc"
  close = .FALSE.

  CALL initialize_in(nvar, fname, ncid, varid, nx, ny)
  ALLOCATE(allvars(nx, ny, nvar))

  !debug: PRINT *,'calling initial_read'
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  CALL initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                    allvars, ulon, ulat, dx, dy, rot, &
                    dimids, ncid_out, varid_out, nvar_out, nbuoy)
  !debug: STOP "done with initial_read"


!----------- Initialize buoys, this should be a read in 
! Initialize Output -- need definite size in nbuoy
  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)
!  CALL initialize_drifter(drift_name, ncid_drift, varid_driftin, nvar_out, buoys)
!  CALL close_out(ncid_drift)
!RG: go to 1d list of points
  ALLOCATE(buoys(nbuoy))
! Dummy for testing
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
  !debug: PRINT *,'done in main assigning buoys'

!debug DO k = 1, nbuoy
  !debug PRINT *,k,buoys(k)%x, buoys(k)%y,  buoys(k)%ilat, buoys(k)%ilon, buoys(k)%clat, buoys(k)%clon
!debug ENDDO
!debug STOP 'debug halt'

!----------------------------------------------------------------
! RUN
! run(buoys, u, v, dx, dy, dt, nx, ny, nstep, nvar, ncid, varid, allvars)

! First time step:
  u = allvars(:,:,9)
  v = allvars(:,:,10)
  !debug: PRINT *,'u ',MAXVAL(u), MINVAL(u)
  !debug: PRINT *,'v ',MAXVAL(v), MINVAL(v)
  !debug: PRINT *,'buoys(1)%x',buoys(1)%x
  !debug: PRINT *,'calling run'
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
