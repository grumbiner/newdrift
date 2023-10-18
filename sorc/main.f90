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
! Names
  CHARACTER(90) fname, drift_name, outname
  LOGICAL close


! -- Begin main for offline ----

! Forcing / velocities
  READ (*,*) fname
  CALL initialize_in(nvar, fname, ncid, varid, nx, ny)

! Initialize Output -- need definite sizes
  READ (*,*) outname
  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))

  !RG: really initialize_io
  !Get first set of data and construct the local metric for drifting
  CALL initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                    allvars, ulon, ulat, dx, dy, rot, &
                    dimids, ncid_out, varid_out, nvar_out, nbuoy)
  !debug: PRINT *, "done with initial_read"

  u    = allvars(:,:,9)
  v    = allvars(:,:,10)
  !debug: PRINT *,"ulat, ulon max = ",MAXVAL(ulat), MAXVAL(ulon)
  !debug: PRINT *,"local metric dx, dy max = ",MAXVAL(dx), MAXVAL(dy)
  !debug: 
  PRINT *,"local metric dx, dy min = ",MINVAL(dx), MINVAL(dy)

  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)

  ALLOCATE(aice(nx, ny))
  aice = allvars(:,:,8)

! Read in run parameters:
  READ (*,*) dt
  READ (*,*) nstep
  PRINT *,'dt, nstep = ',nstep

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
    IF (aice(i*ratio, j*ratio) > 0. .AND. aice(i*ratio,j*ratio) <= 1.0  .AND. &
        dx(i*ratio, j*ratio) .NE. 0. .AND. dy(i*ratio, j*ratio) .NE. 0. .AND. &
        ABS(u(i*ratio, j*ratio)) < 100. .AND. ABS(v(i*ratio, j*ratio)) < 100. ) THEN
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

!----------------------------------------------------------------
! RUN
! run(buoys, u, v, dx, dy, dt, nx, ny, nstep, nvar, ncid, varid, allvars)

! First time step:

  u = allvars(:,:,9)
  v = allvars(:,:,10)
  !DEALLOCATE(allvars)
  !debug: 
  PRINT *,'calling run'
  CALL run(buoys, nbuoy, u, v, dx, dy, nx, ny, dt, dtout)

! Iterate
! -- one file with many time steps, many files 1 time step each
  !debug: nstep = 1
  !initial read has taken care of this: CALL read(nx, ny, nvar, ncid, varid, allvars)
  DO n = 2, nstep
    !debug:
    PRINT *,'n = ', n

    !CALL read(nx, ny, nvar, ncid, varid, allvars)
    !u = allvars(:,:,9)
    !v = allvars(:,:,10)
    CALL run(buoys, nbuoy, u, v, dx, dy, nx, ny, dt, dtout)
  ENDDO

!----------------------------------------------------------------
! WRITE Write out results -- drift distance and direction
  close = .TRUE.
  CALL writeout(ncid_out, varid_out, nvar_out, buoys, nbuoy, close)

END program newdrift
