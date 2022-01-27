PROGRAM newdrift
  !USE netcdf

  USE drifter_mod

  IMPLICIT none

  CHARACTER(90) fname

! Names from rtofs output
  INTEGER nvar
  PARAMETER (nvar = 24)
  INTEGER ncid, varid(nvar)
!
  INTEGER nx, ny
  PARAMETER (nx = 4500)
  PARAMETER (ny = 3297)
  
  REAL dt
  PARAMETER (dt = 3600.) !seconds

  REAL, allocatable :: allvars(:,:,:)
! nx*ny large enough to require -frecursive in gfortran
  REAL, allocatable  :: ulat(:,:), ulon(:,:)
  REAL, allocatable  :: dx(:,:), dy(:,:), rot(:,:)
  REAL, allocatable  :: u(:,:), v(:,:)

! Utilities for main
  INTEGER i, j, imax, jmax, ratio
  INTEGER n, nstep

!For drifter:
  CLASS(drifter), allocatable :: buoys(:,:)
  

! Allocate space for variables and initialize the netcdf reading
  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  ALLOCATE(u(nx, ny), v(nx, ny))

  fname = "cice.nc"
  CALL initialize_in(fname, ncid, varid)

  !----------- Initialize buoys, this should be a read in 
  ratio = 90
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)
  ALLOCATE(buoys(imax,jmax))
  DO j = 1, jmax
  DO i = 1, imax
    buoys(i,j)%x = i*ratio
    buoys(i,j)%y = j*ratio
  ENDDO
  ENDDO

!----------------------------------------------------------------

! First time step:
  CALL read(nx, ny, nvar, ncid, varid, allvars)
  CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  DO j = 1, jmax
  DO i = 1, imax
    CALL buoys(i,j)%move(dt, nx, ny)
  ENDDO
  ENDDO

! Iterate
  nstep = 1
  DO n = 2, nstep
    CALL read(nx, ny, nvar, ncid, varid, allvars)
    DO j = 1, jmax
    DO i = 1, imax
      CALL buoys(i,j)%move(dt, nx, ny)
    ENDDO
    ENDDO
  ENDDO

!----------------------------------------------------------------
! Write out results -- drift distance and direction



END program newdrift
