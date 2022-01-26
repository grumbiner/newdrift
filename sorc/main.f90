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
  INTEGER i, j, imax, jmax

!For drifter:
  CLASS(drifter), allocatable :: buoys(:,:)
  

  fname = "cice.nc"
  CALL initialize(fname, ncid, varid)

  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  ALLOCATE(u(nx, ny), v(nx, ny))

  CALL read(nx, ny, nvar, ncid, varid, allvars)
  CALL first(ulat, ulon, dx, dy, rot, nx, ny)

  imax = INT(nx/50)
  jmax = INT(ny/50)
  ALLOCATE(buoys(imax,jmax))
  DO j = 1, jmax
  DO i = 1, imax
    buoys(i,j)%x = i*50.
    buoys(i,j)%y = j*50.
  ENDDO
  ENDDO

  DO j = 1, jmax
  DO i = 1, imax
    !CALL buoys(i,j)%move(buoys(i,j), u, v, dx, dy, dt, nx, ny)

    CALL buoys(i,j)%move(dt, nx, ny)
  ENDDO
  ENDDO


END program newdrift
