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
  INTEGER ncid_out, varid_out(nvar_out)
  INTEGER dimids(2)
!
  INTEGER nx, ny
  PARAMETER (nx = 4500)
  PARAMETER (ny = 3297)
  
  REAL dt
  PARAMETER (dt = 3600.) !seconds

  REAL, allocatable :: allvars(:,:,:)
! nx*ny large enough to require -frecursive in gfortran if nx,ny specified here
  REAL, allocatable  :: ulat(:,:), ulon(:,:)
  REAL, allocatable  :: dx(:,:), dy(:,:), rot(:,:)
  REAL, allocatable  :: u(:,:), v(:,:)

! Utilities for main
  INTEGER i, j, imax, jmax, ratio
  INTEGER n, nstep

!For drifter:
  CLASS(drifter), allocatable :: buoys(:,:)
!
  CHARACTER(90) fname

  

! Allocate space for variables and initialize the netcdf reading
  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  ALLOCATE(u(nx, ny), v(nx, ny))

  fname = "cice.nc"
  CALL initialize_in(fname, ncid, varid)

  !----------- Initialize buoys, this should be a read in 
  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)
!RG: go to 1d list of points
  ALLOCATE(buoys(imax,jmax))
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

!----------------------------------------------------------------

! First time step:
  CALL read(nx, ny, nvar, ncid, varid, allvars)
  CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  DO j = 1, jmax
  DO i = 1, imax
    CALL buoys(i,j)%move(u, v, dx, dy, dt, nx, ny)
  ENDDO
  ENDDO

! Iterate
  nstep = 1
  DO n = 2, nstep
    CALL read(nx, ny, nvar, ncid, varid, allvars)
    DO j = 1, jmax
    DO i = 1, imax
      CALL buoys(i,j)%move(u, v, dx, dy, dt, nx, ny)
    ENDDO
    ENDDO
  ENDDO

!----------------------------------------------------------------
! Write out results -- drift distance and direction
  fname = "output.nc"
  CALL initialize_out(fname, ncid_out, varid_out, nvar_out, imax, jmax, dimids)

  CALL outvars(ncid_out, varid_out, nvar_out, buoys, imax, jmax)

  CALL close_out(ncid_out)

END program newdrift
