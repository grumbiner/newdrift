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
  INTEGER dimids(2)
!
  
! Read from input (or argument to main)
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

!For drifter -- make 1d
  CLASS(drifter), allocatable :: buoys(:,:)

! Read from .nc file (or argument to main)
  INTEGER nx, ny
!
  CHARACTER(90) fname, drift_name, outname

  fname      = "cice.nc"
  drift_name = "drift_in.nc"
  outname    = "output.nc"

  CALL initialize_in(nvar, fname, ncid, varid, nx, ny)
  ALLOCATE(allvars(nx, ny, nvar))

  !CALL read(nx, ny, nvar, ncid, varid, allvars)
  PRINT *,'calling initial_read'
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  PRINT *,'main allvars ',allocated(allvars)
  PRINT *,'main dx, dy, rot ',allocated(dx), allocated(dy), allocated(rot)
  PRINT *,'main ulat, ulon ',allocated(ulat), allocated(ulon)
  CALL initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                    allvars, ulon, ulat, dx, dy, rot, &
                    dimids, ncid_out, varid_out, nvar_out, imax, jmax)
  !debug: 
  STOP "done with initial_read"


! Forcing / velocities

! Initialize Output -- need definite size in nbuoy
  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)
  !CALL initialize_out(outname, ncid_out, varid_out, nvar_out, imax, jmax, dimids)

  !----------- Initialize buoys, this should be a read in 
!  CALL initialize_drifter(drift_name, ncid_drift, varid_driftin, nvar_out, buoys)
!  CALL close_out(ncid_drift)
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
! RUN
! run(buoys, u, v, dx, dy, dt, nx, ny, nstep, nvar, ncid, varid, allvars)

! First time step:
  u = allvars(:,:,9)
  v = allvars(:,:,10)
  DO j = 1, jmax
  DO i = 1, imax
    CALL buoys(i,j)%move(u, v, dx, dy, dt, nx, ny)
  ENDDO
  ENDDO

! Iterate
  nstep = 1
  DO n = 2, nstep
    CALL read(nx, ny, nvar, ncid, varid, allvars)
    u = allvars(:,:,9)
    v = allvars(:,:,10)
    DO j = 1, jmax
    DO i = 1, imax
      CALL buoys(i,j)%move(u, v, dx, dy, dt, nx, ny)
    ENDDO
    ENDDO
  ENDDO

!----------------------------------------------------------------
! WRITE Write out results -- drift distance and direction
  CALL outvars(ncid_out, varid_out, nvar_out, buoys, imax, jmax)
  CALL close_out(ncid_out)

END program newdrift
