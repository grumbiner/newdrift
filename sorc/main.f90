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
  INTEGER dimids(2)
!
  INTEGER nx, ny
  PARAMETER (nx = 4500)
  PARAMETER (ny = 3297)
  
  REAL dt
  !PARAMETER (dt = 3600.) !seconds

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
  CHARACTER(90) fname, drift_name, outname


! Allocate space for variables and initialize the netcdf reading
  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  ALLOCATE(u(nx, ny), v(nx, ny))

! Forcing / velocities
  READ (*,*) fname
  !fname = "cice.nc"
  CALL initialize_in(fname, ncid, varid)
  !debug PRINT *,'varid = ',varid

  !Get first set of data and construct the local metric for drifting
  CALL read(nx, ny, nvar, ncid, varid, allvars)
  ulon = allvars(:,:,3)
  ulat = allvars(:,:,4)
  u    = allvars(:,:,9)
  v    = allvars(:,:,10)
  PRINT *,"ulat, ulon max = ",MAXVAL(ulat), MAXVAL(ulon)
  CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  PRINT *,"dx, dy max = ",MAXVAL(dx), MAXVAL(dy)

! Initialize Output -- need definite sizes
  READ (*,*) outname
  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)
  !RG: Need to work on/with multiple time step options
  CALL initialize_out(outname, ncid_out, varid_out, nvar_out, imax, jmax, dimids)

! Read in run parameters:
  READ (*,*) dt
  READ (*,*) nstep
  PRINT *,'nstep = ',nstep

  !----------- Initialize buoys, this should be a read in 
  drift_name = "drift_in.nc"
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
  !debug: PRINT *,1,j,buoys(1,j)%ilat ,  buoys(1,j)%ilon 
  ENDDO

!debug CALL outvars(ncid_out, varid_out, nvar_out, buoys, imax, jmax)
!debug CALL close_out(ncid_out)
!debug STOP "debug output"

!----------------------------------------------------------------
! sbr(buoys, u, v, dx, dy, dt, nx, ny, nstep, nvar, ncid, varid, allvars)

! First time step:
  DO j = 1, jmax
  DO i = 1, imax
    CALL buoys(i,j)%move(u, v, dx, dy, dt, nx, ny)
  ENDDO
  ENDDO
  !RG: write out at each time step
  PRINT *,'finished first step'

! Iterate
  DO n = 2, nstep
    !debug 
    PRINT *,'on step ',n
    !RG temporary debug: CALL read(nx, ny, nvar, ncid, varid, allvars)
    u = allvars(:,:,9)
    v = allvars(:,:,10)
    PRINT *,'set u,v '
    PRINT *,'imax, jmax = ',imax, jmax
    DO j = 1, jmax
    DO i = 1, imax
      CALL buoys(i,j)%move(u, v, dx, dy, dt, nx, ny)
    ENDDO
    ENDDO
    !RG: write out at each time step
  ENDDO !n


!----------------------------------------------------------------
! Write out results -- drift distance and direction
  CALL outvars(ncid_out, varid_out, nvar_out, buoys, imax, jmax)

  CALL close_out(ncid_out)

END program newdrift
