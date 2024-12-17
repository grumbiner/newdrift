PROGRAM newdrift
  !USE netcdf

  USE drifter_mod
  USE io

  IMPLICIT none

! Netcdf-related
  INTEGER nvar, nvar_out, nvar_drift
  !cice_inst: PARAMETER (nvar = 24)
  PARAMETER (nvar = 7)
  PARAMETER (nvar_out = 6)
  PARAMETER (nvar_drift = 2)
  INTEGER ncid, varid(nvar)
  INTEGER ncid_out, ncid_drift

  INTEGER varid_out(nvar_out), varid_driftic(nvar_drift)
  INTEGER dimids(1)
  
! Read from input (or argument to main)
  REAL dt, dtout
  INTEGER outfreq

  REAL, allocatable :: allvars(:,:,:)

! nx*ny large enough to require -frecursive in gfortran if nx,ny specified here
  REAL, allocatable  :: ulat(:,:), ulon(:,:)
  REAL, allocatable  :: dx(:,:), dy(:,:), rot(:,:)
  REAL, allocatable  :: u(:,:), v(:,:)
  REAL, allocatable  :: aice(:,:)

! Utilities for main
  INTEGER i, j, k, imax, jmax, ratio
  INTEGER n, nstep
  REAL x, y

!For drifter 
  CLASS(drifter), allocatable :: buoys(:)
  INTEGER nbuoys, nactual

! Read from .nc file (or argument to main)
  INTEGER nx, ny
! Names
  CHARACTER(90) fname, drift_name, outname, tmp, tmp2
  LOGICAL close


! -- Begin main for offline ----
  READ (*,*) tmp
  OPEN(10, FILE=tmp, FORM='FORMATTED', STATUS='OLD')

  READ (10,*) tmp2
  fname = trim(tmp2)
  READ (10,*) tmp
  outname = trim(tmp)

! Read in run parameters:
  READ (10,*) dt
  READ (10,*) nstep
  READ (10,*) outfreq
  PRINT *,'dt, nstep, outfreq = ',dt, nstep, outfreq

! Forcing / velocities
  CALL initialize_in(nvar, fname, ncid, varid, nx, ny)

! Initialize Output -- need definite sizes
  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  ALLOCATE(aice(nx, ny))

  !RG: really initialize_io
  !Get first set of data and construct the local metric for drifting
  CALL initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                    allvars, ulon, ulat, dx, dy, rot, &
                    dimids, ncid_out, varid_out, nvar_out, nbuoys)


!cice_inst:
!  aice = allvars(:,:,8)
!2ds_ice:
  aice = allvars(:,:,3)

!cice_inst:
!  u    = allvars(:,:,9)
!  v    = allvars(:,:,10)
!2ds_ice
  u    = allvars(:,:,6)
  v    = allvars(:,:,7)

  !---------------------------------------------------------
!  !RG: Should com from initial read, reading in buoy file
  nbuoys = (nx/5)*(ny/5)
  ALLOCATE(buoys(nbuoys))
  PRINT *,'allocated the ',nbuoys,' buoys'

  CALL zero_buoys(buoys, nbuoys)
  PRINT *,'back from zero_buoys'

! Dummy for testing
  PRINT *,'calling dummy buoys'
  CALL dummy_buoys(aice, dx, dy, u, v, ulat, ulon, nx, ny, buoys, nbuoys)
  PRINT *,'returned from calling dummy buoys'

  nactual = nbuoys

!----------------------------------------------------------------
! RUN
! run(buoys, u, v, dx, dy, dt, nx, ny, nstep, nvar, ncid, varid, allvars)

! First time step:

  u = allvars(:,:,6)
  v = allvars(:,:,7)
  !DEALLOCATE(allvars)
  CALL run(buoys, nactual, u, v, dx, dy, nx, ny, dt, dtout)

! Iterate
! -- one file with many time steps, many files 1 time step each
  !debug: nstep = 1
  !initial read has taken care of this: CALL read(nx, ny, nvar, ncid, varid, allvars)
  DO n = 2, nstep

    !CALL read(nx, ny, nvar, ncid, varid, allvars)
    CALL run(buoys, nactual, u, v, dx, dy, nx, ny, dt, dtout)
  ENDDO

!----------------------------------------------------------------
! WRITE Write out results -- drift distance and direction
  close = .TRUE.
  CALL writeout(ncid_out, varid_out, nvar_out, buoys, nactual, close)

END program newdrift
