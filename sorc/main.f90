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
  INTEGER nbuoy, nactual

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
!debug: PRINT *,fname, outname

! Read in run parameters:
  READ (10,*) dt
  READ (10,*) nstep
  READ (10,*) outfreq
  PRINT *,'dt, nstep, outfreq = ',dt, nstep, outfreq

! Forcing / velocities
  CALL initialize_in(nvar, fname, ncid, varid, nx, ny)
!debug:  STOP

! Initialize Output -- need definite sizes
  ALLOCATE(allvars(nx, ny, nvar))
  ALLOCATE(ulat(nx, ny), ulon(nx, ny), dx(nx, ny), dy(nx, ny), rot(nx, ny))
  ALLOCATE(aice(nx, ny))

  !RG: really initialize_io
  !Get first set of data and construct the local metric for drifting
  CALL initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                    allvars, ulon, ulat, dx, dy, rot, &
                    dimids, ncid_out, varid_out, nvar_out, nbuoy)
  !debug: PRINT *, "done with initial_read"
  !debug: 
  PRINT *,"local metric dx, dy max = ",MAXVAL(dx), MAXVAL(dy)
  !debug: 
  PRINT *,"local metric dx, dy min = ",MINVAL(dx), MINVAL(dy)
  !debug: 
  PRINT *,"ulat, ulon max = ",MAXVAL(ulat), MAXVAL(ulon)
  !debug: 
  PRINT *,"ulat, ulon min = ",MINVAL(ulat), MINVAL(ulon)


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
  !RG: Should com from initial read, reading in buoy file
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
  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)
  x = 0.0
  y = 0.0
  DO j = 1, jmax
  DO i = 1, imax
    IF (aice(i*ratio, j*ratio) > 0. .AND. aice(i*ratio,j*ratio) <= 1.0  .AND. &
        dx(i*ratio, j*ratio) .NE. 0. .AND. dy(i*ratio, j*ratio) .NE. 0. .AND. &
        ABS(u(i*ratio, j*ratio)) < 100. .AND. ABS(v(i*ratio, j*ratio)) < 100. ) THEN
      !debug: PRINT *,'aice, dx, dy, u, v',aice(i*ratio, j*ratio), dx(i*ratio, j*ratio), &
      !debug:   dy(i*ratio, j*ratio), u(i*ratio, j*ratio), v(i*ratio, j*ratio)

      k = k + 1
      buoys(k)%ilat = ulat(i*ratio, j*ratio)
      buoys(k)%ilon = ulon(i*ratio, j*ratio)
      buoys(k)%clat = ulat(i*ratio, j*ratio)
      buoys(k)%clon = ulon(i*ratio, j*ratio)

      !RG: Must compute physical value here, location vs. grid mesh function
      !CALL ll_to_xy(buoys(k)%ilat, buoys(k)%ilon, ulat, ulon, x, y, nx, ny)
      buoys(k)%x = i*ratio
      buoys(k)%y = j*ratio

    ENDIF
  ENDDO
  !debug: PRINT *,1,j,buoys(1,j)%ilat ,  buoys(1,j)%ilon 
  ENDDO
  !debug: 
  PRINT *,'done in main assigning buoys, nactual = ', k
  nactual = k

!----------------------------------------------------------------
! RUN
! run(buoys, u, v, dx, dy, dt, nx, ny, nstep, nvar, ncid, varid, allvars)

! First time step:

  u = allvars(:,:,6)
  v = allvars(:,:,7)
  !DEALLOCATE(allvars)
  !debug: 
  PRINT *,'calling run'
  CALL run(buoys, nactual, u, v, dx, dy, nx, ny, dt, dtout)

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
    CALL run(buoys, nactual, u, v, dx, dy, nx, ny, dt, dtout)
  ENDDO

!----------------------------------------------------------------
! WRITE Write out results -- drift distance and direction
  close = .TRUE.
  CALL writeout(ncid_out, varid_out, nvar_out, buoys, nactual, close)

END program newdrift
