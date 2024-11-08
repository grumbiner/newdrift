SUBROUTINE initial_read(fname, drift_name, outname, nx, ny, nvar, ncid, varid, &
                        allvars, ulon, ulat, dx, dy, rot, &
                        outdimids, ncid_out, varid_out, nvar_out, nbuoy)

  USE drifter_mod
  USE io
  IMPLICIT none

  INTEGER, intent(in) :: nx, ny
  CHARACTER(90), intent(in) :: fname, drift_name, outname
  INTEGER, intent(inout) :: nvar, ncid, varid(nvar)
  INTEGER, intent(out) :: outdimids(1)
  INTEGER nvar_out, nbuoy
  INTEGER ncid_out, ncid_drift
  INTEGER varid_out(nvar_out), varid_driftic(nvar_out)
  
  REAL, intent(inout) :: allvars(nx, ny, nvar)
  REAL, intent(inout) :: ulat(nx, ny), ulon(nx, ny)
  REAL, intent(out)   :: dx(nx, ny), dy(nx, ny), rot(nx, ny)

! Locals:
  INTEGER i, j, k

! Allocate space for variables and initialize the netcdf reading

! Forcing / velocities
  !Get first set of data and construct the local metric for drifting
  CALL read(nx, ny, nvar, ncid, varid, allvars)

  ulat = allvars(:,:,4)
  ulon = allvars(:,:,3)
  CALL local_metric(ulat, ulon, dx, dy, rot, nx, ny)

!  !----------- Initialize buoys, this should be a read in 
!  CALL initialize_drifter(drift_name, ncid_drift, varid_driftin, nvar_out, buoys)
!  CALL close_out(ncid_drift)

! Initialize Output -- need definite sizes
  PRINT *,'about to initialize_out'
  nbuoy = 84700
  CALL initialize_out(outname, ncid_out, varid_out, nvar_out, nbuoy, outdimids)
  PRINT *,'initialized the output'

END SUBROUTINE initial_read
