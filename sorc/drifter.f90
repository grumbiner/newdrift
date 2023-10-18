! Module/class for drifting bodies
! expects a set of lat-lon locations and a velocity field with velocity 
!    grid type (B/C/...) and lat-longs of velocity points.
!    also time step to extrapolate over
MODULE drifter_mod

  IMPLICIT none
  TYPE, public :: drifter
    REAL x, y        ! current i,j location
    REAL ilat, ilon  ! initial latitude-longitude
    REAL clat, clon  ! current latitude-longitude
  CONTAINS
    PROCEDURE, pass :: move
  END TYPE drifter


CONTAINS
  SUBROUTINE move(buoy, u, v, dx, dy, dt, nx, ny)
!note dx, dy are the mesh variables
    IMPLICIT none

    CLASS(drifter), intent(inout) :: buoy
    INTEGER, intent(in) :: nx, ny
    REAL, intent(in) ::  u(nx, ny), v(nx, ny)
    REAL, intent(in) :: dx(nx, ny), dy(nx, ny)
    REAL, intent(in) :: dt

    REAL deltax, deltay
    INTEGER ti, tj

  !debug PRINT *,'in move',buoy%x, buoy%y, nx, ny

    ti = NINT(buoy%x)
    tj = NINT(buoy%y)
    deltax = u(ti, tj) * dt
    deltay = v(ti, tj) * dt
   
!where deltax < dx, deltax > -x-int(x):
    !IF (deltax < dx) THEN               !note, deltas could be negative
    buoy%x = buoy%x + deltax/dx(ti, tj)

!where deltay < dy:
    buoy%y = buoy%y + deltay/dy(ti, tj)

  RETURN
  END subroutine move

!----------------------------------------------------------------
SUBROUTINE run(buoys, nbuoy, u, v, dx, dy, nx, ny, dt, dtout)
  IMPLICIT none

  INTEGER, intent(in) :: nbuoy, nx, ny
  REAL, intent(in) :: u(nx, ny), v(nx, ny), dx(nx, ny), dy(nx, ny)
  REAL, intent(in) :: dt, dtout

  TYPE(drifter), intent(inout) ::  buoys(nbuoy)

  INTEGER k

  !debug: PRINT *,'entered run, nbuoy = ',nbuoy
  !debug: PRINT *,'nx ny = ',nx, ny
  !debug: PRINT *,'dt = ',dt
  !debug: PRINT *,'u ',MAXVAL(u), MINVAL(u)
  !debug: PRINT *,'v ',MAXVAL(v), MINVAL(v)
  !debug: PRINT *,'dx ',MAXVAL(dx), MINVAL(dx)
  !debug: PRINT *,'dy ',MAXVAL(dy), MINVAL(dy)

  DO k = 1, nbuoy
    !debug IF ( MOD(k,1000) .EQ. 0) THEN
      !debug PRINT *,'k = ',k, buoys(k)%x, buoys(k)%ilat, buoys(k)%clon
    !debug ENDIF

    !c-like 
    CALL buoys(k)%move(u, v, dx, dy, dt, nx, ny)

    !CALL move(buoys(k), u, v, dx, dy, dt, nx, ny)
  ENDDO
  !debug: PRINT *,'leaving run'

  ! output time level if requested

END SUBROUTINE run

END MODULE drifter_mod
