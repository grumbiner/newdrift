! Module/class for drifting bodies
! expects a set of lat-lon locations and a velocity field with velocity 
!    grid type (B/C/...) and lat-longs of velocity points.
!    also time step to extrapolate over
MODULE drifter_mod

  IMPLICIT none
  TYPE, public :: drifter
    REAL x, y
    REAL lat, lon
  CONTAINS
    PROCEDURE, pass :: move
  END TYPE drifter


CONTAINS
  !SUBROUTINE move(buoy, u, v, dx, dy, dt, nx, ny)
  !SUBROUTINE move(buoy, dx, dy, dt, nx, ny)
  SUBROUTINE move(buoy, dt, nx, ny)
!note dx, dy are the mesh variables
    IMPLICIT none

    CLASS(drifter), intent(inout) :: buoy
    INTEGER, intent(in) :: nx, ny
!    REAL, intent(in) ::  u(nx, ny), v(nx, ny)
!    REAL, intent(in) :: dx(nx, ny), dy(nx, ny)
    REAL, intent(in) :: dt

    REAL deltax, deltay
    INTEGER ti, tj

  PRINT *,buoy%x, buoy%y, nx, ny

! where aice != 0
    ti = NINT(buoy%x)
    tj = NINT(buoy%y)
    !deltax = u(ti, tj)
    !deltay = v(ti, tj)
    deltax = 0.
    deltay = 0.
    deltax = deltax*dt
    deltay = deltay*dt
   
!where deltax < dx, deltax > -x-int(x):
    !IF (deltax < dx) THEN               !note, deltas could be negative
    !buoy%x = buoy%x + deltax/dx(ti, tj)
    buoy%x = buoy%x + deltax

!where deltay < dy:
    !buoy%y = buoy%y + deltay/dy(ti, tj)
    buoy%y = buoy%y + deltay

  RETURN
  END subroutine move


END MODULE drifter_mod
