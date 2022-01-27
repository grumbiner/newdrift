!haversine arcdis
!bearing
!grid rotation

!lat, lon in degrees
!dx, dy in meters
SUBROUTINE local_metric(ulat, ulon, dx, dy, rot, nx, ny)
  IMPLICIT none
  INTEGER, intent(in) :: nx, ny
  REAL, intent(in)  :: ulat(nx, ny), ulon(nx, ny)
  REAL, intent(out) :: dx(nx, ny), dy(nx, ny), rot(nx, ny)

  REAL  :: dlatdi(nx, ny), dlatdj(nx, ny)
  REAL  :: dlondi(nx, ny), dlondj(nx, ny)
  INTEGER i, j

  !debug PRINT *,'in local metric'

  rot = 0.
  CALL local_cartesian(ulat, dx, dy, nx, ny)
  DO j = 1, ny-1
  DO i = 1, nx-1
    dlatdi(i,j) = ulat(i+1,j) - ulat(i,j)
    dlondi(i,j) = ulon(i+1,j) - ulon(i,j)
    dlatdj(i,j) = ulat(i,j+1) - ulat(i,j)
    dlondj(i,j) = ulon(i,j+1) - ulon(i,j)
  ENDDO
  ENDDO

  RETURN
END subroutine local_metric


SUBROUTINE local_cartesian(ulat, dx, dy, nx, ny)
  IMPLICIT none
  INTEGER, intent(in) :: nx, ny
  REAL, intent(in)    :: ulat(nx, ny)
  REAL, intent(out)   :: dx(nx, ny), dy(nx, ny)

  INTEGER i, j
  REAL d2r

  !debug PRINT *,'in local cartesian'

  d2r = 3.141592653589793 / 180.

! From WGS84 via Amy Solomon, ESRL
  DO j = 1, ny
  DO i = 1, nx
    dy(i,j) = 111132.92 - 559.82*cos(2*ulat(i,j)*d2r) + &
                           1.175*cos(4*ulat(i,j)*d2r) - &
                          0.0023*cos(6*ulat(i,j)*d2r)
    dx(i,j) = 111412.84*cos(  ulat(i,j)*d2r) - &
                   93.5*cos(3*ulat(i,j)*d2r) + &
                  0.118*cos(5*ulat(i,j)*d2r)
  ENDDO
  ENDDO

  RETURN
END subroutine local_cartesian


