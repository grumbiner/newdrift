!haversine arcdis
!  http://www.movable-type.co.uk/scripts/gis-faq-5.1.html
!assumes lat lon in degrees
REAL FUNCTION harcdis(lat1, lon1, lat2, lon2)
  IMPLICIT none
  REAL lat1, lon1, lat2, lon2
  REAL dlat, dlon, mlat
  REAL pi, rpd, a, c, d, R
  PARAMETER(pi = 3.141592653589793 )
  PARAMETER(rpd = pi/180.)
  !PARAMETER(R  = 6371.2) ! km

  dlon = lon2 - lon1
  dlat = lat2 - lat1
  mlat = (lat1 + lat2)/2.

  a = sin(dlat*rpd/2)**2 + cos(lat1*rpd)*cos(lat2*rpd)*sin(dlon*rpd/2)**2
  c = 2.*asin(min(1.,sqrt(a)))

! approximating ellipsoidal flattening
  harcdis = c * (6378 - 21.*sin(mlat*rpd) )
  RETURN 
END function harcdis

!bearing
!  convert_bw (bearing to weather direction convention)
!  convert_wb (converse)
!  bearing    (convert lat-lon pair to distance/bearing)
!  unbearing  (convert distance,bearing and starting point to final point)
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
  !rot = atan2(?,?)

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
! Meters per degree
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


!Bilinear interpolation to buoy.x,y

SUBROUTINE parms
!Parameters:
  REAL kmtonm
  PARAMETER (kmtonm = 1. /  1.852 )

  REAL pi, d2r, rpd
  PARAMETER (pi = 3.141592653589793)
  PARAMETER (d2r = pi/180.)
  PARAMETER (rpd = d2r)

  REAL nansen_ampl, nansen_rotation
  PARAMETER (nansen_ampl = 1.468e-2)
  PARAMETER (nansen_rotation = 28.0)

RETURN
END subroutine parms

