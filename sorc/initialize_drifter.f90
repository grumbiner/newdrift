SUBROUTINE zero_buoys(buoys, nbuoy)
  USE drifter_mod

  IMPLICIT none

  INTEGER, intent(in) :: nbuoy
  TYPE(drifter), intent(inout) :: buoys(nbuoy)

  INTEGER k

  !Zero the buoys:
  DO k = 1, nbuoy
    !debug: PRINT *,'zero buoy ',k
    CALL buoys(k)%zero(k)
  ENDDO
  RETURN
END SUBROUTINE zero_buoys

SUBROUTINE dummy_buoys(aice, dx, dy, u, v, ulat, ulon, nx, ny, buoys, nbuoys) 
  USE drifter_mod
  IMPLICIT none
  INTEGER, intent(in) :: nx, ny
  REAL, intent(in) :: aice(nx, ny), dx(nx, ny), dy(nx, ny)
  REAL, intent(in) :: u(nx, ny), v(nx, ny)
  REAL, intent(in) :: ulat(nx, ny), ulon(nx, ny)
  INTEGER, intent(inout) :: nbuoys
  TYPE(drifter), intent(inout) :: buoys(nbuoys)

  INTEGER i, j, k, ratio, imax, jmax

  PRINT *,'entered dummy_buoys'
 
! Dummy for testing
  ratio = 5
  imax = INT(nx/ratio)
  jmax = INT(ny/ratio)

  IF (nbuoys .NE. imax*jmax) THEN
    PRINT *,'mismatch in sizes ',nbuoys,' vs ',imax*jmax
    STOP
  ENDIF
 
  k = 0
  DO j = 1, jmax
  DO i = 1, imax
    !debug: PRINT *,'i,j = ',i,j, k
    IF (aice(i*ratio, j*ratio) > 0. .AND. aice(i*ratio,j*ratio) <= 1.0  .AND. &
        dx(i*ratio, j*ratio) .NE. 0. .AND. dy(i*ratio, j*ratio) .NE. 0. .AND. &
        ABS(u(i*ratio, j*ratio)) < 100. .AND. ABS(v(i*ratio, j*ratio)) < 100. ) THEN
      !debug: PRINT *,'aice, dx, dy, u, v',aice(i*ratio, j*ratio), dx(i*ratio, j*ratio), &
      !debug:   dy(i*ratio, j*ratio), u(i*ratio, j*ratio), v(i*ratio, j*ratio)

      k = k + 1
      !debug: PRINT *,ulat(i*ratio, j*ratio), ulon(i*ratio, j*ratio)
      buoys(k)%ilat = ulat(i*ratio, j*ratio)
      !debug: PRINT *,buoys(k)%ilat
      buoys(k)%ilon = ulon(i*ratio, j*ratio)
      !debug: PRINT *,buoys(k)%ilon
      buoys(k)%clat = ulat(i*ratio, j*ratio)
      buoys(k)%clon = ulon(i*ratio, j*ratio)
      !debug: PRINT *,buoys(k)%ilat, buoys(k)%ilon, buoys(k)%clat, buoys(k)%clon

      !RG: Must compute physical value here, location vs. grid mesh function
      !CALL ll_to_xy(buoys(k)%ilat, buoys(k)%ilon, ulat, ulon, x, y, nx, ny)
      buoys(k)%x = i*ratio
      buoys(k)%y = j*ratio

    ENDIF
  ENDDO
  ENDDO
  !debug:
  PRINT *,'nbuoys, k',nbuoys, k

  RETURN
END SUBROUTINE dummy_buoys
