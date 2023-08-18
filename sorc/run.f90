!----------------------------------------------------------------
SUBROUTINE run(buoys, nbuoy, u, v, dx, dy, nx, ny, dt, dtout)
  USE drifter_mod
  IMPLICIT none

  INTEGER, intent(in) :: nbuoy, nx, ny
  REAL, intent(in) :: u(nx, ny), v(nx, ny), dx(nx, ny), dy(nx, ny)
  REAL, intent(in) :: dt, dtout

  TYPE(drifter), intent(inout) ::  buoys(nbuoy)

  INTEGER k

  PRINT *,'entered run, nbuoy = ',nbuoy
  PRINT *,'nx ny = ',nx, ny
  PRINT *,'dt = ',dt
  PRINT *,'u ',MAXVAL(u), MINVAL(u)
  PRINT *,'v ',MAXVAL(v), MINVAL(v)
  PRINT *,'dx ',MAXVAL(dx), MINVAL(dx)
  PRINT *,'dy ',MAXVAL(dy), MINVAL(dy)

  DO k = 1, nbuoy
    !debug IF ( MOD(k,1000) .EQ. 0) THEN
    !debug  PRINT *,'k = ',k, buoys(k)%x, buoys(k)%ilat, buoys(k)%clon
    !debugENDIF

    !c-like 
    CALL buoys(k)%move(u, v, dx, dy, dt, nx, ny)

    !CALL move(buoys(k), u, v, dx, dy, dt, nx, ny)
  ENDDO
  PRINT *,'leaving run'

  ! output time level if requested

END SUBROUTINE run
