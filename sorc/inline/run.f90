!----------------------------------------------------------------
SUBROUTINE run(buoys, nbuoy, u, v, dx, dy, nx, ny, dt, dtout)
  USE drifter_mod
  IMPLICIT none

  INTEGER, intent(in) :: nbuoy, nx, ny
  REAL, intent(in) :: u(nx, ny), v(nx, ny), dx(nx, ny), dy(nx, ny)
  REAL, intent(in) :: dt, dtout

  TYPE(drifter), intent(inout) ::  buoys(nbuoy)

  INTEGER k

  DO k = 1, nbuoy
    !c-like 
    CALL buoys(k)%move(u, v, dx, dy, dt, nx, ny)

    !CALL move(buoys(k), u, v, dx, dy, dt, nx, ny)
  ENDDO

  ! output time level if requested

END SUBROUTINE run
