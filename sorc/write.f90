!----------------------------------------------------------------
! Write out results -- drift distance and direction

SUBROUTINE writeout(ncid_out, varid_out, nvar_out, buoys, nbuoy, close)
  USE drifter_mod
  USE io

  IMPLICIT none
  INTEGER ncid_out, nvar_out, nbuoy
  INTEGER varid_out(nvar_out)
  TYPE(drifter) :: buoys(nbuoy)
  LOGICAL :: close

  !debug: PRINT *,'calling outvars, buoy1%x = ',buoys(1)%x
  CALL outvars(ncid_out, varid_out, nvar_out, buoys, nbuoy )

  IF (close) THEN
    CALL close_out(ncid_out)
  ENDIF

END SUBROUTINE writeout
