MODULE mo_util

 IMPLICIT NONE
 INCLUDE 'netcdf.inc'

CONTAINS
SUBROUTINE check(status)

           
  INTEGER, INTENT(IN) :: status
   
  IF (status /= NF_NOERR) THEN
    PRINT *, 'Error: ', nf_strerror(status)
    STOP 2
  END IF

END SUBROUTINE check
END MODULE mo_util
