      SUBROUTINE VERSOUT
C
C     THIS SUBROUTINE OUTPUTS THE DATE THE EXECUTABLE WAS CREATED
C
      WRITE(*,10)
 10   FORMAT(/'---SPRINT: Sorted Position Radar Interpolation ')
      WRITE(*,15)
 15   FORMAT('---COPYRIGHT (C) 1993 UNIVERSITY CORPORATION FOR',
     X     ' ATMOSPHERIC RESEARCH',/,'---ALL RIGHTS RESERVED.')

      WRITE(*,20)
 20   FORMAT('---RELEASE DATE: Jan. 19, 1994'/)


      RETURN

      END
