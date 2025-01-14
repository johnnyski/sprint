      SUBROUTINE SKPVOL(IUNIT,ISKIP)
C
C        SKIPS ISKIP VOLUMES (FILES) ON UNIT IUNIT
C
      PARAMETER (MXRAY=10000)
      COMMON /BYTORD/ MBYTE

      IF (ISKIP) 100,200,300
 100  CONTINUE
C         REWIND(IUNIT)
         RETURN
 200  CONTINUE
         RETURN
 300  CONTINUE
         KTREC = 0
         ICNT=0
         DO 500 I=1,ISKIP
            ICNT=0
 350        CONTINUE
            ICNT=ICNT+1
            IF (ICNT.GT.MXRAY) THEN
               WRITE(*,20)
 20            FORMAT(/,5X,'+++ERROR SKIPPING VOLUME(S) BEFORE ',
     X              'PROCESSING. TOO MANY BEAMS.+++')
               STOP
            END IF
            CALL NWRAY(NWCT,MBYTE,NST)
            IF(NST .GT. 1) GO TO 600
            IF(NST .EQ. 1) GO TO 500
            KTREC = KTREC+1
            GO TO 350
 500     CONTINUE
C
      RETURN
C
 600  CONTINUE
      PRINT 601,KTREC+1,I,IUNIT,ISKIP,NST
 601  FORMAT (5X,'***ERROR SKIPPING RECORD ',I4,' OF FILE ',I4,
     X   ' IN CALL TO SKPVOL(',I3,',',I4,') - STATUS = ',I1)
         STOP
      END
