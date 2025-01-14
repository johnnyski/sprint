      SUBROUTINE UFCHARS(IBUF,MBYTE)
C
C     THIS SUBROUTINE PROCESSES CHARACTERS IN UNIVERSAL FORMAT BEAMS.
C     IT FINDS ALL THE CHARACTERS IN THE HEADER AND, DEPENDING ON THE
C     BYTE ORDERING OF THE MACHINE (MBYTE), IT SHIFTS THEM OR IT SWAPS THEM.
C
      INCLUDE 'SPRINT.INC'
      DIMENSION IBUF(8500)
      DATA IEDBD1/ O'777' /
      DATA IVECHAR/'VE'/

      IF (MBYTE.EQ.0) THEN
C
C     BIG ENDIAN
C
         CALL SHILBL(IBUF(1),1)
         CALL SHILBL(IBUF(11),8)
         CALL SHILBL(IBUF(32),1)
         CALL SHILBL(IBUF(41),4)

         IDAT=IBUF(5)

         NFLDS=IBUF(IDAT)

         DO I=1,NFLDS
            CALL SHILBL(IBUF(IDAT+3+(I-1)*2),1)
            IFLD=IBUF(IDAT+4+(I-1)*2)
            CALL SHILBL(IBUF(IFLD+13),1)
            IEDFLD=IBUF(IFLD+16)
            IF (IEDFLD.NE.32768 .AND. IEDFLD.NE.0 .AND. 
     X           IEDFLD.NE.IEDBD1) THEN
               CALL SHILBL(IBUF(IFLD+16),1)
            END IF
            IF (IBUF(IDAT+3+(I-1)*2).EQ.IVECHAR) 
     X           CALL SHILBL(IBUF(IFLD+20),1)
         END DO

      ELSE IF (MBYTE.EQ.1) THEN
C
C     LITTLE ENDIAN
C
*         print1020,ibuf(1), (ibuf(i),i=11,18), ibuf(32),
*     +        (ibuf(i), i=41,44)
* 1020    format('/ ibuf=', 14z8.8)
*         print1021,ibuf(1), (ibuf(i),i=11,18), ibuf(32),
*     +        (ibuf(i), i=41,44)
* 1021    format('/ ibuf=', 14a8)
         CALL SWAPCHAR(IBUF(1),IBUF(1),1)
         CALL SWAPCHAR(IBUF(11),IBUF(11),8)
         CALL SWAPCHAR(IBUF(32),IBUF(32),1)
         CALL SWAPCHAR(IBUF(41),IBUF(41),4)
*         print1020,ibuf(1), (ibuf(i),i=11,18), ibuf(32),
*     +        (ibuf(i), i=41,44)
*         print1021,ibuf(1), (ibuf(i),i=11,18), ibuf(32),
*     +        (ibuf(i), i=41,44)
         IDAT=IBUF(5)

         NFLDS=IBUF(IDAT)

         DO I=1,NFLDS
            CALL SWAPCHAR(IBUF(IDAT+3+(I-1)*2),IBUF(IDAT+3+(I-1)*2),1)
            IFLD=IBUF(IDAT+4+(I-1)*2)
            CALL SWAPCHAR(IBUF(IFLD+13),IBUF(IFLD+13),1)
            IEDFLD=IBUF(IFLD+16)
            IF (IEDFLD.NE.32768 .AND. IEDFLD.NE.0 .AND. 
     X           IEDFLD.NE.IEDBD1) THEN
               CALL SWAPCHAR(IBUF(IFLD+16),IBUF(IFLD+16),1)
            END IF
            IF (IBUF(IDAT+3+(I-1)*2).EQ.IVECHAR) 
     X           CALL SWAPCHAR(IBUF(IFLD+20),IBUF(IFLD+20),1)
         END DO
         
      END IF

      RETURN

      END
