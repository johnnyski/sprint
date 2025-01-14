      SUBROUTINE RDRYDK(KPKBUF,KUNBUF,NST,IUN,IFLG,NLEN)
C
C        RETURNS A SINGLE BLOCK OF LENGTH NLEN OF 
C        16 BIT INFORMATION IN KUNBUF
C     IFLG = 0 ==> NORMAL READ
C     ELSE ==> FORCED READ FROM DISK
C
C     IBEGREC  - RECORD LENGTH OF FIRST RECORD IN VOLUME
C     ILSTREC  - RECORD LENGTH OF NEXT RECORD
C     NLEN     - RECORD LENGTH OF CURRENT RECORD (RETURNED)
C     IBEG     - FLAG SET WHEN A VOLUME IS FULLY WRITTEN; UNSET AFTER
C                FIRST RECORD IS READ IN
C     
      PARAMETER (MAXBLK=85000)
      COMMON /IO/KPCK(85000),KOUT(8500),IBUF(8500),NBUF(2125,4),
     X     IZ8(17),IBEGREC
      COMMON /INTIO/IBEG
      COMMON /RDRY/ILSTREC
      DIMENSION KPKBUF(ILSTREC),KUNBUF(ILSTREC-1)


      IF (IBEG.NE.1) THEN
C
C     NORMAL READ
C
         READ(IUN,ERR=100) IVAL, KUNBUF
         NLEN=ILSTREC-1
         ILSTREC=IVAL
      ELSE
C
C     READ FIRST RECORD
C
         CALL RDRYDK2(IUN,KPKBUF,IBEGREC,NST)
         IF (NST.NE.0) GOTO 100
         NLEN=IBEGREC-1
         DO I=1,IBEGREC-1
            KUNBUF(I)=KPKBUF(I+1)
         END DO
         ILSTREC=KPKBUF(1)
         IBEG=0
      END IF

      NST=0
      RETURN

 100  NST=1
      RETURN


      END




      SUBROUTINE RDRYDK2(IUN,KPKBUF,IBEGREC,NST)
      DIMENSION KPKBUF(IBEGREC)

      
      READ(IUN,ERR=100) KPKBUF

      NST=0
      RETURN

 100  NST=1
      RETURN


      END
