      SUBROUTINE OUTPCK(IVOL,NX,NY,NZ,IAXK,ITER,NFLDS,
     X     ITAPHD,CSP,ICAX)
      INCLUDE 'SPRINT.INC'
      PARAMETER (NID=296,LD09P=-9999,NKOM=11,MAXFLD=8,MAXPLN=65536,
     X     MAXYZ=512000,MXCRT=128,NRCBF=400000,IDIM=64/WORDSZ)
      INTEGER CVMGP
      DIMENSION LHED(10),IVOL(IDIM,2,NX,NY,NZ),IAXK(ITER),CSP(3,3),
     X     ITAPHD(510),RBUF(MAXPLN),SCLFLD(MAXFLD)
      CHARACTER*1 LAX(3)
      COMMON ICART(MAXPLN),ICOB(IDIM,2,MAXYZ),IBLV(IDIM,NRCBF,8),
     X     ZRTAB(MXCRT,2)
      
      COMMON /UNITS/LUN,LSKP,FEETS
      COMMON /UNITSC/ IPROJ,LTAP,ISCI
      CHARACTER*4 IPROJ
      CHARACTER*8 LTAP,ISCI
      
      COMMON /DISKIO/ IOTYPE,IPOS
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      DIMENSION IBUF(16)
      COMMON /BYTORD/ MBYTE
      
      CHARACTER*8 CTEMP1,LOUTNM,NAMFLD(MAXFLD),ITIT(10)
      DATA LAX/'X','Y','Z'/
      
C      DATA MAS/O'177777'/
      DATA ITIT/10*' '/
      DATA IBEG/1/
      DATA CARBAD/1E+07/
      IDLEN=510
      LHLEN=10
      ISKP=0
      
C     
C     PUT OUT TAPE HEADER
C     
      LHD8=ITAPHD(96)
      LHD9=ITAPHD(97)
      DO 90 L=1,NFLDS
         IF (MBYTE.EQ.0) THEN
            CALL SBYTES(ITMP1,ITAPHD(171+L*5),0,16,0,2)
            CALL SBYTES(ITMP2,ITAPHD(173+L*5),0,16,0,2)
            WRITE(NAMFLD(L),74)ITMP1,ITMP2
 74         FORMAT(2A4)
         ELSE
c            CALL SBYTES(ITMP,ITAPHD(172+L*5),0,32,0,1)
c            CALL SBYTES(ITMP,ITAPHD(171+L*5),16,32,0,1)
            WRITE(NAMFLD(L),63)ITAPHD(171+L*5),ITAPHD(172+L*5),
     X           ITAPHD(173+L*5),ITAPHD(174+L*5)
 63         FORMAT(4A2)
         END IF
         SCLFLD(L)=1.0/ITAPHD(175+L*5)
 90   CONTINUE

      
      
      CALL BLDDES(ITAPHD,IBUF)
      IF (LUN.LE.0) THEN
         CALL TPQERX(324,1)
      ELSE
 2       IF (LUN.LT.10) THEN
            WRITE (CTEMP1,302)LUN
            READ (CTEMP1,510)LOUTNM
 510        FORMAT(A8)
 302        FORMAT('fort.',I1,'  ')
         ELSE
            WRITE (CTEMP1,303)LUN
            READ (CTEMP1,510)LOUTNM
 303        FORMAT('fort.',I2,' ')
         END IF
      END IF
      READ(LOUTNM,525)L1
 525  FORMAT(5X,I1)
      READ(LOUTNM,526)L2
 526  FORMAT(6X,I1)
      IF (IBEG.EQ.1) THEN
C     
C     IF FIRST OUTPUT VOLUME, INITIALIZE POSITION OF OUTPUT FILE
C     
         CALL COUT(L1,L2,IPOS,ISKP,IBUF)
         IBEG=0
         IPOS=2
      ELSE IF (IBEG.EQ.0) THEN
         CALL COUT(L1,L2,IPOS,ISKP,IBUF)
         IPOS=2
      END IF
      CALL SBYTES(ITAPHD,ITAPHD,0,16,0,510)
      CALL CWRITE(ITAPHD,IDLEN)
      NST=0
      IF (NST.NE.0) GOTO 500
C     
C     CHECK FOR X,Y, OR Z (300,200,100  RESP.)
C     
      GOTO (300,200,100),ICAX
      
C     
C     CONSTANT Z-AXIS
C     
 100  CONTINUE
      IF ((ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4) .AND.
     X     IPPI.EQ.0) THEN
         PRINT 106, LAX(ICAX)
      ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
         PRINT 107
      ELSE IF (ICOPLANE.EQ.0 .AND. IPPI.EQ.1) THEN
         PRINT 108
      END IF
 106  FORMAT (//3X,'LEVEL',3X,A1,' KM',5X,'FIELD',5X,9X,'MEAN',8X,
     X     'MNSQ',8X,'STDV',5X,'NMPT',3X,'I1',3X,'I2',3X,'J1',3X,
     X     'J2',12X,'MIN',9X,'MAX')
 107  FORMAT (//3X,'LEVEL',3X,'COPL',5X,'FIELD',5X,9X,'MEAN',8X,
     X     'MNSQ',8X,'STDV',5X,'NMPT',3X,'I1',3X,'I2',3X,'J1',3X,
     X     'J2',12X,'MIN',9X,'MAX')
 108  FORMAT (//3X,'LEVEL',3X,'ELEV',5X,'FIELD',5X,9X,'MEAN',8X,
     X     'MNSQ',8X,'STDV',5X,'NMPT',3X,'I1',3X,'I2',3X,'J1',3X,
     X     'J2',12X,'MIN',9X,'MAX')
C     
C     PURE BINARY I/O
C     
      IPOS=2
      DO 550 K=1,NZ
         IF (IPPI.EQ.0) THEN
            CVAL=CSP(1,ICAX) + (K-1)*CSP(3,ICAX)
         ELSE
            CVAL=ZRTAB(K,1)
         END IF
         CALL BLHED(CVAL,K,NFLDS,ITER,LHED,LHD8,LHD9)
         CALL SBYTES(LHED,LHED,0,16,0,10)
         CALL CWRITE(LHED,LHLEN)
         DO 540 L=1,NFLDS
            SCALE=SCLFLD(L)
            ITIT(1)=NAMFLD(L)
            INDX=0
            IFWP = ((L-1)/4) + 1
            LPOS = L - ((IFWP-1) * 4)
            MSHIF = 64 - (16*LPOS)
            DO 530 J=1,NY
               DO 520 I=1,NX
                  INDX=INDX+1
                  CALL GBYTES(IVOL(1,IFWP,I,J,K),IAXK(INDX),
     X                 48-MSHIF,16,0,1)
                  X=IAXK(INDX)
                  IF (X.EQ.32768) THEN
                     RBUF(INDX)=CARBAD
                     GOTO 520
                  ENDIF
                  RBUF(INDX)=CVMGP(NINT(X-65536.),NINT(X),
     X                 NINT(X-32768.))*SCALE
 520           CONTINUE
 530        CONTINUE
            CALL STRID(ITIT(1),RBUF,NX,NY,CARBAD,N,K,CVAL)
            CALL SBYTES(IAXK,IAXK,0,16,0,ITER)
            CALL CWRITE(IAXK,ITER)
 540     CONTINUE
 550  CONTINUE
      
      RETURN
C     
C     CONSTANT Y-AXIS
C     
 200  CONTINUE
      PRINT 106, LAX(ICAX)
      DO 250 J=1,NY
         CVAL=CSP(1,ICAX) + (J-1)*CSP(3,ICAX)
         CALL BLHED(CVAL,J,NFLDS,ITER,LHED,LHD8,LHD9)
         CALL TAPOUT(LUN,LHED,10,3200,IPACK,NST)
         IF (NST.NE.0) GOTO 500
         DO 240 L=1,NFLDS
            SCALE=SCLFLD(L)
            ITIT(1)=NAMFLD(L)
            INDX=0
            IFWP = ((L-1)/4) + 1
            LPOS = L - ((IFWP-1) * 4)
            MSHIF = 64 - (16*LPOS)
            DO 230 K=1,NZ
               DO 220 I=1,NX
                  INDX=INDX+1
C     IAXK(INDX)=ICEDAND(ICEDSHFT(IVOL(IFWP,I,J,K),
C     X                 -MSHIF),MAS)
                  CALL GBYTES(IVOL(1,IFWP,I,J,K),IAXK(INDX),
     X                 48-MSHIF,16,0,1)
                  X=IAXK(INDX)
                  IF (X.EQ.32768) THEN
                     RBUF(INDX)=CARBAD
                     GOTO 220
                  ENDIF
                  RBUF(INDX)=CVMGP(NINT(X-65536.),NINT(X),
     X                 NINT(X-32768.))*SCALE
 220           CONTINUE
 230        CONTINUE
C     CALL RITER(RBUF,NX,NZ,J,CVAL,ITIT,1,1,0,'Y')
            CALL STRID(ITIT(1),RBUF,NX,NZ,CARBAD,N,J,CVAL)
            CALL TAPOUT(LUN,IAXK,ITER,3200,IPACK,NST)
            IF (NST.NE.0) GOTO 500
 240     CONTINUE
 250  CONTINUE
      ENDFILE LUN
      ENDFILE LUN
      BACKSPACE LUN
      RETURN
C     
C     CONSTANT X-AXIS
C     
 300  CONTINUE
      PRINT 106, LAX(ICAX)
      DO 350 I=1,NX
         CVAL=CSP(1,ICAX) + (I-1)*CSP(3,ICAX)
         CALL BLHED(CVAL,I,NFLDS,ITER,LHED,LHD8,LHD9)
         CALL TAPOUT(LUN,LHED,10,3200,IPACK,NST)
         IF (NST.NE.0) GOTO 500
         DO 340 L=1,NFLDS
            SCALE=SCLFLD(L)
            ITIT(1)=NAMFLD(L)
            INDX=0
            IFWP = ((L-1)/4) + 1
            LPOS = L - ((IFWP-1) * 4)
            MSHIF = 64 - (16*LPOS)
            DO 330 K=1,NZ
               DO 320 J=1,NY
                  INDX=INDX+1
C     IAXK(INDX)=ICEDAND(ICEDSHFT(IVOL(IFWP,I,J,K),
C     X                 -MSHIF),MAS)
                  CALL GBYTES(IVOL(1,IFWP,I,J,K),IAXK(INDX),
     X                 48-MSHIF,16,0,1)
                  X=IAXK(INDX)
                  IF (X.EQ.32768) THEN
                     RBUF(INDX)=CARBAD
                     GOTO 320
                  ENDIF
                  RBUF(INDX)=CVMGP(NINT(X-65536.),NINT(X),
     X                 NINT(X-32768.))*SCALE
 320           CONTINUE
 330        CONTINUE
C     CALL RITER(RBUF,NY,NZ,I,CVAL,ITIT,1,1,0,'Y')
            CALL STRID(ITIT(1),RBUF,NY,NZ,CARBAD,N,I,CVAL)
            CALL TAPOUT(LUN,IAXK,ITER,3200,IPACK,NST)
            IF (NST.NE.0) GOTO 500
 340     CONTINUE
 350  CONTINUE

      RETURN
C     
C     ERRORS
C     
 500  CONTINUE
      PRINT 501
 501  FORMAT (5X,'***ERROR IN WRITING OUTPUT VOLUME***')
      STOP
      END
