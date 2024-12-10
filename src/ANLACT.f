      SUBROUTINE ANLACT(FNUM,P1,P2,P3,P4,P10,NRG,KST,IBAD,J1,
     X     J2,NGFLD,ISCALE,K,EL,TILTT,Y0,RG1,AZ,
     X     DRG,ICOPLANE,ROTANG)
C     
C     THIS SUBROUTINE REPLACES A FIELD FROM THE INPUT DATA TAPE WITH
C     THE OUTPUT OF AN ANALYTICAL FUNCTION.
C     
C     FNUM IS ANALYTICAL FUNCTION NUMBER
C     P1,P2,P3 ARE PARAMETERS FOR THE FUNCTIONS
C     
      PARAMETER(MAXRNG=768)
      COMMON /IO/KPCK(85000),KOUT(8500),IBUF(8500),NBUF(2125,4),IZ8(17),
     X           ILSTREC
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      CHARACTER*8 P10
      DATA DTR /0.0174533/

      ISCALE=100
      IF (FNUM.EQ.1) THEN
C     
C     REPLACE FIELD WITH A CONSTANT
C     
         DO 100 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 100
            IF (J.GT.NGFLD) GOTO 100
            KOUT(KST+J)=NINT(P1*ISCALE)
            K=K+1
 100     CONTINUE
      ELSE IF (FNUM.EQ.2) THEN
C     
C     REPLACE FIELD WITH 2 TERM RADIAL VELOCITY
C     
         VRVAL=(P1*SIN(AZ*DTR) + P2*COS(AZ*DTR))*
     X        COS(EL*DTR)
         DO 200 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 200
            IF(J.GT.NGFLD) GO TO 200
            KOUT(KST+J)=NINT(VRVAL*ISCALE)
            K=K+1
 200     CONTINUE
         
      ELSE IF (FNUM.EQ.3) THEN
C     
C     REPLACE FIELD WITH 3 TERM RADIAL VELOCITY
C     
         VRVAL=P1*SIN(AZ*DTR)*COS(EL*DTR) +
     X        P2*COS(AZ*DTR)*COS(EL*DTR) +
     X        P3*SIN(EL*DTR)
         DO 300 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 300
            IF(J.GT.NGFLD) GO TO 300
            KOUT(KST+J)=NINT(VRVAL*ISCALE)
            K=K+1
 300     CONTINUE
      ELSE IF (FNUM.EQ.8) THEN
C
C     REPLACE WITH RANGE
C
         IF (P1.EQ.0.0) P1=1.0

         DO 800 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 800
            IF (J.GT.NGFLD) GOTO 800
            R=RG1+(J-1)*DRG/1000.
            KOUT(KST+J)=NINT((P1*R)*ISCALE)
            K=K+1
 800     CONTINUE
      ELSE IF (FNUM.EQ.12) THEN
C
C     REPLACE FIELD WITH ROTATION ANGLE
C
         ISCALE=64
         DO 1200 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 1200
            IF (J.GT.NGFLD) GOTO 1200
            KOUT(KST+J)=NINT(ROTANG*ISCALE)
            K=K+1
 1200    CONTINUE
         
      ELSE IF (FNUM.EQ.13) THEN
C
C     REPLACE FIELD WITH TRACK RELATIVE TILT ANGLE
C
         DO 1300 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 1300
            IF (J.GT.NGFLD) GOTO 1300
            KOUT(KST+J)=NINT(TILTT*ISCALE)
            K=K+1
 1300    CONTINUE
      ELSE IF (FNUM.EQ.14) THEN
C
C     REPLACE FIELD WITH RANGE IN X-Z PLANE (Y IS DIRECTION OF TRACK)
C
         DO 1400 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 1400
            IF (J.GT.NGFLD) GOTO 1400
            R=RG1+(J-1)*DRG/1000.
            RGXZ=R*COS(TILTT*DTR)
            KOUT(KST+J)=NINT(RGXZ*ISCALE)
            K=K+1
 1400    CONTINUE
         
      ELSE IF (FNUM.EQ.15) THEN
C
C     REPLACE FIELD WITH Y DISTANCE (DISTANCE ALONG TRACK)
C
         DO 1500 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 1500
            IF (J.GT.NGFLD) GOTO 1500
            R=RG1+(J-1)*DRG/1000.
            Y=Y0+SIN(TILTT*DTR)*R
            KOUT(KST+J)=NINT(Y*ISCALE)
            K=K+1
 1500    CONTINUE
         
      ELSE
         WRITE(*,*)'***INVALID ANALYTIC FUNCTION FOR AIRBORNE SCANS'
         STOP
      END IF
         
      RETURN

      END
