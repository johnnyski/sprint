      SUBROUTINE ANLREP(FNUM,P1,P2,P3,P4,P10,NRG,KST,IBAD,J1,J2,NGFLD,
     X     ISCALE,K,EL,FIX,AZZ,ICOPLANE,RG1,DRG,XORR,YORR,ZORR)
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
      DATA PI /3.14159265/
      DATA DTR /0.0174533/


C     
C     THE FOLLOWING FUNCTION CALCULATES ELEV ANGLE FROM COPLANE AND HOR AZIM.
C     
      CALEL(F,A)=ATAN((TAN(F*DTR)*ABS(SIN(A*DTR))))/DTR
C
C     USE NOMINAL ELEV ANGLE AS CALCULATED FROM FIX ANGLE INSTEAD OF ACTUAL 
C
      IF (P10.EQ.'NOMINAL') THEN
         IF (ICOPLANE.GE.1) THEN
            EL=CALEL(FIX,AZZ)
         ELSE
            EL=FIX
         END IF
      END IF

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
         VRVAL=(P1*SIN(AZZ*DTR) + P2*COS(AZZ*DTR))*
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
         VRVAL=P1*SIN(AZZ*DTR)*COS(EL*DTR) +
     X        P2*COS(AZZ*DTR)*COS(EL*DTR) +
     X        P3*SIN(EL*DTR)
         DO 300 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1 .OR. J.GT.J2) GOTO 300
            IF(J.GT.NGFLD) GO TO 300
            KOUT(KST+J)=NINT(VRVAL*ISCALE)
            K=K+1
 300     CONTINUE
      ELSE IF (FNUM.EQ.4) THEN
C     
C     RANDOM UNIFORM DIST. BETWEEN P1 AND P2
C     
         CON = P2-P1
         DO 400 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 400
            IF (J.GT.NGFLD) GOTO 400
            VX=RANF()
            KOUT(KST+J)=NINT((CON*VX + P1)*ISCALE)
            K=K+1
 400     CONTINUE
         
      ELSE IF (FNUM.EQ.5) THEN
C     
C     NORMAL DIST. WITH P1 MEAN AND P2 STAN. DEVIATION
C     
         IF (P2.LE.0.0) P2=1.0
         PI2=ATAN(1.)*8.0
         DO 500 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 500
            IF (J.GT.NGFLD) GOTO 500
            U1=RANF()
            U2=RANF()
            VX=SQRT(-2.0*ALOG(U1))*COS(PI2*U2)
            KOUT(KST+J)=NINT((P2*VX + P1)*ISCALE)
            K=K+1
 500     CONTINUE
         
      ELSE IF (FNUM.EQ.6) THEN
C     
C     REPLACE WITH COSINE PRODUCT
C     
         IF (P2.LE.0.0) THEN
            T1=0.0
         ELSE
            T1=2*PI/P2
         END IF
         
         IF (P3.LE.0.0) THEN
            T2=0.0
         ELSE
            T2=2*PI/P3
         END IF
         
         IF (P4.LE.0.0) THEN
            T3=0.0
         ELSE
            T3=2*PI/P4
         END IF

         DO 600 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 600
            IF (J.GT.NGFLD) GOTO 600
            R=RG1+(J-1)*DRG
            X=R*SIN(AZZ*DTR)*COS(EL*DTR) + XORR
            Y=R*COS(AZZ*DTR)*COS(EL*DTR) + YORR
            Z=R*SIN(EL*DTR) + ZORR
            KOUT(KST+J)=NINT((P1*COS(T1*X)*COS(T2*Y)*COS(T3*Z))*ISCALE)
            K=K+1
 600     CONTINUE
      ELSE IF (FNUM.EQ.7) THEN
C
C     RADIAL VELOCITY THAT SATISFIES THE CONTINUITY EQUATION
C
         IF (P2.EQ.0.0) THEN
            T1=0.0
         ELSE 
            T1=2*PI/P2
         END IF
         IF (P3.EQ.0.0) THEN
            T2=0.0
         ELSE
            T2=2*PI/P3
         END IF
         IF (P4.EQ.0.0) THEN
            T3=0.0
         ELSE
            T3=2*PI/P4
         END IF
         DO 700 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 700
            IF (J.GT.NGFLD) GOTO 700
            R=RG1+(J-1)*DRG
            X=R*SIN(AZZ*DTR)*COS(EL*DTR) + XORR
            Y=R*COS(AZZ*DTR)*COS(EL*DTR) + YORR
            Z=R*SIN(EL*DTR) + ZORR
            U=P1*P2/(4*PI)*SIN(T1*X)*COS(T2*Y)*COS(T3*Z)
            V=P1*P3/(4*PI)*COS(T1*X)*SIN(T2*Y)*COS(T3*Z)
            W=-P1*P4/(2*PI)*COS(T1*X)*COS(T2*Y)*SIN(T3*Z)
            VAL=U*SIN(AZZ*DTR)*COS(EL*DTR) + 
     X          V*COS(AZZ*DTR)*COS(EL*DTR) + W*SIN(EL*DTR)
            KOUT(KST+J)=NINT(VAL*ISCALE)
            K=K+1
 700     CONTINUE
      ELSE IF (FNUM.EQ.8) THEN
C
C     REPLACE WITH RANGE
C
         IF (P1.EQ.0.0) P1=1.0

         DO 800 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 800
            IF (J.GT.NGFLD) GOTO 800
            R=RG1+(J-1)*DRG
            KOUT(KST+J)=NINT((P1*R)*ISCALE)
            K=K+1
 800     CONTINUE
         
      ELSE IF (FNUM.EQ.9) THEN
C
C     REPLACE WITH ELEVATION
C
         IF (P1.EQ.0.0) P1=1.0
         DO 900 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 900
            IF (J.GT.NGFLD) GOTO 900
            KOUT(KST+J)=NINT((EL*P1)*ISCALE)
            K=K+1
 900     CONTINUE

      ELSE IF (FNUM.EQ.10) THEN
C
C     REPLACE WITH HORIZONTAL AZIMUTH
C
         IF (P1.EQ.0.0) P1=1.0
         DO 1000 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 1000
            IF (J.GT.NGFLD) GOTO 1000
            KOUT(KST+J)=NINT((AZZ*P1)*ISCALE)
            K=K+1
 1000    CONTINUE

      ELSE IF (FNUM.EQ.11) THEN
C
C     REPLACE WITH PRODUCT OF THREE SQUARE WAVES
C
         IF (P2.LE.0.0) THEN
            T1=0.0
         ELSE
            T1=2*PI/P2
         END IF
         
         IF (P3.LE.0.0) THEN
            T2=0.0
         ELSE
            T2=2*PI/P3
         END IF
         
         IF (P4.LE.0.0) THEN
            T3=0.0
         ELSE
            T3=2*PI/P4
         END IF

         DO 1020 J=1,MAXRNG
            KOUT(KST+J)=IBAD
            IF (J.LT.J1. OR. J.GT.J2) GOTO 1020
            IF (J.GT.NGFLD) GOTO 1020
            R=RG1+(J-1)*DRG
            X=R*SIN(AZZ*DTR)*COS(EL*DTR) + XORR
            Y=R*COS(AZZ*DTR)*COS(EL*DTR) + YORR
            Z=R*SIN(EL*DTR) + ZORR
            SIGN1=1
            SIGN2=1
            SIGN3=1
            SIGN1=SIGN(SIGN1,COS(T1*X))
            SIGN2=SIGN(SIGN3,COS(T2*Y))
            SIGN3=SIGN(SIGN3,COS(T3*Z))
            KOUT(KST+J)=NINT((P1*SIGN1*SIGN2*SIGN3)*ISCALE)
            K=K+1
 1020     CONTINUE

      ELSE
         WRITE(*,*)'+++UNKNOWN ANALYTICAL FUNCTION IN ANLREP+++'
         STOP
      END IF
      
      RETURN
      
      END
