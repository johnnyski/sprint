      SUBROUTINE CRTTCP(ICART,ICTAB,NPREC,ICRTST,INPTST)
C
C     THIS SUBROUTINE GENERATES THE 2-D PLANES FOR COPLANE INTERPOLATION
C     AND ASSIGNS EACH (X,Y) GRID POINT AN AZIMUTH VALUE. THESE POINTS
C     ARE THEN SORTED IN ORDER OF INCREASING AZIMUTH.
C
      DIMENSION ICART(NPREC),ICTAB(4),AZC(4)
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /SETAZM/ USAZ1,USAZ2,USGAP,USINC,IFOR36,IFORDR
      LOGICAL IS360
C
C        COMMON BLOCK  /ADJUST/
C
C        SCLDBZ- SCALE FACTOR-  PACKED QUANTITY TO REFLECTIVITY FACTOR (DBZ)
C        SCLAZ-                 PACKED QUANTITY TO AZIMUTH (DEG)
C        SCLRNG-                PACKED QUANTITY TO GATE NUMBER
C        UNSCDB- INVERSE OF  SCLDBZ
C        UNSCAZ- INVERSE OF  SCLAZ
C        UNSCRG- INVERSE OF  SCLRNG
C
      COMMON/ADJUST/INFLD(10,3),SCLIF(10,2),NIF,IOFLD(10,3),SCLOF(10,2),
     X   NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,LOWAZ,IZAD,IS360,
     X   MINAZ,MAXAZ,METSPC(10),IWFLD(10),NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(10),CIOFLD(10)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      
C
C     FUNCTIONS FOR CONVERTING AN (X,Y) POINT RELATIVE TO THE ORIGIN
C     TO ONE RELATIVE TO THE RADAR
C
      FXC(X,Y) = X + XORTR
      FYC(X,Y) = Y + YORTR
C
C        CHECK IF REORDERING IS NECESSARY
C
      IF(ICRTST.EQ.0.AND.INPTST.EQ.0) RETURN
      INPTST=0
      ICRTST=0
C
C        ATR- DEGREES TO RADIANS CONVERSION FACTOR
C        RTA- RADIANS TO DEGREES CONVERSION FACTOR
C
      ATR=ATAN(1.)/45.
      RTA=1./ATR
      IZAD=360. * UNSCAZ

      ANGR=(ANGXAX-90.)*ATR
      ASNFKP=SIN(ANGR)
      ACSFKP=COS(ANGR)
      ASNF=0.0
      ACSF=1.0

C
C     FIND RADAR COORDINATES IN ROTATED COORDINATE SYSTEM
C
      XRAD=XORG*ACSFKP - YORG*ASNFKP
      YRAD=XORG*ASNFKP + YORG*ACSFKP

C
C     FIND COORDINATE OF LOWER LEFT HAND PART OF GRID, RELATIVE TO RADAR
C
      XORTR = X1-XRAD
      YORTR = Y1-YRAD

      NXY=NX*NY
      AZLOW=0.0
C
C     THE FOLLOWING REPRESENT THE COORDINATES OF THE 4 CORNERS OF
C     THE GRID, RELATIVE TO THE RADAR POSITION
C
      XLL=FXC(0.,0.)
      YLL=FYC(0.,0.)
      XTL=FXC(0.,Y2-Y1)
      YTL=FYC(0.,Y2-Y1)
      XTR=FXC(X2-X1,Y2-Y1)
      YTR=FYC(X2-X1,Y2-Y1)
      XLR=FXC(X2-X1,0.)
      YLR=FYC(X2-X1,0.)
      
      IS360=.FALSE.

C
C     FIND AZIMUTHAL LIMITS OF 2-D GRID
C
      IF (XLL.LE.0.0) XLL=0.001
      IF (YLL.EQ.0.0) YLL=0.001
      IF (XTL.LE.0.0) XTL=0.001
      AZC(1)=ATAN2(XLL,YLL)*RTA
      IF (YTL.EQ.0.0) YTL=0.001
      AZC(2)=ATAN2(XTL,YTL)*RTA
      IF (YTR.EQ.0.0) YTR=0.001
      AZC(3)=ATAN2(XTR,YTR)*RTA
      IF (YLR.EQ.0.0) YLR=0.001
      AZC(4)=ATAN2(XLR,YLR)*RTA
      DO 10 I=1,4
         IF (AZC(I).LT.0) AZC(I)=AZC(I)+180.0
 10   CONTINUE
      AZ1=1000.
      AZ2=-1000.
      DO 4 I=1,4
         IF(AZC(I).LT.AZ1) AZ1=AZC(I)
         IF(AZC(I).GT.AZ2) AZ2=AZC(I)
    4 CONTINUE
      AZTL=AMIN1(AZ1,AZ2)
      AZTR=AMAX1(AZ1,AZ2)
      MINAZ=(AZTL)*UNSCAZ+0.5
      MAXAZ=(AZTR)*UNSCAZ
      LOWAZ=AZLOW*UNSCAZ
C
C     CALCULATE AND CONVERT CARTESIAN COORDINATES TO POLAR COORDINATES RELATIVE
C     TO RADAR
C
      ICTAB(1)=0
      ICTAB(2)=0
      ICTAB(3)=0
      ICTAB(4)=0
      N=0
      Y=-YD
      DO 15 J=1,NY
         Y=Y+YD
         YTRM1=XORTR
         YTRM2=YORTR+Y
         X=-XD
         DO 15 I=1,NX
            X=X+XD
            XT=X+YTRM1
            YT=YTRM2
            IF (YT.EQ.0.0) YT=0.001
            AZ=ATAN2(XT,YT)*RTA
            IF(AZ.LT.0) AZ=AZ+180.0
            IAZ=AZ*UNSCAZ+0.5
            IF(IAZ.LT.LOWAZ) IAZ=IAZ+IZAD
            N=N+1
               
            CALL IPUTCP(ICART(N),IAZ,I,J)
 15      CONTINUE
C
C     SORT (X,Y) POSITIONS IN INCREASING AZIMUTH
C
         CALL SINSRT(ICART,1,N)

C
C     GENERATE TABLES AND WRITE TO DISK IF NECESSARY
C
      K1=1
      K2=NPREC
      IF(K2.GT.N) K2=N
      KB=K2-K1+1
      ICTAB(1)=1
      ICTAB(2)=KB
      CALL IGETCP2(ICART(K1),ICTAB(3),IX,IY)
      CALL IGETCP2(ICART(K2),ICTAB(4),IX,IY)
      RETURN
      END
