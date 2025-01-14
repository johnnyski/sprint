      SUBROUTINE SETVOL
      PARAMETER (MAXEL=80,MAXIN=8500,NIOB=85000,NID=296)
      PARAMETER (MAXRNG=768)
      LOGICAL IS360
      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /IDBLK/ ID(NID)
      COMMON/ADJUST/INFLD(10,3),SCLIF(10,2),NIF,IOFLD(10,3),SCLOF(10,2),
     X   NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,LOWAZ,IZAD,IS360,
     X   MINAZ,MAXAZ,METSPC(10),IWFLD(10),NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(10),CIOFLD(10)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /IO/ KPCK(NIOB),IRAY(MAXIN,3),KZ(3),KZC,KZP,IDSNEW,ITMDIF,
     X   ITIME(4),IBEGT(4),NSBM,NRDOPT,ILSTREC
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X   NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,CFAC(10)
      COMMON /COPE/ IADJAZ
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      COMMON /COPLAN/ ICOPLN,BASAD
      CHARACTER*8 CTEMP1,NAMF
      LOGICAL ICOPLN
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG
      EQUIVALENCE (ID(41),KNQ), (ID(42),KRC)
C      DATA EPS/10.0/
C
C        READ IN VOLUME HEADER
C
      IREC=0
      IBEGT(1)=ID(4)
      IBEGT(2)=ID(5)
      IBEGT(3)=ID(6)
      IBEGT(4)=IREC
      ITIME(4)=IREC
      NEL=ID(35)
      SCLA=1./ID(44)
      K=129
      K=129
      IF (ICOPLANE.NE.5) THEN
         DO 40 I=1,NEL
            ELB(I)=ID(K)*SCLA
            IF (I.GT.1) THEN
               IF (ELB(I).LT.ELB(I-1)) ELB(I)=ELB(I)+360.0
            END IF 
            KDIR(I)=ID(K+1)
            KNDREC(I)=ID(K+2)+IREC
            K=K+3
 40      CONTINUE
      ELSE
C
C     AIRBORNE/DORADE
C
         DO 43 I=1,NEL
            KDIR(I)=1
            IF (I.GT.1) THEN
               KNDREC(I)=ID(K+2)+IREC+KNDREC(I-1)
            ELSE
               KNDREC(I)=ID(K+2)+IREC
            END IF
            K=K+3
 43      CONTINUE
      END IF
      IEL1=1
      IEL2=NEL
      ISD=SIGN(1.0,ELB(2)-ELB(1))
      J=ISD+0.5
      IF (ICOPLANE.NE.5) THEN
         DO 50 I=2,NEL
            DEI(I-J)=ABS(UNSCEL/(ELB(I)-ELB(I-1)))
 50      CONTINUE
      END IF
      IF (IRP.EQ.0 .OR. IRP.EQ.2) THEN
C
C     VARIABLE NUMBERS OF RANGE GATES ALLOWED FOR UF AND DORADE;
C     SET NG=MAXRNG FOR READING RAYS BACK IN IN BEAMIN
C
         NG=MAXRNG
      ELSE
C
C     CONSTANT NUMBERS OF RANGE GATES ASSUMED FOR FF;
C
         NG=ID(34)
      END IF
      
      RNOT=ID(31)+ID(32)*0.001
      DRG=ID(33)*0.001
      ZRAD=ID(46)*0.001
      XORG=ID(47)*0.01
      YORG=ID(48)*0.01
      IADJAZ=ID(49)*0.001*UNSCAZ
      ICOPLN=.FALSE.
      NIF=ID(75)
      MAXRD=ID(37)+ID(10)
      KNQ=0
      KRC=0
      J=76
      DO 60 I=1,NIF
         WRITE (CTEMP1,101)ID(J),ID(J+1)
 101     FORMAT(2A4)
         READ (CTEMP1,500)NAMF
 500     FORMAT(A8)
         CFAC(I)=ID(J+2) * 0.01
C
C        SAVE NYQUIST VELOCITY AND RADAR CONSTANT IF PRESENT
C
         IF(ITPFLDC(NAMF).EQ.3.AND.KNQ.EQ.0) KNQ=ABS(CFAC(I))*100.0+0.5
         IF(ITPFLDC(NAMF).EQ.1.AND.KRC.EQ.0) KRC=CFAC(I)*100.0+0.5
         CINFLD(I)=NAMF
         SCLIF(I,1)=ID(J+4)
         SCLIF(I,2)=1./SCLIF(I,1)
         J=J+5
 60   CONTINUE
      RETURN
      END
