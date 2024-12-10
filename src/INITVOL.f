      SUBROUTINE INITVOL(IPROJ)
C     
C     INITIALIZE OUTPUT VOLUME SCAN HEADER
C     
      PARAMETER (NID=296,MAXRNG=768,MAXFLD=8)
      COMMON /IO/KPCK(85000),KOUT(8500),IBUF(8500),NBUF(2125,4),
     X     IZ8(17),ILSTREC
      COMMON /IDBLK/ID(NID)

      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /CINPC/ NMRAD,ITAP
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP

      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ
     X     ,RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL

      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD

      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /INITV/ NRNG,RMIN,GATSPAC,IMIN,ISEC
      DIMENSION ISCAN(2)
      CHARACTER*4 IPROJ
      CHARACTER*8 CTEMP1,IPROG,NAMTIM
      INTEGER CVMGP
      DATA ISCAN/'PP','CO'/
      DATA IPROG,IBL,IBAD/'CART.0  ','  ',-32768/
      DATA NAMTIM/'TIME'/
C     
      INORM(I)=CVMGP(I-65536,I,I-32768)
C     
C     IF UNIVERSAL FORMAT
C     
      IF (IRP.EQ.0) THEN
         IPTD=IBUF(5)
         IPTF=IBUF(IPTD+4)
         NRG=IBUF(IPTF+5)
         IF (NRG.GT.MAXRNG) NRG=MAXRNG
         IRGFG=INORM(IBUF(IPTF+2))
         IADFG=INORM(IBUF(IPTF+3))
         R0=IRGFG+IADFG*0.001
         IF (RNOTUS.NE.0.0) R0=R0+RNOTUS
         RG1=R0
         DRG=IBUF(IPTF+4)*.001
C     
C     IF RP-7 PROCESSOR
C     
      ELSE IF (IRP.EQ.1) THEN
         NRG=IBUF(15)
         IF (NRG.GT.MAXRNG) NRG=MAXRNG
         IRGFG=INORM(IBUF(12))
         IADFG=INORM(IBUF(13))
         R0=IRGFG+IADFG*0.001
         IF (RNOTUS.NE.0.0) R0=R0+RNOTUS
         RG1=R0
         DRG=IBUF(14)*.001
      ELSE IF (IRP.EQ.2) THEN
         NRG=NRNG
         IF (NRG.GT.MAXRNG) NRG=MAXRNG
         R0=RMIN*0.001
         IF (RNOTUS.NE.0.0) R0=R0+RNOTUS
         RG1=R0
         DRG=GATSPAC*.001
      END IF         
      IF (DRGUS.GT.0.0) DRG=DRGUS
      IF (RUSR2.GT.0.0) THEN
         FRAC=AMOD(R0,DRG)
         J=(RUSR1-FRAC)/DRG + 0.999
         IF (J.LT.0) CALL CHKMSG(5,0)
         RG1=J*DRG+FRAC
         NRG=(RUSR2-RG1)/DRG +1.0
         IF(NRG.GT.MAXRNG) NRG=MAXRNG
         RG2=RG1+(NRG-1)*DRG
      ENDIF
      CALL UNCODE(KDAY,ID(1),ID(2),ID(3))
      IF (IRP.EQ.0) THEN
         ID(5)=IBUF(30)
         ID(6)=IBUF(31)
      ELSE IF (IRP.EQ.1) THEN
         ID(5)=IBUF(8)
         ID(6)=IBUF(9)
      ELSE IF (IRP.EQ.2) THEN
         ID(5)=IMIN
         ID(6)=ISEC
      ELSE
         WRITE(*,*)'***UNKNOWN PROCESSOR TYPE IN INITVOL***'
         STOP
      ENDIF 
      WRITE (CTEMP1,500)ITAP
      READ (CTEMP1,10)ID(11),ID(12),ID(13)
      
      WRITE (CTEMP1,500)NMRAD
      READ (CTEMP1,10)ID(14),ID(15),ID(16)
 10   FORMAT (3A2)
      
      READ (IPROG,20)ID(17),ID(18),ID(19),ID(20)
 20   FORMAT (4A2)
      
 500  FORMAT(A4)
      READ (IPROJ,30)ID(21),ID(22)
 30   FORMAT (2A2)
      
      ID(23)=IBL
      ID(24)=IBL
      ID(25)=ID(11)
      ID(26)=ID(12)
      ID(27)=ID(13)
      ID(28)=IBL
      ID(29)=IBL
      ID(30)=IBL
      ID(31)=INT(RG1)
      ID(32)=NINT((RG1-ID(31))*1000.0)
      ID(33)=NINT(DRG*1000.0)
      ID(34)=NRG

      ID(37)=10
      ID(44)=100
      ID(45)=IBAD
      ID(50)=ISCAN(1)
      ID(51)=0
      IF((IBUF(35).EQ.2 .AND. IRP.EQ.0)) THEN
C     
C     COPLANE SCANNING MODE
C     
         ID(50)=ISCAN(2)
         ID(51)=IBUF(50)
      ELSE IF (IBUF(26).EQ.2 .AND. IRP.EQ.1) THEN
         ID(50)=ISCAN(2)
         ID(51)=IBUF(50)
      END IF
      IF (IRP.EQ.0) THEN
         DO 40 L=19,24
            IF (IBUF(L).EQ.IBAD) IBUF(L)=0
 40      CONTINUE
         IF (IABS(IBUF(19)).GT.360) IBUF(19)=0
         ID(65)=IBUF(19)*100
         IF (IABS(IBUF(20)).GT.60) IBUF(20)=0
         ID(66)=IBUF(20)*100
         IF (IABS(IBUF(21)).GT.3840) IBUF(21)=0
         ID(67)=IBUF(21)/64*100
         IF (IABS(IBUF(22)).GT.360) IBUF(22)=0
         ID(68)=IBUF(22)*100
         IF (IABS(IBUF(23)).GT.60) IBUF(23)=0
         ID(69)=IBUF(23)*100
         IF (IABS(IBUF(24)).GT.3840) IBUF(24)=0
         ID(70)=IBUF(24)/64*100
      ELSE IF (IRP.EQ.1) THEN
         IF (IBUF(59).EQ.IBAD) IBUF(59)=0
         IF (IBUF(60).EQ.IBAD) IBUF(60)=0
         IF (ABS(IBUF(59)/728.17778).GT.360) IBUF(59)=0
         ID(65)=IBUF(59)/728.17778*100
         ID(66)=0
         ID(67)=0
         IF (ABS(IBUF(60)/364.08889).GT.360) IBUF(60)=0
         ID(68)=IBUF(60)/364.08889*100
         ID(69)=0
         ID(70)=0
      ELSE IF (IRP.EQ.2) THEN
         ID(65)=0
         ID(66)=0
         ID(67)=0
         ID(68)=0
         ID(69)=0
         ID(70)=0
      END IF
      
      ID(75)=NFLDS
      IF(IFIELD(NFLDS).EQ.NAMTIM) ID(75)=NFLDS-1
      IF (IRP.EQ.2 .OR. IRP.EQ.0) THEN
C
C     SET NUMBER OF DATA VALUES BASED ON MAX RANGE GATES FOR DORADE/UF DATA
C
         NFLINP=NFLDS
         IF(IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
         ID(10)=NFLINP*MAXRNG

      ELSE
         ID(10)=ID(34)*ID(75)
      END IF
      RETURN
      END
