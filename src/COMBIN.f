      SUBROUTINE COMBIN(IUPR,ILWR,IOB,IEL,IRNG,KZLV,IVV1,IVV2)

      INCLUDE 'SPRINT.INC'
      PARAMETER (MAXEL=80, IDIM=64/WORDSZ) 
      DIMENSION IUPR(1),ILWR(1),IOB(1),IVV2(IDIM)
      LOGICAL IS360
      COMMON/ADJUST/INFLD(10,3),SCLIF(10,2),NIF,IOFLD(10,3),SCLOF(10,2),
     X   NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,LOWAZ,IZAD,IS360,
     X   MINAZ,MAXAZ,METSPC(10),IWFLD(10),NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(10),CIOFLD(10)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X   NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,CFAC(10)
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      EQUIVALENCE (METSPC(1),METH),(METSPC(4),MXREST)
      CHARACTER*8 NAMQAL
      DATA IBAD/-32768/
      DATA ATR/0.017453293/
      DATA NAMQAL/'QUAL'/

      FRUP=IEL*SCLEL
      FRLO=1.-FRUP
      if (frup.lt.0.0) then
         write(*,*)'***frup=',frup
         write(*,*)'***iel,sclel=',iel,sclel
      end if
      if (frup.gt.1.0) then
         write(*,*)'***frup=',frup
         write(*,*)'***iel,sclel=',iel,sclel
      end if
      IF(FRUP.GE.0.5) THEN
C
C        CLOSER TO UPPER SCAN PLANE
C
         IUPL=1
         MXELD=IRNG*SIN(ABS(ELB(KPEL)-ELB(KPEL-ISD))*FRLO*ATR)
      ELSE
C
C        CLOSER TO LOWER SCAN PLANE
C
         IUPL=0
         MXELD=IRNG*SIN(ABS(ELB(KPEL)-ELB(KPEL-ISD))*FRUP*ATR)
      END IF
C
C        COMPUTE FINAL CARTESIAN ESTIMATE
C
      DO 20 I=1,NFLI
         IUPEST=IUPR(I)
         ILWEST=ILWR(I)
         IF(METH.EQ.2) GO TO 19
         L=IWFLD(I)
      IF(L.LT.0) THEN
         IUPEST=NINT(IUPEST*FRUP+ILWEST*FRLO)
         GO TO 20
      END IF
         IF(INFLD(L,3).NE.0.AND.INFLD(L,2).EQ.3) THEN
C
C           NORMALIZE VELOCITIES
C
      VNYQ=ABS(CFAC(L))
      IVIND=I
         VSCL=0.125
C         IF(AND(IUPEST,1).NE.0) THEN
C            MN=SHIFTR(AND(iupest,76B),1) - 16
C            velt=((iupest-1.)/2. - (16+mn))*2.0
C            VEL=(velt/64.)*VSCL
C            IUPEST=OR(IFIX(VEL),1)
C         END IF
C         IF(AND(ILWEST,1).NE.0) THEN
C            MN=SHIFTR(AND(ilwest,76B),1) - 16
C            velt=((ilwest-1.)/2. - (16+mn))*2.0
C            VEL=(velt/64.)*VSCL
C            ILWEST=OR(IFIX(VEL),1)
C         END IF
         

      END IF
         IF(ICEDAND(IUPEST,1).EQ.0.OR.ICEDAND(ILWEST,1).EQ.0) GO TO 19
C
C        GOOD ESTIMATES- PROCEED WITH INTERPOLATION
C
         IF(INFLD(L,3).NE.0.AND.INFLD(L,2).LE.2) THEN
C
C           10(LOG) FIELD TO BE CONVERTED TO LINEAR
C
            SFAC=0.1*SCLIF(L,2)
            IUPEST=ALOG(EXP(IUPEST*SFAC)*FRUP+
     X                  EXP(ILWEST*SFAC)*FRLO) * SCLIF(L,1) * 10.
         ELSE
C
C           NO CONVERSION
C
            IUPEST=NINT(IUPEST*FRUP+ILWEST*FRLO)
         END IF
         GO TO 20
   19    CONTINUE
C
C           SELECT THE CLOSEST POINT IF WITHIN RANGE
C
            IF(IUPL.EQ.0) IUPEST=ILWEST
            IF(MXELD.GT.MXREST) IUPEST=IBAD
   20 IUPR(I)=IUPEST
C
C        DERIVE FIELDS IF NECESSARY AND THRESHOLD FINAL RESULTS
C
      DO 30 I=1,NOF
      IF(CIOFLD(I).EQ.NAMQAL) THEN
         IOB(I)=IBAD
         IF(IUPR(IVIND).EQ.IBAD) GO TO 30
         CALL IVVUNP(IVV1,DF1,SUM1,SUMSQ1)
         CALL IVVUNP(IVV2,DF2,SUM2,SUMSQ2)
         IOB(I)=0
         CNT1=IFIX(DF1)
         CNT2=IFIX(DF2)
         DF1=DF1-CNT1
         DF2=DF2-CNT2
         IF(CNT1.EQ.0.0.OR.CNT2.EQ.0.0) GO TO 30
         IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.2 .OR. ICOPLANE.EQ.3 .OR.
     X        ICOPLANE.EQ.4) THEN
C
C     FOR 3-D INTERPOLATIONS
C
            CNT=CNT1+CNT2
            DIV=1./CNT
            AVG=(SUM1+SUM2)*DIV
            TEST=(SUMSQ1+SUMSQ2)*DIV-(AVG*AVG)
            IF(CNT.GT.1.0) TEST=(TEST*CNT)/(CNT-1.0)
         ELSE IF (ICOPLANE.EQ.1) THEN
C
C     FOR 2-D INTERPOLATION (IN COPLANES)
C
            CNT=CNT1
            DIV=1./CNT
            AVG=SUM1*DIV
            TEST=SUMSQ1*DIV - (AVG*AVG)
            IF(CNT.GT.1.0) TEST=(TEST*CNT)/(CNT-1.0)
         END IF
C
C+++Use standard deviation instead of variance
C+++    when forming the QUAL field.  6/89 cgm
C 

            IF(TEST.GT.0.0) TEST = SQRT(3.0*TEST)
            TEST = 1.0 - (TEST/VNYQ)
C                           
            TEST=AMAX1(TEST,-3.0)
            TEST=IFIX(TEST*100.0)
            FRAC=(FRLO*DF1)**2 + (FRLO*(1.0-DF1))**2
     X         + (FRUP*DF2)**2 + (FRUP*(1.0-DF2))**2
            FRAC=AMAX1(0.0,FRAC-0.001)
            FRAC=SIGN(FRAC,TEST)
            IOB(I)=(TEST+FRAC)*SCLOF(I,1)
         GO TO 30
      END IF
         J=IABS(IOFLD(I,2))
         IUPEST=IUPR(J)
         IOB(I)=IUPEST
         IF(IOFLD(I,2).GE.0.OR.IUPEST.EQ.IBAD) GO TO 30
C
C     DERIVE REFLECTIVITY FIELD FROM POWER FIELD
C
      L=IWFLD(J)
      IF ((IRNG*SCLRNG).GT.0.0) THEN
       IOB(I)=(CFAC(L) + (IUPEST*SCLIF(L,2)) + 20.*ALOG10(IRNG*SCLRNG))
     X        * SCLOF(I,1)
      ELSE
         IOB(I)=IBAD
      END IF
   30 CONTINUE
      RETURN
      END
