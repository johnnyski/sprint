      SUBROUTINE TRPARVOL(ICART,ICTAB,NPREC,ICOB,NDX,NDY,NDZ,YRTAB,
     X     MXCRT,IBLV,NRCBF,NST,DASANG,IFLAT,NUMFILT,XRTAB,IVREF)
C
C     HANDLES THE INTERPOLATION OF A VOLUME OF AIRBORNE DATA
C
      INCLUDE 'SPRINT.INC'
      PARAMETER  (MAXEL=80,MAXIN=8500,NIOB=85000,MAXFLD=8,
     X     NRCBF2=400000,IDIM=64/WORDSZ,MAXPLN=65536,NID=296,
     X     MAXBMS=1000,IVDIM=(NRCBF2+1)/(WORDSZ/8))
      LOGICAL IS360,IFN360
      DIMENSION ICART(NPREC),ICOB(IDIM,2,NDX,NDY,NDZ),YRTAB(MXCRT,2),
     X     IBLV(IDIM,4,NRCBF,2),IDATL(MAXFLD),IDATH(MAXFLD),
     X     IVREF(IVDIM,2),IDATCB(MAXFLD),ICTAB(4),
     X     KZBUF(2,2),IBKNT(4,2),IVV2(IDIM),XRTAB(MXCRT,2)
      COMMON /NGATES/ NRGC,NRGP
      COMMON /IDBLK/ID(NID)
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /IO/ KPCK(NIOB),IRAY(MAXIN,3),KZ(3),KZC,KZP,IDSNEW,ITMDIF,
     X     ITIME(4),IBEGT(4),NSBM,NRDOPT,ILSTREC
      COMMON /SCNDAT/ ELB(MAXEL),DEI(MAXEL),KDIR(MAXEL),KNDREC(MAXEL),
     X     NEL,IEL1,IEL2,KEL,KPEL,ISD,ELTOL,RNOT,DRG,NG,MAXRD,CFAC(10)
      COMMON/ADJUST/INFLD(10,3),SCLIF(10,2),NIF,IOFLD(10,3),SCLOF(10,2),
     X     NOF,SCLAZ,SCLRNG,SCLEL,UNSCAZ,UNSCRG,UNSCEL,LOWAZ,IZAD,IS360,
     X   MINAZ,MAXAZ,METSPC(10),IWFLD(10),NFLI,SCLNYQ,UNSNYQ
      COMMON /FNAMES/ CINFLD(10),CIOFLD(10)
      CHARACTER*8 CINFLD, CIOFLD
      COMMON /SETAZM/ USAZ1,USAZ2,USGAP,USINC,IFOR36,IFORDR
      COMMON /RHIS/ IRHICRS,LOWAZ2,MAXAZ2,MINAZ2,ICART2(MAXPLN),
     X     ICTAB2(4),KZV(3),IDSV
      COMMON /COPLAN/ ICOPLN,BASAD
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG
      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /CINPC/ NMRAD,ITAP
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP


      LOGICAL ICOPLN
      DATA ATR /0.0174533/
      DATA IDATL/MAXFLD*0/
      DATA IDATH/MAXFLD*0/
      DATA IDATCB/MAXFLD*0/
      DATA MASK/ O'100000'/

      IRHICRS=0
      ORLAT=ALATS(1)
      ORLON=ALONS(1)
C
C     CREATE A BIT MASK USED TO INTIALIZE ICOB. THE MASK IS
C     32768 (DECIMAL) REPEATED THROUGH THE LENGTH OF THE WORD
C
      INITCB = 0
      DO I=1,(WORDSZ/16)
         INITCB = ICEDOR(INITCB,ICEDSHFT(MASK,(I-1)*16))
      END DO
C     360 SCANS ONLY
      ICROSS=IZAD/2
      MAXEND= USGAP * UNSCAZ

      XORTR=X1
      ZORTR=Z1
      DO 3 K=1,NDZ
         DO 2 J=1,NDY
            DO 1 I=1,NDX
               DO 19 L=1,IDIM
                  ICOB(L,1,I,J,K)=INITCB
                  ICOB(L,2,I,J,K)=INITCB
 19            CONTINUE
 1          CONTINUE
 2       CONTINUE
 3    CONTINUE
      
      DO 5 I=1,NY
         YRTAB(I,1)=Y1+(I-1)*YD
         YRTAB(I,2)=YRTAB(I,1)**2
 5    CONTINUE
      
      YDI=1./YD

C
C     LOOP OVER ALL SWEEPS IN VOLUME
C
      NSWPS=ID(35)
      ATR=ATAN(1.)/45.
      RTA=1./ATR
      KL=2
      KH=1
      IDS=0
      IBKNT(2,1)=0
      IBKNT(2,2)=0
      KEL=IEL1-1
      DO 200 ISWP=1,NSWPS
         KEL=KEL+1
         KZP=1
         KZC=2
         IFN360=.TRUE.
         NRDOPT=1
         CALL BEAMIN
         IF (NSBM.NE.0) GOTO 704
         NBEAMS=ID(131+(ISWP-1)*3)
         DRIFT=REAL(ID(130+(ISWP-1)*3))/REAL(ID(44))
         ALAT=ALATS(ISWP)
         ALON=ALONS(ISWP)

         ALON=-ALON
         ORLON2=-ORLON
         CALL LL2XYDRV(ALAT,ALON,X,YPS,ORLAT,ORLON2,ANGXAX)
         IF (ISWP.NE.NSWPS) THEN
            ALAT=ALATS(ISWP+1)
            ALON=ALONS(ISWP+1)
            ALON=-ALON
            ORLON2=-ORLON
            CALL LL2XYDRV(ALAT,ALON,X,YPE,ORLAT,ORLON2,ANGXAX)
         END IF
         IDS=IDSNEW
         KSAV=KL
         KL=KH
         KH=KSAV
         JL=2.+(IDS*0.5)
         KZLO=IDS*32767
         KZHI=KZLO
C
C     LOOP OVER ALL RAYS IN SWEEP
C
         DO 100 NRAY=2,NBEAMS
            KSAV=KZC
            KZC=KZP
            KZP=KSAV
            CALL BEAMIN
            IF (NSBM.NE.0) GOTO 704
            NRGP=IRAY(8,KZP)
            NRGC=IRAY(8,KZC)
            ALAT=IRAY(9,KZP)/1000.
            ALON=IRAY(10,KZP)/1000.
            ALON=-ALON
            ORLON2=-ORLON
            CALL LL2XYDRV(ALAT,ALON,X,YP1,ORLAT,ORLON2,ANGXAX)
            TILT=IRAY(6,KZP)/64.
            IF (NRAY.EQ.2 .AND. ISWP.NE.NSWPS) THEN
C
C     INITIALIZATION FOR LOCATIONS IN FRONT OF THIS SCAN
C
               CALL RSCART(ICART,NRCBF,ICTAB,KZ(KZP),IDS,JL,IC,IHI,
     X              IBKNT(1,KH),KZHI,IS360,NST,IABOVE)

               IF (NST.NE.0) GOTO 800
            END IF
            IF (IS360) ICSAV=IC
            IF (NRAY.EQ.2 .AND. IBKNT(2,KL).NE.0) THEN
C
C     INITIALIZATION FOR LOCATIONS BEHIND THIS SCAN
C
               CALL SCNSET(KZBUF(1,KL),IBKNT(1,KL),IBLV(1,1,1,KL),NRCBF,
     X              KZ(KZP),IDS,JL,ILO,LPMN,LPMX,KZLO,IS360,IZAD,ICROSS,
     X              NST,IBELOW)
               IF (NST.NE.0) GOTO 800
            END IF
            IF (IS360)ILSAV=ILO
            IF (ISWP.NE.NSWPS) THEN
C
C     INTERPOLATION OF LOCATIONS FORWARD OF THIS SCAN
C
 35            CALL IGETCP2(ICART(IC),KZLV,IX,IZ)
               KZHI=KZLV
               IF(IS360) THEN
C     360 SCANS ONLY
                  IF(IABS(KZLV-KZ(KZC)).GT.ICROSS)
     X                 KZ(KZC)=KZ(KZC)+ISIGN(IZAD,KZLV-KZ(KZC))
                  IF(IABS(KZLV-KZ(KZP)).GT.ICROSS)
     X                 KZ(KZP)=KZ(KZP)+ISIGN(IZAD,KZLV-KZ(KZP))
                  ITST=(KZ(KZP)-KZLV)*IDS
                  IF(ITST.GT.0) GO TO 80
               END IF
               
               ITST=(KZ(KZC)-KZLV)*IDS
               IF (ITST.LT.0 .AND. ISWP.GT.1) GOTO 55
               IF (ITST.LT.0 .AND. ISWP.EQ.1) GOTO 80
               X=(IX-1)*XD
               Z=(IZ-1)*ZD
               RGXZSQ=(X+XORTR)**2. + (Z+ZORTR)**2.
               RGXZ=SQRT(RGXZSQ)
               DIST=RGXZ
               DELY=DIST*TAN(TILT*ATR)
               IY1=((YP1+DELY) - YRTAB(1,1))*YDI + 2.
               IF (ISWP.NE.NSWPS) THEN
                  IY2=(((YPE-YPS)*2)+YP1+DELY - YRTAB(1,1))*YDI + 1.
               ELSE
                  IY2=NY
               END IF
               IF (IY1.EQ.0 .AND. IY2.EQ.0) GOTO 38
               IY1=MAX0(IY1,1)
               IY2=MIN0(IY2,NY)
               IF (IY1.GT.IY2) GOTO 38

               DO 37 I=IY1,IY2
                  RNG=RGXZ/COS(TILT*ATR)
                  IRNG=RNG*UNSCRG+0.5
                  RNG=IRNG*SCLRNG
                  IG=(RNG-RNOT)/DRG+1.0
                  IF (IG.LE.0 .OR. IG.GE.NRGC .OR. IG.GE.NRGP) GOTO 37
                  MN=-25
                  CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,0,
     X                 IBLV(1,4,IHI,KH),NST,MN,NUMFILT,KZ)
                  IF (NST.NE.0) GOTO 800
                  IF (WORDSZ.EQ.32) THEN
                     CALL IPUT16(IBLV(1,1,IHI,KH),
     X                    NINT((YP1+DELY)*1000.),I)
                     IBLV(2,1,IHI,KH)=ICART(IC)
                  ELSE IF (WORDSZ.EQ.64) THEN
                     CALL IPUT16(IWRD,NINT((YP1+DELY)*1000.),I)
                     IBLV(1,1,IHI,KH)=ICEDOR(IWRD,ICEDSHFT(ICART(IC),
     X                    -32))
                  END IF
                  CALL IPKDAT(IBLV(1,2,IHI,KH),IDATH,NFLI)
                  ILOC=(IHI-1)/(WORDSZ/8) + 1
                  INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - IHI)
                  CALL IPUT8(IVREF(ILOC,KH),INDX,MN)
C                  IVREF(IHI,KH)=MN
                  IHI=IHI+IDS
                  IF (IHI.LE.0 .OR. IHI.GT.NRCBF) GOTO 707
 37            CONTINUE
 38            CONTINUE
               IC=IC+IDS
               IF (IS360) THEN
                  IF (IC.LE.0) IC=ICTAB(2)
                  IF (IC.GT.ICTAB(2)) IC=1
                  IF (IC.EQ.ICSAV) GOTO 45
               END IF
               IF (IC.GT.0 .AND. IC.LE.ICTAB(2))GOTO 35
            END IF
 45            CONTINUE
               KZHI=IDS*32767

 55            CONTINUE
C
C     INTERPOLATE LOCATIONS BELOW THE CURRENT PPI SCAN AND WRITE 
C     THEM TO OUTPUT BUFFERS
C
               IF (IBELOW.EQ.1) THEN
                  CALL IGETCP(IBLV(1,1,ILO,KL),KZLV,IX,IZ)
                  KZLO=KZLV
                  IF(IS360) THEN
C     360 SCANS ONLY
                     IF(IABS(KZLV-KZ(KZC)).GT.ICROSS)
     X                    KZ(KZC)=KZ(KZC)+ISIGN(IZAD,KZLV-KZ(KZC))
                     IF(IABS(KZLV-KZ(KZP)).GT.ICROSS)
     X                    KZ(KZP)=KZ(KZP)+ISIGN(IZAD,KZLV-KZ(KZP))
                     ITST=(KZ(KZP)-KZLV)*IDS
                     IF(ITST.GT.0) GO TO 80
                  END IF
                  ITST=(KZ(KZC)-KZLV)*IDS
                  IF (ITST.LT.0) GOTO 80
                  
                  CALL IGET16(IBLV(1,1,ILO,KL),IYP2,IY)
                  X=(IX-1)*XD
                  Z=(IZ-1)*ZD
                  RGXZ=SQRT((X+XORTR)**2 + (Z+ZORTR)**2)
                  IRNG=RGXZ*UNSCRG+0.5
                  IRNG=IRNG/COS(TILT*ATR)
                  RNG=IRNG*SCLRNG
                  IG=(RNG-RNOT)/DRG+1.0
                  IF (IG.LE.0 .OR. IG.GE.NRGP .OR. IG.GE.NRGC) GOTO 48
                  DELY=(IRNG*SCLRNG)*SIN(TILT*ATR)
                  IDISTY=((IY-1)*YD + YORTR - IYP2/1000.)*100.
                  DISTY=IDISTY/100.
                  YP2=IYP2/1000.
                  IF (((IY-1)*YD + YORTR).GT.(YP1+DELY)) GOTO 48
                  CALL IGTDAT(IBLV(1,2,ILO,KL),IDATL,NFLI)
C     MN=IVREF(ILO,KL)
                  ILOC=(ILO-1)/(WORDSZ/8) + 1
                  INDX=(WORDSZ/8) - (ILOC*(WORDSZ/8) - ILO)
                  CALL IGET8(IVREF(ILOC,KL),INDX,MN)
                  CALL TRPD(IDATH,IEL,IRNG,KZLV,IDATL,1,IVV2,NST,MN,
     X                 NUMFILT,KZ)
                  IF (NST.NE.0) GOTO 800
C
C     CALCULATE THE IMPORTANT WEIGHTING FACTOR BASED ON Y DISTANCE
C     BETWEEN TWO SWEEPS
                  IEL=(DISTY/((YP1+DELY)-YP2))*UNSCEL
                  IF (IEL.LT.0) THEN
                   WRITE(*,*)'***IEL,DISTY,YP1,YP2,IY,DELY,TILTP,IRNG=',
     X                 IEL,DISTY,YP1,YP2,IY,DELY,TILTP,IRNG
                   STOP
                  END IF
                  IF (IEL.GT.(1.0*UNSCEL)) THEN
                     WRITE(*,*)'***(YP1+DELY),((IY-1)*YD + YORTR)=',
     X                    (YP1+DELY),((IY-1)*YD + YORTR)
                     WRITE(*,*)'***DISTY,YP2,YP1=',DISTY,YP2,YP1
                     WRITE(*,*)'***IEL=',IEL
                     STOP
                  END IF
                  CALL COMBIN(IDATH,IDATL,IDATCB,IEL,IRNG,KZLV,
     X                 IBLV(1,4,ILO,KL),IVV2)
                  IF (IX.LE.0 .OR. IY.LE.0 .OR. IZ.LE.0) THEN
                     WRITE(*,*)'***IX,IY,IZ=',IX,IY,IZ
                     STOP
                  END IF
                  CALL IPKDAT(ICOB(1,1,IX,IY,IZ),IDATCB,NOF)
 48               ILO=ILO+IDS
                  IF(ILO.GE.LPMN.AND.ILO.LE.LPMX) GO TO 55
                  IF (IS360) THEN
                     IF (ILO.NE.ILSAV) THEN
                        IF (ILO.LT.LPMN) ILO=LPMX
                        IF (ILO.GT.LPMX) ILO=LPMN
C
C     GO AND READ NEXT POINT BELOW CURRENT SCAN
C
                        GOTO 55
                     END IF
                  END IF
               END IF
               KZLO=IDS*32767
C
C     NO MORE LOCATIONS BELOW THE CURRENT SCAN
C
 80            CONTINUE
 100        CONTINUE
C
C     END OF ELEVATION SCAN
C
            IHI=IHI-IDS
            IF (IHI.LE.0 .OR. IHI.GT.NRCBF) THEN
               WRITE(*,*)'***IHI ERROR***.IHI,IDS=',IHI,IDS
C               STOP
            END IF
            IBKNT(2+IDS,KH)=IHI
            IBKNT(2,KH)=1
            IBKNT(4,KH)=1
            CALL IGETCP(IBLV(1,1,IHI,KH),KZBUF(JL,KH),IX,IZ)
            K=1+(2-JL)*(NRCBF-1)
            CALL IGETCP(IBLV(1,1,K,KH),KZBUF(3-JL,KH),IX,IZ)
 200     CONTINUE
C
C     END OF VOLUME SCAN
C
         NST=0
         RETURN

 704     CALL TPQERX(312,1)
         GOTO 800
 707     CALL TPQERX(315,0)
         GOTO 800

 800     CONTINUE
         NST=1
         RETURN


         END

               
