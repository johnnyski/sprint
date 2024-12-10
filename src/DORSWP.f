      SUBROUTINE DORSWP(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE,REQTIME,ALTMEAN,TRCKMEAN,DRFTMEAN,PTCHMEAN,
     X     ROLLMEAN,TRCKMN,TRCKMX,ALTMN,ALTMX,ROTBEG,ROTEND,SPACMN,
     X     SPACMX,SPACMEAN,NRAYS,ISTAT,FLDDAT,RADNAM,FLTNUM,FLDNAM,
     X     SCALES,OFFSET,IFRST,NFLINP,TILT,ALAT1,ALON1)
C
C     READS IN A SWEEP OF DORADE BEAMS
C
      PARAMETER (NID=296,MAXFLD=8,MAXSKP=27,MXCNT=500,MAXEL=80)
      PARAMETER (MAXRNG=768)

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

      COMMON /UNITS/LUN,LSKP,FEETS
      COMMON /UNITSC/ IPROJ,LTAP,ISCI
      CHARACTER*4 IPROJ
      CHARACTER*8 LTAP,ISCI
      
      COMMON /INITV/ NRNG,RMIN,GATSPAC,IMIN,ISEC
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ
     X     ,RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL
      
      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD
      
      COMMON /BYTORD/ MBYTE
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      
      CHARACTER*8 RADNAM,FLTNUM,FLDNAM(MAXFLD)
      CHARACTER*8 TFIELD(2,MAXFLD),CTEMP,RFNAM,P10
      DIMENSION FLDDAT(MAXRNG,MAXFLD), SCALES(MAXFLD), OFFSET(MAXFLD)
      DIMENSION THNUM(MAXFLD),MTFIEL(MAXFLD)
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG
      DATA ATR /0.0174533/
      DATA JRH6,JRH7,IBAD /64,100,-32768/

      
      IDPTR  =76
      IFC    =0
      IREWND =0
      DIR    =0.0
      HEADMX =0.0
      HEADMN =1000.0
      ALTMX  =0.0
      ALTMN  =0.0
      TRCKSM =0.0
      ALTSUM =0.0
      DRFTSM =0.0
      PTCHSM =0.0
      ROLLSM =0.0
      ROTSUM =0.0
      SPACMX =0.0
      SPACMN =1000.
      SPACSM =0.0
      ROTKP  =-1000.


      IBEGRG=ID(31)*1000 + ID(32)
C     
C     LINE 100 IS THE START OF THE LOOP FOR READING A SWEEP'S WORTH OF BEAMS
C
      DO J=1,MAXFLD
         DO I=1,MAXRNG
            FLDDAT(I,J)=0.0
         END DO
      END DO
 100  CALL RDBEAM(RADNAM, REQTIME, IUN, IREWND, ISTAT, IVOL, IYR, 
     X     IMON, IDAY, IHR, IMIN, ISEC, MSEC, FLTNUM, NUMRADS, RADCON, 
     X     ITP, SCANRATE, NFLDDAT, ALON, ALAT, ALTMSL, ALTGND, PRESALT, 
     X     VNYDAT, RNGMX, NUMFREQ, NUMIPP, RMIN, GATSPAC, NRNG, ISWP, 
     X     JULDAY, AZ, EL, IRYSTAT, GNDSPDEW, GNDSPDNS, VERVEL, 
     X     HEADING, ROLL, PITCH, DRIFT, ROTANG, TILT, UAIR, VAIR, WAIR, 
     X     HEDCHGRT, PITCHGRT, FLDDAT, FLDNAM, SCALES, OFFSET, BAD, 
     X     FXANG)

      IF (ISTAT.NE.0) GOTO 200

      IF (ICOPLANE.EQ.5) THEN
         KST   =ID(37)
         IF (ROTKP.NE.-1000.) THEN
            SPAC=ROTANG-ROTKP
            IF (SPAC.LT.0.0) SPAC=(ROTANG+360.-ROTKP)
            SPACSM=SPACSM+SPAC
            IF (SPAC.GT.SPACMX) SPACMX=SPAC
            IF (SPAC.LT.SPACMN) SPACMN=SPAC
         END IF
         ROTKP=ROTANG
         IF (ROTANG.GT.360.0) ROTANG=ROTANG-360.0
         IF (ROTANG.LT.0.0)   ROTANG=ROTANG+360.0
C
C     CALCULATE THE TRACK-RELATIVE ROTATION AND TILT ANGLES
C
         XT=COS((ROTANG+ROLL)*ATR)*SIN(DRIFT*ATR)*COS(TILT*ATR)*
     X        SIN(PITCH*ATR) + COS(DRIFT*ATR)*SIN((ROTANG+ROLL)*ATR)*
     X        COS(TILT*ATR) - SIN(DRIFT*ATR)*COS(PITCH*ATR)*
     X        SIN(TILT*ATR)
         YT=-COS((ROTANG+ROLL)*ATR)*COS(DRIFT*ATR)*COS(TILT*ATR)*
     X        SIN(PITCH*ATR) + SIN(DRIFT*ATR)*SIN((ROTANG+ROLL)*ATR)*
     X        COS(TILT*ATR) + COS(PITCH*ATR)*COS(DRIFT*ATR)*
     X        SIN(TILT*ATR)
         Z =COS(PITCH*ATR)*COS(TILT*ATR)*COS((ROTANG+ROLL)*ATR) +
     X        SIN(PITCH*ATR)*SIN(TILT*ATR)
         THETAT=ATAN2(XT,Z)/ATR
         IF (THETAT.LT.0.0) THETAT=THETAT+360.0
         TILTT=ASIN(YT)/ATR
         IF (TILTT.LT.0.0)  TILTT =TILTT+360.0
         KOUT(1)=NINT(THETAT*JRH6)
         ROT=ROTANG
         KOUT(2)=HEADING*JRH7
         KOUT(3)=IHR
         KOUT(4)=IMIN
         KOUT(5)=ISEC
         KOUT(6)=NINT(TILTT*64.)
         KOUT(7)=JRH7
         ID(7)  =KOUT(3)
         ID(8)  =KOUT(4)
         ID(9)  =KOUT(5)
         KOUT(8) =NRNG
         KOUT(9) =NINT(ALAT*1000.)
         KOUT(10)=NINT(ALON*1000.)
         TRACK =HEADING+DRIFT
         IF (TRACK.GT.TRCKMX)   TRCKMX=TRACK
         IF (TRACK.LT.TRCKMN)   TRCKMN=TRACK
         IF (ALTGND .GT.ALTMX ) ALTMX =ALTMSL
         IF (ALTGND .LT.ALTMN ) ALTMN =ALTMSL

         ALTSUM=ALTSUM+PRESALT
         TRCKSM=TRCKSM+TRACK
         DRFTSM=DRFTSM+DRIFT
         PTCHSM=PTCHSM+PITCH
         ROLLSM=ROLLSM+ROLL
         
         IF (NRAYS.EQ.0) THEN
            ROTBEG=ROTANG
            ALAT1=ALAT
            ALON1=ALON
         END IF
         NRAYS=NRAYS+1
         IBEAM=IBEAM+1

         if (alats(1).eq.0.0) then
            orlat1=alat1
            orlon1=-alon1
            alon2=-alon
            call ll2xydrv(alat,alon2,xj,y0,orlat1,orlon1,288.4)
         else
            orlat1=alats(1)
            orlon1=-alons(1)
            alon2=-alon
            call ll2xydrv(alat,alon2,xj,y0,orlat1,orlon1,288.4)
         end if

C         
C     RANGE GATE SPACING PARAMETERS NEED TO BE CALCULATED FOR EACH
C     BEAM SINCE NUMBER OF GATES CAN CHANGE.
C
         NRG=NRNG
         IF (NRG.GT.MAXRNG) NRG=MAXRNG
         R0=RMIN*0.001
         IF (RNOTUS.NE.0.0) R0=R0+RNOTUS
         RG1=R0
         DRG=GATSPAC
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

         RJ1=RMIN
         IF (RNOTUS.NE.0.0) RJ1=RJ1+RNOTUS
         NGFH=NRNG
         IF (RUSR2.GT.0.0) THEN
            JL=NINT((RJ1-RG1)/DRG)
            JNG=MIN0(JL+NGFH,NGFH)
            IUNPAD=MAX0(-JL,0)
            IPUTAD=MAX0(JL,0)
            JNG=MIN0(JNG,NRG-IPUTAD)
         ELSE
            IF (RNOTUS.EQ.0.0 .AND. NINT(RJ1).NE.IBEGRG) THEN
               WRITE(*,130)RJ1,IBEGRG
 130           FORMAT(/,5X,'+++CHANGE IN FIRST GATE POS. RJ1=',
     X              F8.2,' IBEGRG=',F8.2,' +++')
               STOP
            END IF

            IF (NINT(GATSPAC).NE.ID(33) .AND. DRGUS.EQ.0.0) THEN
C
C     CHANGE IN RANGE GATE SPACING
C
            write(*,*)'***gatspac,id(33),drgus=',gatspac,id(33),drgus
               IDRGCHG=IDRGCHG+1
               NRAYS=NRAYS-1
               IBEAM=IBEAM-1
               NBDBM=NBDBM+1
               GOTO 100
            END IF
            IUNPAD=0
            IPUTAD=0
            JNG=NRG
         END IF
         J1=1+IPUTAD
         J2=J1+JNG-1
         

C
C     LOCATE FIELDS FOR THRESHOLDING
C
         IF (NTHRSH.GT.0) THEN
            DO I=1,NTHRSH
               DO J=1,NFLDDAT
                  THNUM(I)=-1
                  IF (TFIELD(2,I).EQ.FLDNAM(J)) THNUM(I)=J
               END DO
               IF (THNUM(I).EQ.-1) THEN
                  WRITE(*,60) TFIELD(2,I)
                  STOP
               END IF
            END DO
         END IF
C
C     LOCATE REQUESTED FIELDS
C
         IDPTR=76
         DO I=1,NFLDS
            INUM=-1
            DO J=1,NFLDDAT
               IEND=INDEX(IFIELD(I),' ')
               WRITE(CTEMP,13)FLDNAM(J)
 13            FORMAT(A7)
               IF (CTEMP(1:IEND-1).EQ.(IFIELD(I)(1:IEND-1)) .OR.
     X              IFIELD(I).EQ.'AZ' .OR. IFIELD(I).EQ.'EL')
     X              INUM=J
            END DO
            IF (INUM.EQ.-1) THEN
               WRITE(*,60) IFIELD(I)
 60            FORMAT(/,5X,'+++ FIELD ',A8,'NOT FOUND IN INPUT DATASET',
     X              '+++')
               WRITE(*,70)
 70            FORMAT(5X,'FIELDS PRESENT...')
               DO J=1,NFLDDAT
                  WRITE(*,80)FLDNAM(J)
 80               FORMAT(5X,A8)
               END DO
               STOP
            END IF
            IF (IFRST.EQ.1) THEN
               READ(IFIELD(I),400)ID(IDPTR),ID(IDPTR+1)
 400           FORMAT(2A4)
               IFC=IFC+1
               MTFIEL(IFC)=I
               ITYP=ITPFLDC(IFIELD(I))
               ID(IDPTR+2)=0
               IF (ITYP.EQ.1) THEN
                  IF (CFAC1.EQ.0.0) THEN
                     ID(IDPTR+2)=-1
                  ELSE IF (CFAC1.EQ.-32767.) THEN
                     WRITE(*,90)
 90                  FORMAT(/,5X,'+++NEED CALIB. INFO FOR DM FIELD+++')
                     STOP
                  ELSE
                     ID(IDPTR+2)=CFAC1*100.
                  END IF
               ELSE IF (ITYP.EQ.3) THEN
                  IF (VNYQ.EQ.0.0) THEN
                     ID(IDPTR+2)=VNYDAT*100.
                  ELSE
                     ID(IDPTR+2)=VNYQ*100.
                  END IF
               END IF
            END IF
            K=IUNPAD+1
            IF (FNUM.EQ.0 .OR. (FNUM.GT.0 .AND. RFNAM(1:8).NE.
     X           IFIELD(I))) THEN
               IF (IFIELD(I).EQ.'AZ') THEN
C     AZIMUTH OF BEAM RELATIVE TO TRUE NORTH (CAN BE USED IN SYNTHESIS)
                  IF (IFRST.EQ.1) ID(IDPTR+4)=64
                  DO 501 J=1,NRG
                     KOUT(KST+J)=IBAD
                     IF (J.LT.J1 .OR. J.GT.J2) GOTO 501
                     IF (J.GT.NRNG) GOTO 501
                     KOUT(KST+J)=NINT(AZ*ID(IDPTR+4))
                     K=K+1
 501              CONTINUE
               ELSE IF (IFIELD(I).EQ.'EL') THEN
C     ELEVATION OF BEAM (CAN BE USED IN SYNTHESIS)
                  IF (IFRST.EQ.1) ID(IDPTR+4)=100
                  DO 502 J=1,NRG
                     KOUT(KST+J)=IBAD
                     IF (J.LT.J1 .OR. J.GT.J2) GOTO 502
                     IF (J.GT.NRNG) GOTO 502
                     KOUT(KST+J)=NINT(EL*ID(IDPTR+4))
                     K=K+1
 502              CONTINUE
               ELSE
C     NORMAL FIELD
                  IF (IFRST.EQ.1) ID(IDPTR+4)=100
                  DO 500 J=1,NRG
                     KOUT(KST+J)=IBAD
                     IF (J.LT.J1 .OR. J.GT.J2) GOTO 500
                     IF (J.GT.NRNG) GOTO 500
C                     r=rg1+(j-1)*drg/1000.
C                     y=y0+sin(tiltt*atr)*r
C                     rgxz=r*cos(tiltt*atr)
                     kout(kst+j)=nint(-5.*cos((az-198.4)*atr)*
     X                    cos(el*atr)*100.)
c                     KOUT(KST+J)=NINT(FLDDAT(K,INUM)*100.)
                     K=K+1
 500              CONTINUE
               END IF
            ELSE
C
C     ANALYTIC FUNCTION
C
               CALL ANLACT(FNUM,P1,P2,P3,P4,P10,NRG,KST,IBAD,J1,
     X              J2,NRNG,ID(IDPTR+4),K,EL,TILTT,Y0,RG1,AZ,
     X              DRG,ICOPLANE,ROTANG)
            END IF
            KST=KST+NRG
            IDPTR=IDPTR+5
         END DO
C
C     WRITE THE BEAM TO DISK
C
         NLEN=NFLINP*NRG + ID(37)
         CALL WRRYDK(KPCK,KOUT,NST,LTMP,0,NLEN)
      END IF
C
C     GO READ NEXT BEAM
C
      IFRST=0
      GOTO 100

 200  CONTINUE

C
C     END OF SWEEP ; CALCULATE STATS
C
      ROTEND=ROTKP
      IF (NRAYS.GT.0) THEN
         ALTMEAN =ALTSUM/NRAYS
         TRCKMEAN=TRCKSM/NRAYS
         DRFTMEAN=DRFTSM/NRAYS
         PTCHMEAN=PTCHSM/NRAYS
         ROLLMEAN=ROLLSM/NRAYS
         SPACMEAN=SPACSM/NRAYS
      ELSE
         ALTMEAN =0.0
         TRCKMEAN=0.0
         DRFTMEAN=0.0
         PTCHMEAN=0.0
      END IF

      RETURN

      END
