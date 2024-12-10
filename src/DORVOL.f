      SUBROUTINE DORVOL(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE)
C     
C     THIS SUBROUTINE READS ONE VOLUME SCAN IN DORADE-7 FORMAT
C     AND OUTPUTS IT TO DISK IN A SIMPLIFIED FORMAT.
C
C     NUFST
C     =0    NO END YET (NORMAL EXECUTION)
C     =1    PAST ENDING TIME
C     =2    END OF TAPE
C     
C     ICOPLANE
C     =0    R,A,E -> CARTESIAN
C     =1    R,A,C -> COPLANE (ANGLES OF DATA)
C     =2    R,A,C -> COPLANE (USER SPECIFIED COPLANE ANGLES)
C     =3    R,A,C -> CARTESIAN
C     =4    RHI   -> CARTESIAN
C     =5    AIR   -> CARTESIAN
C     
C     ANGXAX = ANGLE OF X AXIS FROM TRUE NORTH (USUALLY 90.0)
C     BASANG = ANGLE OF Y AXIS FROM TRUE NORTH; USED IN COPLANE SCANS
C     DASANG = ANGLE OF Y AXIS FROM BASELINE OF RADARS; USED IN COPLANE SCANS
C     
C     FOR NON-COPLANE SCANS, BASANG AND DASANG WON'T BE USED.
C     FOR COPLANE SCANS WHERE THE USER IS INTERPOLATING TO COPLANES,
C     BASANG AND DASANG ARE THE SAME. FOR COPLANE SCANS WHERE THE USER
C     IS INTERPOLATING DIRECTLY TO A 3-D CARTESIAN COORD. SYSTEM, THEY
C     MAY BE DIFFERENT.
C     
      
      
      
C     
      PARAMETER (NID=296,MAXFLD=8,MAXSKP=27,MXCNT=500)
      PARAMETER (MAXEL=80,MAXIN=8500,NIOB=85000,MAXRNG=768)
      DIMENSION JPCK(1),ELSCAN(1),ISIDE(MAXFLD)
      DIMENSION TLIMITS(2,MAXFLD),ITHR(MAXFLD+1)
      DIMENSION CTDBM(MXCNT),CTDBMXH(MXCNT),CTDBMXV(MXCNT)
      CHARACTER*8 KRD(10),RFNAM,P10
      CHARACTER*8 TFIELD(2,MAXFLD)
      COMMON /IO/KPCK(85000),KOUT(8500),IBUF(8500),NBUF(2125,4),
     X     IZ8(17),ILSTREC
      COMMON /IDBLK/ID(NID)
      COMMON /FXTABL/ IFXTAB,ISKIP,IACCPT,FTABLE(MAXSKP),ITRAN
      
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
      
      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ
     X     ,RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL
      
      COMMON /CPRO/KDAY,KBTIM,KETIM,IROV,ISWMTH,FXSTOL
      COMMON /CPROC/ IREORD(3)
      CHARACTER*1 IREORD
      
      CHARACTER*7 RADNAM,FLTNUM,FLDNAM(MAXFLD)
      DIMENSION FLDDAT(MAXRNG,MAXFLD), SCALES(MAXFLD), OFFSET(MAXFLD)

      COMMON /BYTORD/ MBYTE
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      COMMON /INITV/ NRNG,RMIN,GATSPAC,IMIN,ISEC
      LOGICAL IELCHK,ILSCHK
      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG

      DIMENSION ALTMEAN(MAXEL), TRCKMEAN(MAXEL),DRFTMEAN(MAXEL)
      DIMENSION PTCHMEAN(MAXEL), ROLLMEAN(MAXEL)
      CHARACTER*3 KOMM
      CHARACTER*8 INP,IOUT,ICRT,IEMPTY,NAMTIM
      DATA INP,IOUT,ICRT/'INPUT','OUTPUT','GRID'/
      DATA IEMPTY /'-9999'/
      DATA ITHR/9*0/
C     
C     OVERFLOW COUNTER
C     
      DATA NAMTIM/'TIME'/
C     THE FOLLOWING IS THE DEGREES TO RADIANS CONVERSION
      DATA DTR /0.0174533/

      IF (IROV.EQ.-1) THEN
         ICOMBN=1
      ELSE IF (IROV.EQ.2) THEN
         ICOMBN=2
      ELSE
         ICOMBN=0
      END IF

      ILSCHK=.FALSE.
 2    IFRST  =1
      IDRGCHG=0
      INRNG  =0
      IEOF   =0
      IFLGBAS=0
      BASANG =ANGXAX-90.0
      IPTR   =129
      ELTOL  =ELTUS
      NFLINP=NFLDS
      IF(IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
      ALATS(1)=0.0
      ALONS(1)=0.0
      
C     
C     CALCULATE RADAR COORDINATES IN ROTATED (IF ROTATED) COORD. SYS.
C     
      IF (ANGXAX.NE.90.0) THEN
         DHETA=(ANGXAX-90.0)*DTR
         XORR=FLOAT(ID(47))/100.*COS(DHETA) - FLOAT(ID(48))/100.
     X        *SIN(DHETA)
         YORR=FLOAT(ID(47))/100.*SIN(DHETA) + FLOAT(ID(48))/100.
     X        *COS(DHETA)
         ZORR=FLOAT(ID(46))/1000.
      ELSE
         XORR=FLOAT(ID(47))/100.
         YORR=FLOAT(ID(48))/100.
         ZORR=FLOAT(ID(46))/1000.
      END IF
      
C     
C     CHECK TO SEE IF ALL COMMONS ARE FILLED
C     
      IF (ITAP.NE.IEMPTY) GOTO 5
      PRINT 105,INP
      STOP
 5    CONTINUE
      IF (LTAP.NE.IEMPTY) GOTO 10
      PRINT 105,IOUT
      STOP
 10   CONTINUE
 20   CONTINUE
      IF (ISCI.NE.IEMPTY) GOTO 25
      PRINT 105,NNFO
      STOP
 25   CONTINUE
      IF (ICRTST.GE.0) GOTO 60
      PRINT 105,ICRT
      STOP
C     
C     OTHER CHECKS GO HERE
C     
 105  FORMAT(5X,'+++  ',A8,' COMMAND MUST APPEAR BEFORE THE PROCESS',
     X     ' COMMAND  +++')
 60   CONTINUE
 200  CONTINUE
C     
C     START      - FIND VOLUME => TRANSLATE
C     
C     
C     INITIALIZE THRESHOLD COUNTERS
      DO 23 I=1,MAXFLD+1
         ITHR(I)=0
 23   CONTINUE


      NSWPS=0
      LTMP=1
      WRITE(LTMP) LTMP
      REWIND LTMP
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
      REQTIME=0.0
      IF (NMRAD.EQ.'ELDA') THEN
         RADNAM='TA-ELDR'
      ELSE IF (NMRAD.EQ.'ELDF') THEN
         RADNAM='TF-ELDR'
      ELSE
         RADNAM=NMRAD
      END IF
C
C     GET TO RIGHT VOLUME (BY TIME)
C
 50   CALL RDBEAM(RADNAM, REQTIME, IUN, IREWND, ISTAT, IVOL, IYR, 
     X     IMON, IDAY, IHR, IMIN, ISEC, MSEC, FLTNUM, NUMRADS, RADCON, 
     X     ITP, SCANRATE, NFLD, ALON, ALAT, ALTMSL, ALTGND, PRESALT, 
     X     VNY2, RNGMX, NUMFREQ, NUMIPP, RMIN, GATSPAC, NRNG, ISWP, 
     X     JULDAY, AZ, EL, IRYSTAT, GNDSPDEW, GNDSPDNS, VERVEL, 
     X     HEADING, ROLL, PITCH, DRIFT, ROTANG, TILT, UAIR, VAIR, WAIR, 
     X     HEDCHGRT, PITCHGRT, FLDDAT, FLDNAM, SCALES, OFFSET, BAD, 
     X     FXANG)
      
 65   IF (ISTAT.EQ.2) THEN
         IFTIM=10000*IHR + 100*IMIN + ISEC
         IF (IFTIM.LT.KBTIM) THEN
            PRINT 230, KDAY,IFTIM
 230        FORMAT(' +++  VOLUME SKIPPED  -  DAY: ',I6,
     X           5X,'BEGINNING TIME: ',I6,'  +++')
            GOTO 50
         ELSE IF (IFTIM.GT.KETIM) THEN
            PRINT 233, IUN,IFTIM,KETIM
 233        FORMAT(/1X,'+++  UNIT: ',I2,5X,'INITIAL TIME ON TAPE: ',I6,
     X           '  IS PAST THE REQUESTED ENDING TIME TO PROCESS: ',I6,
     X           '  +++'/)
            NUFST=1
            RETURN
         END IF
      ELSE IF (ISTAT.EQ.0 .OR. ISTAT.EQ.1) THEN
         GOTO 50
      ELSE
         WRITE(*,70)IUN
 70      FORMAT(/,5X,' +++ ERROR SKIPPING TO START TIME ON UNIT ',I3,
     X        ' +++')
         STOP
      END IF
      PRINT 231,KDAY,IFTIM
 231  FORMAT (//100('+')//
     X     8X,'VOLUME FOUND   -   DAY : ',I6,8X,'BEGINNING TIME : ',I6)
      CALL INITVOL(IPROJ)
      PRINT 236
 236  FORMAT(//6X,'SCAN',18X,'TRACK',24X,'ROTANG',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'MIN',6X,'MAX',6X,'MEAN',
     X     11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',6X,'MEAN',7X,'GOOD',
     X     3X,'BAD')
C
C     READ IN  SWEEPS
C
 100  CONTINUE
      NRAYS=0
      TRCKMX=-999.
      TRCKMN= 999.
      CALL DORSWP(NUFST,JPCK,ELSCAN,ICRTST,CTDBM,CTDBMXH,CTDBMXV,
     X     NTHRSH,TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,
     X     DASANG,ISIDE,REQTIME,ALTMEAN(NSWPS+1),TRCKMEAN(NSWPS+1),
     X     DRFTMEAN(NSWPS+1),PTCHMEAN(NSWPS+1),ROLLMEAN(NSWPS+1),
     X     TRCKMN,TRCKMX,ALTMN,ALTMX,ROTBEG,ROTEND,SPACMN,SPACMX,
     X     SPACMEAN,NRAYS,ISTAT,FLDDAT,RADNAM,FLTNUM,FLDNAM,SCALES,
     X     OFFSET,IFRST,NFLINP,TILT,ALAT1,ALON1)
C
C     ISTAT=1  --> NEW SWEEP
C     ISTAT=2  --> NEW VOLUME
C     ISTAT=3  --> EOD
C     ISTAT=4  --> I/O ERROR
C

      IF (ISTAT.EQ.1 .OR. ISTAT.EQ.2 .OR. (ISTAT.EQ.3 .AND. NRAYS.GT.2))
     X     THEN
C
C     END OF SWEEP; PROCESS IT
C
         IF (NRAYS.LT.MNBEM) THEN
            WRITE(*,*)'+++TOO FEW RAYS IN SWEEP. SWEEP SKIPPED. NRAYS=',
     X           NRAYS
            GOTO 100
         END IF
         ID(IPTR)  =NINT(TRCKMEAN(NSWPS+1)*ID(44))
         ID(IPTR+1)=NINT(DRFTMEAN(NSWPS+1)*ID(44))
         ID(IPTR+2)=NRAYS
         NSWPS=NSWPS+1
         IF (NSWPS.GT.MAXEL) THEN
            WRITE(*,125)
 125        FORMAT(/,5X,'+++ MAX. NUMBER OF SWEEPS IS 80 +++')
            STOP
         END IF
         ALATS(NSWPS)=ALAT1
         ALONS(NSWPS)=ALON1

C
C     OUTPUT STATS
C
         PRINT 325,NSWPS,TRCKMN,TRCKMX,TRCKMEAN(NSWPS),ROTBEG,ROTEND,
     X        SPACMN,SPACMX,SPACMEAN,NRAYS
 325     FORMAT(4X,I2,4X,' 1',8X,3(F6.2,3X),4X,2(F6.1,4X),3(F6.2,3X),
     X        I7,'   0')

         IPTR=IPTR+3
         IF (ISTAT.EQ.1) THEN
C
C     READ IN NEXT SWEEP
C
            GOTO 100 
            
         ELSE IF (ISTAT.EQ.2 .AND. (ICOMBN.EQ.1 .OR. ICOMBN.EQ.2)) THEN
C     COMBINE NEXT VOLUME IN SAME FILE WITH CURRENT VOLUME 
            GOTO 65
         ELSE IF (ISTAT.EQ.3 .AND. ICOMBN.EQ.2) THEN
C
C     TRY TO OPEN A NEW FILE AND COMBINE VOLUMES FROM THERE W/ CURRENT
C
               READ 503,KRD
 503           FORMAT (10A8)
               READ (KRD,504)KOMM
 504           FORMAT (A3)
               IF (KOMM.NE.'INP') THEN
                  PRINT 505
 505              FORMAT (5X,'***COULD NOT FIND NEXT INPUT CARD.',
     X                 ' NO MORE VOLUMES WILL BE COMBINED.')
                  IF (NSWPS.LT.2) THEN
                     WRITE(*,501)NSWPS
 501                 FORMAT(5X,'*** VOLUME DISCARDED - ONLY ',I1,
     X                    ' ELEVATION SCANS')
                     GOTO 2
                  END IF

                  CALL WRRYDK(KPCK,KOUT,INST,LTMP,9,0)
                  ID(35)=NSWPS
                  ID(36)=NRAYS
                  REWIND LTMP
                  IELCHK=IELCHK.OR.ILSCHK

                  CALL AIRCHK(ALTMEAN,TRCKMEAN,DRFTMEAN,PTCHMEAN,
     X                 ROLLMEAN)
                  CALL SETVOL
                  NUFST=0
                  RETURN
               ELSE
                  IJNK=1
                  CALL INPFIL(KRD,INPTST,IJNK,AZCOR)
                  GOTO 50
               ENDIF
            
         ELSE IF (ISTAT.EQ.2 .OR. ISTAT.EQ.3) THEN
C
C     END OF VOLUME ALSO
C
            IF (NSWPS.LT.2) THEN
               WRITE(*,501)NSWPS
               GOTO 2
            END IF

            CALL WRRYDK(KPCK,KOUT,INST,LTMP,9,0)
            ID(35)=NSWPS
            ID(36)=NRAYS
            REWIND LTMP
            IELCHK=IELCHK.OR.ILSCHK
         

            CALL AIRCHK(ALTMEAN,TRCKMEAN,DRFTMEAN,PTCHMEAN,ROLLMEAN)
            CALL SETVOL
            NUFST=0

            RETURN
         END IF

      ELSE

         WRITE(*,40)
 40      FORMAT(/,5X,'+++ I/O ERROR READING DORADE VOLUME. CONTACT',
     X        ' SOFTWARE ENGINEER+++')
         STOP
      END IF

      END