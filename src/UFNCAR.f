      SUBROUTINE UFNCAR(NUFST,JPCK,ELSCAN,ICRTST,NTHRSH,
     X     TFIELD,TLIMITS,RFNAM,FNUM,P1,P2,P3,P4,P10,AZCOR,DASANG,
     X     ISIDE)
C     
C     THIS SUBROUTINE READS ONE VOLUME SCAN IN UNIVERSAL FORMAT
C     AND OUTPUTS IT TO DISK IN A SIMPLIFIED FORMAT.
C     
C     NUFST
C     =0    NO END YET (NORMAL EXECUTION)
C     =1    PAST ENDING TIME
C     =2    END OF TAPE
C     ICOPLANE
C     =0    R,A,E -> CARTESIAN
C     =1    R,A,C -> COPLANE (ANGLES OF DATA)
C     =2    R,A,C -> COPLANE (USER SPECIFIED COPLANE ANGLES)
C     =3    R,A,C -> CARTESIAN
C     =4    RHI   -> CARTESIAN
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
      PARAMETER (NID=296,MAXFLD=8,MAXSKP=27,MAXEL=80)
      PARAMETER(MAXRNG=768)
      DIMENSION MTFIEL(MAXFLD),ITHR(MAXFLD+1)
      DIMENSION JPCK(1),ELSCAN(1),TLIMITS(2,MAXFLD),THVAL(2,1024)
      DIMENSION ISIDE(MAXFLD)
      CHARACTER*8 KRD(10),RFNAM,P10,NTM(MAXFLD)
      CHARACTER*8 CTEMP1,CTEMP3,IEMPTY
      CHARACTER*2 NAMPRE
      CHARACTER*3 KOMM
      CHARACTER*8 NAMFLD
      CHARACTER*8 TFIELD(2,MAXFLD),THON(2),CFIELD,CTEMP2,ITM(MAXFLD,3)
      INTEGER CVMGP
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

      COMMON /BYTORD/ MBYTE
      COMMON /SCRDSK/ LTMP
      COMMON /SCAN/ ICOPLANE,IFLGBAS,IPPI
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X     ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      LOGICAL IELCHK,ILSCHK
      CHARACTER*8 INP,IOUT,MET,ICRT,NAMTIM
      CHARACTER*2 MUF,MFL
      DATA INP,IOUT,MET,ICRT/'INPUT','OUTPUT','INTERP','GRID'/
      DATA LFTIM,JRH6,JRH7,IBAD,IEMPTY,MUF,MFL
     X     /0,64,100,-32768,'-9999','UF','FL'/
      DATA MFTOL/25/
      DATA IEDBD1/ O'777' /
C     MXL   IS THE MAXIMUM ELEVATION*10+1 TO ALLOW IN THE MODAL SELECTION
C     MXSCN IS THE MAXIMUM EXPECTED NUMBER OF BEAMS/SCAN
      DATA MXL/901/
      DATA MXSCN/1000/
      DATA NAMTIM/'TIME'/
C     THE FOLLOWING IS THE DEGREES TO RADIANS CONVERSION
      DATA DTR /0.0174533/
      DATA ITHR/9*0/
C     
C     NORMALIZE 16-BIT 2-S COMPLEMENTS INTEGERS
C     
      INORM(I)=CVMGP(I-65536,I,I-32768)
C     
C     MAKE SCALED (X 64) ELEVATIONS IN 350-360 RANGE NEGATIVE (0 TO -10)
C     
      INANG(I)=CVMGP(I-23040,I,I-22400)
C     
C     
C     THE FOLLOWING FUNCTION CALCULATES COPLANE ANGLE FROM ELEV. AND AZIM.
C     
      CALCOP(E,A)=ATAN((TAN(E*DTR)/ABS(SIN(A*DTR))))/DTR
C     
C     THE FOLLOWING FUNCTION CALCULATES ELEV ANGLE FROM FIXED AND AZIM.
C     
      CALEL(F,A)=ATAN((TAN(F*DTR)*ABS(SIN(A*DTR))))/DTR
C     
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
      END IF

      IEOF=0
      IFLGBAS=0
      BASANG=ANGXAX-90.0
      SCALE=1.0/64.0
      ELTOL=ELTUS
      
C     
C     CHECK TO SEE IF ALL COMMONS ARE FILLED
C     
      IF (ITAP.NE.IEMPTY) GOTO 5
      PRINT 100,INP
      STOP
 5    CONTINUE
      IF (LTAP.NE.IEMPTY) GOTO 10
      PRINT 100,IOUT
      STOP
 10   CONTINUE
 20   CONTINUE
      IF (ISCI.NE.IEMPTY) GOTO 25
      PRINT 100,MET
      STOP
 25   CONTINUE
      IF (ICRTST.GE.0) GOTO 60
      PRINT 100,ICRT
      STOP
C     
C     OTHER CHECKS GO HERE
C     
 100  FORMAT(5X,'+++  ',A8,' COMMAND MUST APPEAR BEFORE THE PROCESS',
     X     ' COMMAND  +++')
 60   CONTINUE
      NFLINP=NFLDS
      IF(IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
 200  CONTINUE
C     
C     START      - FIND VOLUME => TRANSLATE
C     
C     INITIALIZE THRESHOLD COUNTERS
      DO 23 I=1,MAXFLD+1
         ITHR(I)=0
 23   CONTINUE
C     
C     DETERMINE THE METHOD OF ELEVATION ANGLE COMPUTATION
C     
      WRITE (CTEMP1,550)IRCFXL
 550  FORMAT(A8)
      READ (CTEMP1,232)I,GNEL
 232  FORMAT(A2,1X,F5.0)
      IF(CTEMP1(1:2).EQ.'MD') THEN
C     MODE
         IELT=1
         IGNEL=AMAX1(GNEL,0.0)
         IELCHK=.TRUE.
      ELSE IF(CTEMP1(1:2).EQ.'MN') THEN
C     MEAN
         IELT=2
         IGNEL=AMAX1(GNEL,0.0)
         IELCHK=.TRUE.
      ELSE
C     TAPE VALUE
         IELT=0
         IGNEL=0
         IELCHK=.FALSE.
      END IF
C     
C
C     CHECK TO MAKE SURE USER IS NOT REQUESTING INTERP. TO DEFAULT
C     COPLANE ANGLES AND REQUESTING A REDEF. OF FIXED ANGLES
C
      IF (ICOPLANE.EQ.1 .AND. IELCHK) THEN
         WRITE(*,*)'***CANNOT INTERPOLATE TO DEFAULT COPLANE ANGLES',
     X             ' OF DATA AND REDEFINE FIXED ANGLE AS MEAN OR MODE'
         STOP
      END IF

      LTMP=1
      WRITE(LTMP) LTMP
      REWIND LTMP
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
      CALL NWRAY(NWDS,MBYTE,NST)
      IBUF(33)=INORM(IBUF(33))
      IF (NST.NE.0) THEN
         PRINT 201,ITAP,IUN
 201     FORMAT (5X,'**END OF INFORMATION ON TAPE ',A6,' UNIT ',I3)
         NUFST=2
         RETURN
      END IF  
 220  CONTINUE
C     
C     CHECK IF VOLUME IS GOOD
C     
      IFUT=IBUF(32)
C      CALL SHILBL(IFUT,1)
      LFTIM=0
C     MARK BRADFORD PATCH TO REMOVE CONVERSION TO MDT
C     IF (IFUT.EQ.'UT') LFTIM=-6
      IHR=IBUF(29)+LFTIM
      IF (IHR.LT.0) IHR=IHR+24
      IFTIM=10000*IHR+100*IBUF(30)+IBUF(31)
      IDAY=10000*IBUF(26)+100*IBUF(27)+IBUF(28)
      IF (KETIM.NE.999999) THEN
C     POSITION BY DATE AND TIME; ELSE ANY VOLUME IS OK
         IF (IDAY.LT.KDAY) THEN
            PRINT 230, IDAY,IFTIM
            CALL SKPVOL(IUN,1)
C     
C     GO FIND A NEW VOLUME
C     
            GOTO 200
         ELSE IF (IDAY.GT.KDAY) THEN
C     UP TIME BY 24 HOURS 
            IFTIM=IFTIM+240000*(IDAY-KDAY)
         END IF
         IF (IFTIM.LT.KBTIM) THEN
            PRINT 230, IDAY,IFTIM
 230        FORMAT(' +++  VOLUME SKIPPED  -  DAY: ',I6,
     X           5X,'BEGINNING TIME: ',I6,'  +++')
            CALL SKPVOL(IUN,1)
C     
C     GO FIND A NEW VOLUME
C     
            GOTO 200
         ELSE IF (IFTIM.GT.KETIM) THEN
            PRINT 233, IUN,IFTIM,KETIM
 233        FORMAT(/1X,'+++  UNIT: ',I2,5X,'INITIAL TIME ON TAPE: ',I6,
     X           '  IS PAST THE REQUESTED ENDING TIME TO PROCESS: ',I6,
     X           '  +++'/)
            NUFST=1
            BACKSPACE IUN
            RETURN
         ENDIF
      END IF
      PRINT 231,IDAY,IFTIM
 231  FORMAT (//100('+')//
     X     8X,'VOLUME FOUND   -   DAY : ',I6,8X,'BEGINNING TIME : ',I6)
      IF(IBUF(35).NE.1.AND.IBUF(35).NE.2.AND.IBUF(35).NE.8
     X     .AND. IBUF(35).NE.3) THEN
         PRINT 221,IBUF(35)
 221     FORMAT (5X,'***ERROR IN MODE ',I1,'  - VOLUME DISCARDED***')
         CALL DUMBUF(IBUF,150,'UF',1)
         CALL SKPVOL(IUN,1)
C     
C     GO FIND A NEW VOLUME
C     
         GOTO 200
      ENDIF
      IDRGCHG=0
      INRNG=0
      IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
         IF (IBUF(35).NE.2) THEN
            WRITE(*,1010)
 1010       FORMAT('***WARNING. VOLUME NOT SCANNED IN COPLANES.',
     X           ' INTERPOLATING ANYWAY***')
c 1010       FORMAT(' ***REQUESTED COPLANE INTERP--VOLUME IS NOT ',
c     X           'COPLANE...SKIPPED DAY: ',I6,' BEGINNING TIME: ',I6)
c            CALL SKPVOL(IUN,1)
c            GOTO 200
         END IF
      ELSE
         IF (IBUF(35).EQ.2) THEN
            ICOPLANE=3
         ELSE IF (IBUF(35).EQ.3) THEN
            ICOPLANE=4
         ELSE
            ICOPLANE=0
         END IF
      END IF
c      IF (IBUF(35).EQ.2 .AND. ICOPLANE.EQ.0) ICOPLANE=3
c      IF (IBUF(35).NE.2 .AND. ICOPLANE.EQ.3) ICOPLANE=0
c      IF ((ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) .AND. IBUF(35).NE.2) THEN
c         WRITE(*,1010)KDAY,IFTIM
c 1010    FORMAT(' ***REQUESTED COPLANE INTERP--VOLUME IS NOT COPLANE',
c     X        '...SKIPPED DAY: ',I6,' BEGINNING TIME: ',I6)
c         CALL SKPVOL(IUN,1)
c         GOTO 200
c      END IF
C     
C     CHECK FOR RHI MODE
C     
c      IF (IBUF(35).EQ.3 .AND. ICOPLANE.EQ.0) THEN
c         ICOPLANE=4
c      END IF

C
C     OUTPUT INFO ABOUT TYPE OF INTERPOLATION
C
      IF (ICOPLANE.EQ.0) THEN
         WRITE(*,2000)
 2000    FORMAT(5X,'+++GRIDDING DATA IN R,A,E TO 3-D CARTESIAN+++')
      ELSE IF (ICOPLANE.GT.0 .AND. ICOPLANE.LT.3) THEN
         DASANG=BASANG
         WRITE(*,2010)
 2010    FORMAT(5X,'+++GRIDDING DATA IN R,A,C, TO REGULAR COPLANE ',
     X         'GRID+++')
      ELSE IF (ICOPLANE.EQ.3) THEN
         IF (DASANG.EQ.-999.0) DASANG=IBUF(IBUF(3)+4)*SCALE
         BASANG=DASANG
         WRITE(*,2020)
 2020    FORMAT(5X,'+++GRIDDING DATA IN R,A,C TO 3-D CARTESIAN+++')
      ELSE IF (ICOPLANE.EQ.4) THEN
         WRITE(*,2022)
 2022    FORMAT(5X,'+++GRIDDING DATA IN RHI TO 3-D CARTESIAN+++')
      END IF


      WRITE(CTEMP3,934)IBUF(1)
      IF (CTEMP3.NE.MUF) THEN
         PRINT 222,ITAP
 222     FORMAT(5X,'***ERROR IN FORMAT OF TAPE ',A6,'-MAY NOT BE U.F.')
      ENDIF

      IF (ICOPLANE.EQ.4 .AND. ANGXAX.NE.90.0) THEN
         WRITE(*,2044)
 2044    FORMAT(/,5X,'+++WARNING--ANGLE OF X-AXIS RESET TO 90 DEGREES',
     X        ' FOR RHI SCAN. ROTATE IN CEDRIC.+++')
      END IF


C     
C     FOUND VOLUME
C     
      CALL INITVOL(IPROJ)
      MFBM=0
      ID(4)=IHR
      ISWP=IBUF(10)
      NSWPS=0
      NRAYS=0
      IBEAM=0
C     
C     NORMALIZE ELEVATION/CHECK FOR NEGATIVES
C     
      IBUF(36)=INORM(IBUF(36))
      IBUF(36)=INANG(IBUF(36))
      ELSAV=IBUF(36)*SCALE
      IF (IPTR.GT.129 .AND. ABS(ELSAV-ID(IPTR-3)/REAL(ID(44))).GT.
     X     180.0) ELSAV=ELSAV+360.0
      DIR=0.0
      IF (ICOPLANE.LT.4) THEN
         PAZ=IBUF(33)*SCALE + AZCOR
         IF (PAZ.GT.360.0) PAZ=PAZ-360.0
         IF (PAZ.LT.0.0) PAZ=PAZ+360.0
      ELSE IF (ICOPLANE.EQ.4) THEN
         PAZ=IBUF(34)*SCALE
      END IF
      IPTR=129
      BEGAZ=PAZ
      IBEGRG=ID(31)*1000+ID(32)
      NBDBM=0
      ELMAX=0.0
      AZMAX=0.0
      ELSUM=0.0
      ELMIN=1000.0
      AZMIN=1000.0
      IFIRST=1
      CALL CONFLD(ELSCAN,MXSCN+MXL,0.0)
      IF (ICOPLANE.EQ.0) THEN
         PRINT 236
      ELSE IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN
         PRINT 237
      ELSE IF (ICOPLANE.EQ.4) THEN
         PRINT 238
      END IF
 236  FORMAT (//6X,'SCAN',18X,'ELEVATION',29X,'AZIMUTH',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD')
 237  FORMAT (//6X,'SCAN',18X,'COPLANE',29X,'AZIMUTH',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD')
 238  FORMAT (//6X,'SCAN',18X,'AZIMUTH',29X,'ELEVATION',14X,'SPACING',
     X     18X,'BEAMS'/4X,'NO',3X,'DIR',8X,'FIXED',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',11X,'BEG',7X,'END',6X,'MIN',6X,'MAX',
     X     5X,'MEAN',7X,'GOOD',3X,'BAD')
      IALREAD=1

C
C     CHECK TO SEE IF USER WANTS FIRST SCAN DISCARDED
C
      ISKIPFX=0
      IF (ISKIP.EQ.1 .AND. IFXTAB.EQ.1) THEN
         NSWP1=NSWPS+1
         DO 850 ISK=1,MAXSKP
            IF (FTABLE(ISK).EQ.REAL(NSWP1)) THEN
               ISKIPFX=1
               GOTO 810
            END IF
 850     CONTINUE
 810     CONTINUE
      ELSE IF (IACCPT.EQ.1 .AND. IFXTAB.EQ.1) THEN
         NSWP1=NSWPS+1
         DO 825 I=1,MAXSKP
            IF (REAL(NSWP1).EQ.FTABLE(I)) GOTO 835
 825     CONTINUE
         ISKIPFX=1
 835     CONTINUE
      END IF
      

C     
C     MAIN LOOP OVER RAYS FOLLOWS
C     
 240  DO 495 ILP=1,1000000
         IF (ILP.EQ.1000000) THEN
            WRITE(*,*)'ERROR-TOO MANY RAYS IN UFNCAR'
            STOP
         END IF
         IF (IALREAD.EQ.1) THEN
            IALREAD=0
         ELSE
            CALL NWRAY(NWDS,MBYTE,NST)
            IBUF(33)=INORM(IBUF(33))
         END IF
C     
C     HERE WE BRANCH BASED ON THE STATUS OF THE READ
C     
 250     CONTINUE
         IF (NST.EQ.0 .OR. IJ.EQ.1) THEN
            IF (IJ.EQ.1) IJ=0
            IF (NST.EQ.0) IEOF=0
C     
C     ***GOOD READ***
C     
C     
C     CONSTRUCT RAY HEADER
C     
            KOUT(6)=JRH6
            KOUT(7)=JRH7

C
C     CHECK IF THIS IS THE FIRST SWEEP TO BE PROCESSED; IF SO GET RANGE INFO 
C     FROM IT
C
            IF (ISKIPFX.EQ.0 .AND. IFIRST.EQ.1) THEN
               IFIRST=0
               CALL INITVOL(IPROJ)
            END IF
C
C     SEE IF THIS RAY SHOULD BE THROWN OUT BECAUSE IT'S PART OF AN UNWANTED
C     ELEVATION SCAN
C
            IF (ISKIPFX.EQ.1) THEN
               IF (ISWMTH.NE.0) THEN
                  ELCUR=ELSAV
                  IF (ELCUR.GT.360.0) ELCUR=ELCUR-360.0
                  IF (ABS(IBUF(36)*SCALE-ELCUR).LT.FXSTOL) GOTO 495
               ELSE 
                  IF (IBUF(10).EQ.ISWP) GOTO 495
               END IF
            END IF

            IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
C     
C     CONVERT AZ. IN HOR. TO AZ. IN COPLANE
C     
               AZZ=IBUF(33)*SCALE + AZCOR
               AZZ=AZZ-BASANG
               IF (AZZ.LT.0) AZZ=AZZ+360.
               IF (AZZ.GT.360.0) AZZ=AZZ-360.
               IF (AZZ.GT.180.0 .OR. AZZ.LT.0.0) THEN
C     
C     DATA IS ON OTHER SIDE OF BASELINE; PRINT WARNING MESSAGE
C     
                  
                  WRITE(*,252)AZZ
 252              FORMAT('***BEAM ON WRONG SIDE OF BASELINE.',
     X                 ' HOR. AZIMUTH=',F8.2,' ---BEAM DISCARDED***')
                  NBDBM=NBDBM+1
                  GOTO 495
               END IF
               ID(51)=BASANG*64.0
               ELL=IBUF(34)*SCALE
               COP=IBUF(36)*SCALE
               IF (AZZ.EQ.90.0) THEN
                  AZC=90.0
               ELSE IF (COP.LT.90.0) THEN
                  AZC=(ATAN2(TAN(AZZ*DTR),COS(COP*DTR))/DTR)
                  IF (AZC.LT.0) AZC=AZC+180.0
               ELSE
                  WRITE(*,*)'***BAD ANGLES IN UFNCAR***'
                  STOP
               END IF
               KOUT(1)=NINT(AZC*64.)
            ELSE IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3) THEN
               AZZ=IBUF(33)*SCALE + AZCOR - (ANGXAX-90.0)
               IF (AZZ.GT.360.0) AZZ=AZZ-360.0
               IF (AZZ.LT.0.0) AZZ=AZZ+360.0
               AZTMP=(IBUF(33)*SCALE + AZCOR)
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               KOUT(1)=NINT((AZTMP)/SCALE)
            ELSE IF (ICOPLANE.EQ.4) THEN
               AZC=90.0 - IBUF(34)*SCALE
               IF (AZC.LT.0.0 .OR. AZC.GT.90.0) THEN
                  WRITE(*,*)'***NEG. ELEV. ANGLE ---BEAM DISCARDED***'
                  NBDBM=NBDBM+1
                  GOTO 495
               END IF
               AZ=IBUF(33)*SCALE + AZCOR
               KOUT(1)=NINT(AZ*64.)
               AZZ=AZ
            ELSE
               WRITE(*,*)'***INVALID ANGLE MODE IN UFNCAR***'
               STOP
            END IF
C     
C     NORMALIZE ELEVATION/CHECK FOR NEGATIVES
C     
            IBUF(34)=INORM(IBUF(34))
            IBUF(34)=INANG(IBUF(34))
            KOUT(2)=IBUF(34)*SCALE*KOUT(7)
            KOUT(3)=IBUF(29)+LFTIM
            IF (KOUT(3).LT.0) KOUT(3)=KOUT(3)+24
            ID(7)=KOUT(3)
            KOUT(4)=IBUF(30)
            ID(8)=IBUF(30)
            KOUT(5)=IBUF(31)
            ID(9)=IBUF(31)
            
 300        CONTINUE
 310        CONTINUE
C     
C     KEY ON SWEEP NUMBER OR FIXED ANGLE
C     
            
            ELCUR=ELSAV
            IF (ELCUR.GT.360.0) ELCUR=ELCUR-360.0
            IF((ISWMTH.EQ.0 .AND. IBUF(10).NE.ISWP) .OR.
     X      (ISWMTH.NE.0 .AND. ABS(ELCUR-(IBUF(36)*SCALE)).GT.FXSTOL))
     X           THEN
C     AN ELEVATION HAS BEEN COMPLETED
C     WRAP UP OLD SCAN
C     
 320           CONTINUE
               IF (IBEAM.LT.MNBEM .AND. IFXTAB.NE.1) THEN
c                  CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
                  IF (IBEAM.GT.0) THEN
                     CALL WRRYDK(KPCK,KOUT,INST,LTMP,(NSWPS+11),IBEAM)
                     WRITE(*,234)NSWPS+1,IBEAM
                  END IF
 234              FORMAT(5X,'+++SWEEP ',I3,' DISCARDED. ONLY HAD',
     X                 I4,' BEAMS+++')
                  NRAYS=NRAYS-IBEAM
               ELSE
                  ELFIX=ELSAV
C     
C     RE-COMPUTE FIXED ANGLE IF REQUESTED
C     
                  IF(IELCHK) THEN
                     I1=IGNEL+1
                     I2=IBEAM-IGNEL
                     IF(I2.LT.I1) THEN
                        I1=IBEAM/2+1
                        I2=I1
                     END IF
                     SUM=0.0
                     DO 321 I=I1,I2
                        SUM=SUM+ELSCAN(I)
                        J=NINT(ELSCAN(I)*10.0)+1
                        IF(J.LT.1.OR.J.GT.MXL) GO TO 321
                        ELSCAN(MXSCN+J)=ELSCAN(MXSCN+J)+1.0
 321                 CONTINUE
                     IF(IELT.EQ.1) THEN
                        MODE=0
                        ELMOD=0.0
                        DO 322 I=1,MXL
                           IF(ELSCAN(MXSCN+I).LE.ELMOD) GO TO 322
                           MODE=I
                           ELMOD=ELSCAN(MXSCN+I)
 322                    CONTINUE
                        IF(MODE.NE.0) ELFIX=(MODE-1)*0.1
                     ELSE IF (IELT.EQ.2) THEN
                        ELFIX=SUM/(I2-I1+1)
                     END IF
                  END IF
                  ID(IPTR)=NINT(ELFIX*ID(44))
                  IF (ICOPLANE.EQ.4 .AND. .NOT.IELCHK .AND. 
     X                 AZCOR.NE.0.0) THEN
C
C     ADJUST FIXED ANGLE BY AZIMUTH CORRECTION TOO FOR RHIS
C
                     ELFIX=ELFIX+AZCOR
                     ID(IPTR)=NINT(ELFIX*ID(44))
                  END IF
                  ID(IPTR+1)=SIGN(1.0,DIR)
C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS
C
                  IF (ICOPLANE.EQ.4 .AND. ((ELFIX.LT.45.0 .AND.
     X                 ELFIX.GE.0) .OR. (ELFIX.LT.315. .AND.
     X                 ELFIX.GE.225.0) .OR. (ELFIX.LT.405 .AND.
     X                 ELFIX.GE.315))) ID(IPTR+1)=-ID(IPTR+1)
                  ID(IPTR+2)=NRAYS
                  NSWPS=NSWPS+1
                  IF (NSWPS.GT.MAXEL) THEN
                     PRINT 371,NSWPS
 371                 FORMAT (5X,'***ERROR - CANNOT HAVE ',I2,' SWEEPS')
                     STOP 371
                  ENDIF
C
C     AVOID DIVISIONS BY ZERO IF LAST SWEEP WAS DISCARDED
C
                  IF (ISKIPFX.EQ.0) THEN
                     DELAZ=ABS(DIR)/(IBEAM-1)
                     DELEL=ELSUM/IBEAM
                  ELSE
                     DELAZ=0.0
                     DELEL=0.0
                  END IF

                  PRINT 325,NSWPS,ID(IPTR+1),ELSAV,ELMIN,ELMAX,DELEL, 
     X                 BEGAZ,PAZ,AZMIN,AZMAX,DELAZ,IBEAM,NBDBM
 325              FORMAT(2(4X,I2),8X,4(F6.2,3X),4X,2(F6.1,4X),
     X                 3(F6.2,3X),I7,I6)
                  IPTR=IPTR+3
C
C     CHECK TO SEE IF NEXT ELEVATION SWEEP IS TO BE SKIPPED AT USER'S REQUEST
C     ISKIPFX=0  ==>  DON'T SKIP NEXT SWEEP
C     ISKIPFX=1  ==>  SKIP NEXT SWEEP
C
                  ISKIPFX=0
                  IF (ISKIP.EQ.1 .AND. IFXTAB.EQ.1) THEN
                     NSWP1=NSWPS+1
                     DO 150 ISK=1,MAXSKP
                        IF (FTABLE(ISK).EQ.REAL(NSWP1)) THEN
                           ISKIPFX=1
                           GOTO 210
                        END IF
 150                 CONTINUE
 210                 CONTINUE
                  ELSE IF (IACCPT.EQ.1 .AND. IFXTAB.EQ.1) THEN
                     NSWP1=NSWPS+1
                     DO 225 I=1,MAXSKP
                        IF (REAL(NSWP1).EQ.FTABLE(I)) GOTO 235
 225                 CONTINUE
                     ISKIPFX=1
 235                 CONTINUE
                  END IF
               ENDIF
C     
C     GO DO END OF VOLUME PROCESSING
C     
               IF (NST.NE.0) GOTO 500
C     
C     INITIALIZE FOR NEXT SCAN
C     
               IBEAM=0
               DIR=0.0
               IF (ICOPLANE.LT.4) THEN
                  PAZ=IBUF(33)*SCALE + AZCOR
                  IF (PAZ.GT.360.0) PAZ=PAZ-360.0
                  IF (PAZ.LT.0.0) PAZ=PAZ+360.0
               ELSE IF (ICOPLANE.EQ.4) THEN
C
C     RHI CASE
C
                  PAZ=IBUF(34)*SCALE
                  IF (PAZ.LT.0.0 .OR. PAZ.GT.90.0) THEN
                     WRITE(*,*)'***BAD ANGLE IN UFNCAR***'
                  END IF
               END IF
               BEGAZ=PAZ
               NBDBM=0
               ELMAX=0.0
               AZMAX=0.0
               ELSUM=0.0
               ELMIN=1000.0
               AZMIN=1000.0
               CALL CONFLD(ELSCAN,MXSCN+MXL,0.0)
               ISWP=IBUF(10)
C     
C     NORMALIZE ELEVATION/CHECK FOR NEGATIVES
C     
               IBUF(36)=INORM(IBUF(36))
               IBUF(36)=INANG(IBUF(36))
               ELSAV=IBUF(36)*SCALE
               IF (IPTR.GT.129 .AND. ABS(ELSAV-ID(IPTR-3)/REAL(ID(44)))
     X              .GT.180.0) ELSAV=ELSAV+360.0
            END IF
C     
C     PROCESS BEAM
C     
            IF(IBEAM.GE.MXSCN) THEN
C     
C     TOO MANY BEAMS IN THE SCAN (NON-FATAL)
C     
               NBDBM=NBDBM+1
               PRINT 706, IBEAM
 706           FORMAT(5X,'*** TOO MANY BEAMS/SCAN  (',I5,' MAX )  ',
     X              '--RAY DISCARDED ***')
C     
C     GO GET ANOTHER RAY
C     
               GO TO 495
            END IF
C     
C     PROCESS SWEEP
C     
            IF(.NOT.IELCHK) THEN
               THETA=IBUF(34)*SCALE
               PHI=IBUF(33)*SCALE + AZCOR - (ANGXAX-90)
               IF (PHI.LT.0) PHI=PHI+360.
               IF (PHI.GT.360.0) PHI=PHI-360.0
               IF (THETA.EQ.90.0) THEN
                  WRITE(*,*)'***BAD ANGLES IN UFNCAR***'
                  STOP
               END IF
C     
C     ELANG IS THE ELEV ANGLE CALCULATED FROM THE FIXED AND AZIM ANGLES
C     
               IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
                  ELANG=CALEL(IBUF(36)*SCALE,PHI)
                  DASANG=BASANG
                  IF (DASANG.LT.0.0) DASANG=DASANG+360.0
                  IF (ELANG.LT.0.0 .OR. ELANG.GT.90.0) THEN
                     WRITE(*,*)'***INVALID ELEV ANGLE IN UFNCAR***'
                     STOP
                  END IF
               ELSE IF (ICOPLANE.EQ.3) THEN
                  BASANG=DASANG
                  PHI = IBUF(33)*SCALE + AZCOR - BASANG
                  IF (PHI.LT.0.0) PHI=PHI+360.0
                  IF (PHI.GT.360.0) PHI=PHI-360.0
                  ELANG = CALEL(IBUF(36)*SCALE,PHI)
                  IF (ELANG.LT.0.0 .OR. ELANG.GT.90.0) THEN
                     WRITE(*,*)'***INVALID ELEV ANGLE IN UFNCAR***'
                     STOP
                  END IF
               ELSE IF (ICOPLANE.EQ.4) THEN
                  ELANG=IBUF(33)*SCALE + AZCOR
               END IF
C
C     CHECK TOLERANCE NOW
C     
               IF (ICOPLANE.EQ.0) THEN
                  IF (ABS((IBUF(36)-IBUF(34))*SCALE).GT.ELTOL) THEN
                     NBDBM=NBDBM+1
C
C     GO GRAB A NEW BEAM
C
                     GOTO 495
                  END IF
               ELSE IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN
                  IF (ABS((IBUF(34)*SCALE)-ELANG).GT.ELTOL) THEN
                     NBDBM=NBDBM+1
C
C     GO GRAB A NEW BEAM
C
                     GOTO 495
                  END IF
               ELSE IF (ICOPLANE.EQ.4) THEN
                  IF (ABS((IBUF(36)*SCALE)-ELANG).GT.ELTOL) THEN
                     NBDBM=NBDBM+1
                     GOTO 495
                  END IF
               END IF
            END IF
C     
C     NORMALIZE ELEVATION/CHECK FOR NEGATIVES
C     
            IBUF(34)=INORM(IBUF(34))
            IBUF(34)=INANG(IBUF(34))
            ACTEL=IBUF(34)*SCALE
            NRAYS=NRAYS+1
            IBEAM=IBEAM+1
            IF (ICOPLANE.GE.1 .AND. ICOPLANE.LE.3) THEN
               AZTMP=IBUF(33)*SCALE + AZCOR
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               DIF=AZTMP - PAZ
               PAZ=IBUF(33)*SCALE + AZCOR
               IF (PAZ.GT.360.0) PAZ=PAZ-360.0
               IF (PAZ.LT.0.0) PAZ=PAZ+360.0
               IF (ACTEL.GE.90.0 .OR. (PAZ - (BASANG)).EQ.0.0) THEN
                  WRITE(*,*)'***INVALID ANGLE IN UFNCAR***'
                  STOP
               END IF
               ELSCAN(IBEAM)=CALCOP(ACTEL,(PAZ - DASANG))
               IF (ELSCAN(IBEAM).LT.0.0 .OR. ELSCAN(IBEAM).GT.90.0) THEN
                  WRITE(*,*)'***INVALID ANGLE IN UFNCAR***'
                  STOP
               END IF
               IF (ELSCAN(IBEAM).GT.ELMAX) ELMAX=ELSCAN(IBEAM)
               IF (ELSCAN(IBEAM).LT.ELMIN) ELMIN=ELSCAN(IBEAM)
               ELSUM=ELSUM+ELSCAN(IBEAM)
            ELSE IF (ICOPLANE.EQ.0) THEN
               AZTMP=IBUF(33)*SCALE + AZCOR
               IF (AZTMP.GT.360.0) AZTMP=AZTMP-360.0
               IF (AZTMP.LT.0.0)   AZTMP=AZTMP+360.0
               DIF=AZTMP - PAZ
               PAZ=IBUF(33)*SCALE + AZCOR
               IF (PAZ.GT.360.0) PAZ=PAZ-360.0
               IF (PAZ.LT.0.0) PAZ=PAZ+360.0
               ELSCAN(IBEAM)=ACTEL
               ELSUM=ELSUM+ACTEL
               IF (ACTEL.GT.ELMAX) ELMAX=ACTEL
               IF (ACTEL.LT.ELMIN) ELMIN=ACTEL
            ELSE IF (ICOPLANE.EQ.4) THEN
               AZTMP=IBUF(34)*SCALE
               DIF=AZTMP-PAZ
               PAZ=AZTMP
               ELSCAN(IBEAM)=IBUF(33)*SCALE + AZCOR
               IF (IPTR.GT.129) THEN
C     
C     CHECK FOR 360 CROSSOVER. WANT FIXED ANGLES TO MONOTONICALLY INCREASE
C     
                  IF (ABS(ELSCAN(IBEAM)-ID(IPTR-3)/REAL(ID(44))).GT.
     X                 180.0) ELSCAN(IBEAM)=ELSCAN(IBEAM)+360.0
               END IF               
               ELSUM=ELSUM+ELSCAN(IBEAM)
               IF (ELSCAN(IBEAM).GT.ELMAX) ELMAX=ELSCAN(IBEAM)
               IF (ELSCAN(IBEAM).LT.ELMIN) ELMIN=ELSCAN(IBEAM)
            END IF
            IF (ABS(DIF).GT.180) DIF=DIF-SIGN(360.0,DIF)
            ABDIF=ABS(DIF)
            IF (ABDIF.GT.AZMAX) AZMAX=ABDIF
            IF (ABDIF.LT.AZMIN .AND. ABDIF.GT.0.0) AZMIN=ABDIF
            DIR=DIR+DIF
C     
C     ESTABLISH SETRANGE
C     
            LCONT=INTCHK(IBUF,RUSR2.GT.0.0)
            IF(LCONT.GT.0) CALL CHKMSG(9,NRAYS)
            IPTD=IBUF(5)
            IPTF=IBUF(IPTD+4)
C     STORE # OF RANGE GATES FOR THIS BEAM
            IF (RUSR2.EQ.0.0) THEN
               KOUT(8)=MIN(IBUF(IPTF+5),MAXRNG)
               NRG=KOUT(8)
            ELSE
               KOUT(8)=ID(34)
            END IF
            IRGFG=INORM(IBUF(IPTF+2))
            IADFG=INORM(IBUF(IPTF+3))
            RJ1=IRGFG+IADFG*0.001
            IF (RNOTUS.NE.0.0) RJ1=RJ1+RNOTUS
            NGFH=IBUF(IPTF+5)
            IF (RUSR2.GT.0.0) THEN
               JL=(RJ1-RG1)/DRG+10001.5
               JL=JL-10001
               JNG=MIN0(JL+NGFH,NGFH)
               IUNPAD=MAX0(-JL,0)
               IPUTAD=MAX0(JL,0)
               JNG=MIN0(JNG,NRG-IPUTAD)
               GOTO 370
            ENDIF
            IF(RNOTUS.EQ.0.0.AND.NINT(RJ1*1000.0).NE.IBEGRG) THEN
               PRINT 468, ID(31),ID(32),RJ1,IBEGRG
 468           FORMAT(5X,2I8,F20.10,I10)
               CALL CHKMSG(2,NRAYS)
            END IF
            IF (IBUF(IPTF+4).NE.ID(33) .AND. DRGUS.EQ.0.0) THEN
C
C     RANGE GATE SPACING HAS CHANGED
C
               IDRGCHG=IDRGCHG+1
               NRAYS=NRAYS-1
               IBEAM=IBEAM-1
               NBDBM=NBDBM+1
               GOTO 495
            END IF
            IF (IBUF(IPTF+5).NE.ID(34) .AND. (DRGUS.EQ.0.0 .AND.
     X           RUSR2.EQ.0.0)) THEN
C
C     NUMBER OF RANGE GATES HAS CHANGED
C
c               INRNG=INRNG+1
c               NRAYS=NRAYS-1
c               IBEAM=IBEAM-1
c               NBDBM=NBDBM+1
c               GOTO 495
            END IF
            IUNPAD=0
            IPUTAD=0
            JNG=NRG
 370        CONTINUE
            IF(ISWMTH.EQ.0) THEN
C     
C     CHECK IF FIXED ANGLE HAS CHANGED (SWEEP NUMBER MODE ONLY)
C     
               FXANG=IBUF(36)*SCALE
               IF (ELSAV.GT.360.0) FXANG=FXANG+360.0
               IF(FXANG.NE.ELSAV.AND.(.NOT.IELCHK)) 
     X              CALL CHKMSG(8,NRAYS)
            END IF
            J1=1+IPUTAD
            J2=J1+JNG-1
C     
C     CHECK AND PLUCK FIELDS NEEDED
C     
            N=1
            IFC=0
            DO 390 I=1,MAXFLD
               MTFIEL(I)=0
 390        CONTINUE
            KST=ID(37)
            IDPTR=76
            IUFPTR=IPTD+3
            IF(MFBM.EQ.MFTOL) THEN
C     
C     CANNOT LOCATE A REQUESTED FIELD  --LIKELY MISSPECIFIED BY USER.
C     
               PRINT 701, (I,IFIELD(I),I=1,NFLINP)
 701           FORMAT(//5X,' FIELDS REQUESTED...'/8X,
     X              ' #   SPRINT NAME'/(8X,I2,3X,A8) )
               PRINT 702
 702           FORMAT(//5X,' FIELDS PRESENT...'/8X,
     X              ' #   SPRINT NAME   ','PREFIX   EDIT CODE')
            END IF
            
            INUM=1
            IDPTRSV=IDPTR
            IFPTRSV=IUFPTR
            
 1050       IFLDPTR=IBUF(IUFPTR+1)
            WRITE(NAMPRE,934)IBUF(IUFPTR)
            IEDFLD=IBUF(IFLDPTR+16)
            IF(IEDFLD.EQ.32768.OR.IEDFLD.EQ.0.OR.IEDFLD.EQ.IEDBD1) THEN
               WRITE(NAMFLD,437)IBUF(IUFPTR)
 437           FORMAT(A2,'  ')
            ELSE
               WRITE(NAMFLD,407)IBUF(IUFPTR),IBUF(IFLDPTR+16)
            END IF
            IF(MFBM.EQ.MFTOL) THEN
C     
C     SUSPECTED MISSING FIELD NAMES
C     
               PRINT 703, N,NAMFLD,NAMPRE
 703           FORMAT(8X,I2,3X,A4,6X,A2,7X)
            END IF
            WRITE(CFIELD,1000)NAMFLD
 1000       FORMAT(A4)
            DO 111 I=1,NTHRSH
               IF (CFIELD.EQ.TFIELD(2,I)) THEN
                  ITYP=ITPFLDC(NAMFLD)
                  ISCL=IBUF(IFLDPTR+1)
C     
C     SPECIAL MODS FOR CRUMMY RADARS LIKE CHILL
C     
                  FACT=1.0
                  IF (ITYP.EQ.3 .AND. NMRAD.EQ.'CHIL') THEN
                     FACT=-100.0/IBUF(IFLDPTR+1)
                  ENDIF
                  IPF=IBUF(IFLDPTR)
                  NGFLD=IBUF(IFLDPTR+5)
                  K=IUNPAD
                  DO 888 J=1,NRG
                     THVAL(INUM,J)=IBAD
                     IF (J.LT.J1 .OR. J.GT.J2) GOTO 888
                     IF(J.GT.NGFLD) GO TO 888
                     IF (IBUF(IPF+K).EQ.IBUF(45)) GOTO 333
                     X=IBUF(IPF+K)
                     Y=CVMGP(NINT(X-65536.),NINT(X),NINT(X-32768.))
     X                    *FACT/ISCL
                     THVAL(INUM,J)=Y
 333                 CONTINUE
                     K=K+1
 888              CONTINUE
C     
C     STORE THE NAMES OF THE FIELDS ON WHICH WE WILL THRESHOLD
C     
                  WRITE(THON(INUM),256)NAMFLD
 256              FORMAT(A4)
                  INUM=INUM+1
                  IF (INUM.GT.3) THEN
                     WRITE(*,*)'***TOO MANY THRESHOLD FIELDS IN ',
     X                    'UFNCAR***'
                     STOP
                  END IF
                  GOTO 444
               ENDIF
 111        CONTINUE
 444        CONTINUE
            IUFPTR=IUFPTR+2
            N=N+1
C     
C     CONTINUE LOOPING THROUGH MORE FIELDS, IF ANY
C     
            IF (N.LE.IBUF(IPTD+2)) GOTO 1050
            
            N=1
            IUFPTR=IFPTRSV
            
            
 400        CONTINUE
            IFLDPTR=IBUF(IUFPTR+1)
            WRITE(NAMPRE,934)IBUF(IUFPTR)
            IEDFLD=IBUF(IFLDPTR+16)
            IF(IEDFLD.EQ.32768.OR.IEDFLD.EQ.0.OR.IEDFLD.EQ.IEDBD1) THEN
               WRITE(NAMFLD,437)IBUF(IUFPTR)
            ELSE
               WRITE(NAMFLD,407)IBUF(IUFPTR),IBUF(IFLDPTR+16)
 407           FORMAT(2A2)
            END IF
            IF(MFBM.EQ.MFTOL) THEN
C     
C     SUSPECTED MISSING FIELD NAMES
C     
               PRINT 703, N,NAMFLD,NAMPRE
            END IF
            DO 450 I=1,NFLINP
               WRITE(CTEMP1,900)IFIELD(I)
               IF (NAMFLD.EQ.CTEMP1) THEN
                  READ (NAMFLD,401)ID(IDPTR),ID(IDPTR+1)
 401              FORMAT (2A4)
                  IFC=IFC+1
                  MTFIEL(IFC)=I
                  ITYP=ITPFLDC(NAMFLD)
                  ID(IDPTR+4)=IBUF(IFLDPTR+1)
C     
C     SPECIAL MODS FOR CRUMMY RADARS LIKE CHILL
C     
                  FACT=1.0
                  IF (ITYP.EQ.3 .AND. NMRAD.EQ.'CHIL') THEN
                     ID(IDPTR+4)=100
                     FACT=-100.0/IBUF(IFLDPTR+1)
                  ENDIF
                  ID(IDPTR+2)=0
                  IF (ITYP.EQ.1) THEN
                     IF (CFAC1.EQ.0.0) THEN
                     ID(IDPTR+2)=IBUF(IFLDPTR+19)*100.0/IBUF(IFLDPTR+1)
                     ELSE
                        ID(IDPTR+2)=CFAC1*100.
                     ENDIF
                  ELSE IF (ITYP.EQ.3) THEN
                     IF (VNYQ.EQ.0.0) THEN
                     ID(IDPTR+2)=IBUF(IFLDPTR+19)*100.0/IBUF(IFLDPTR+1)
                     ELSE
                        ID(IDPTR+2)=VNYQ*100.
                     ENDIF
                  WRITE(CTEMP3,934)IBUF(IFLDPTR+20)
 934              FORMAT(A2)
                  IF(CTEMP3.EQ.MFL) ID(IDPTR+2)= -ID(IDPTR+2)
                  ENDIF
                  IPF=IBUF(IFLDPTR)
                  NGFLD=IBUF(IFLDPTR+5)
                  K=IUNPAD
C     
C     CHECK FOR REPLACEMENT OF A FIELD BY AN ANALYTIC FUNCTION
C     
                  WRITE(CTEMP3,900)IFIELD(I)
 900              FORMAT(A8)
                  IF (FNUM.EQ.0 .OR. (FNUM.GT.0 .AND. RFNAM.NE.
     X                 CTEMP3)) THEN
                     DO 420 J=1,NRG
                        KOUT(KST+J)=IBAD
                        IF (J.LT.J1 .OR. J.GT.J2) GOTO 420
                        IF(J.GT.NGFLD) GO TO 420
                        IF (IBUF(IPF+K).EQ.IBUF(45) .or.
     X                      (ibuf(ipf+k).gt.20000 .and.
     X                       ibuf(ipf+k).lt.33000)) GOTO 410
                        X=IBUF(IPF+K)
                        Y=CVMGP(NINT(X-65536.),NINT(X),NINT(X-32768.0))
     X                       *FACT
                        KOUT(KST+J)=Y
 410                    CONTINUE
                        K=K+1
 420                 CONTINUE
                  ELSE IF (FNUM.GT.0 .AND. RFNAM.EQ.CTEMP3) THEN
C
C     REPLACE WITH ANALYTICAL FUNCTION
C
                     CALL ANLREP(FNUM,P1,P2,P3,P4,P10,NRG,KST,IBAD,J1,
     X                           J2,NGFLD,IBUF(IFLDPTR+1),K,
     X                           IBUF(34)*SCALE,IBUF(36)*SCALE,AZZ,
     X                           ICOPLANE,RJ1,DRG,XORR,YORR,ZORR)
                  ELSE
                     WRITE(*,*)'***INVALID STATE FOR REPLAC. IN UFNCAR'
                     STOP
                  END IF
                  IDPTR=IDPTR+5
                  KST=KST+NRG
               ENDIF
 450        CONTINUE
            IUFPTR=IUFPTR+2
            N=N+1
            IF (N.LE.IBUF(IPTD+2)) GOTO 400
            IF(IFC.NE.NFLINP) THEN
C     
C     MISSING FIELD - TOSS BEAM
C     
               MFBM=MFBM+1
               IF (MFBM.GT.MFTOL) CALL CHKMSG(7,NRAYS)
               PRINT 455,NSWPS,IBEAM
 455           FORMAT (5X,'*** MISSING FIELD IN SCAN ',I2,'  BEAM ',I4,
     X              ' - BEAM DISCARDED,STATISTICS FOR THIS SCAN ',
     X              'MAY BE AFFECTED')
               NRAYS=NRAYS-1
               IBEAM=IBEAM-1
               NBDBM=NBDBM+1
C     
C     GO GET ANOTHER RAY
C     
               GOTO 495
            END IF
            
            
C     
C     AT THIS POINT ALL FIELDS HAVE BEEN READ FROM TAPE INTO ARRAYS.
C     NOW WE DO THE ACTUAL THRESHOLDING.
C     
            
            IDPTRN=IDPTRSV
            KSTN=ID(37)
C     
C     LOOP OVER THE FIELDS READ FROM TAPE 
C     
            DO 999 K=1,NFLINP
               WRITE(CTEMP1,1030)ID(IDPTRN),ID(IDPTRN+1)
 1030          FORMAT(2A4)
               DO 555 I=1,NTHRSH
                  IF (TFIELD(1,I).EQ.CTEMP1) THEN
                     CTEMP2=TFIELD(2,I)
                     IF(CTEMP2.EQ.THON(1)) THEN
                        IKEY=1
                     ELSE IF (CTEMP2.EQ.THON(2)) THEN
                        IKEY=2
                     ELSE
                        WRITE(*,*)'***INVALID STATE IN RPNCAR***'
                     END IF
C     
C     GET UPPER AND LOWER LIMITS OF FIELD ON WHICH WE'RE THRESHOLDING
C     
                     TLLIMIT=TLIMITS(1,I)
                     TULIMIT=TLIMITS(2,I)
                     DO 777 J=1,NRG
                        IF (J.LT.J1 .OR. J.GT.J2) GOTO 777
                        IF (J.GT.NGFLD) GOTO 777
                        IF ((THVAL(IKEY,J).LT.TLLIMIT .OR. 
     X                       THVAL(IKEY,J).GT.TULIMIT) .AND. ISIDE(I)
     X                       .EQ.1) THEN
                           KOUT(KSTN+J)=IBAD
                           ITHR(I)=ITHR(I)+1
                        ELSE IF ((THVAL(IKEY,J).GE.TLLIMIT .AND.
     X                          THVAL(IKEY,J).LE.TULIMIT) .AND.
     X                          ISIDE(I).EQ.2) THEN
                           KOUT(KSTN+J)=IBAD
                           ITHR(I)=ITHR(I)+1
                        END IF
C     
C THIS NEXT ARRAY ELEMENT WILL CONTAIN THE TOTAL NUMBER OF POINTS IN VOL. SCAN
C     
                        IF (I.EQ.1) ITHR(MAXFLD+1)=ITHR(MAXFLD+1)+1
 777                 CONTINUE
                  END IF
 555           CONTINUE
               KSTN=KSTN+NRG
               IDPTRN=IDPTRN+5
 999        CONTINUE
            
            
 475        CONTINUE
            MFBM=0
C     
C     CHANGE INDEXING OF FIELDS
C     
            DO 480 I=1,NFLINP
               NTM(I)=IFIELD(I)
               ITM(I,1)=INTINF(I,1)
               ITM(I,2)=INTINF(I,2)
               ITM(I,3)=INTINF(I,3)
 480        CONTINUE
            DO 490 I=1,NFLINP
               INDEX=MTFIEL(I)
               IFIELD(I)=NTM(INDEX)
               INTINF(I,1)=ITM(INDEX,1)
               INTINF(I,2)=ITM(INDEX,2)
               INTINF(I,3)=ITM(INDEX,3)
 490        CONTINUE
C     
C     
            CALL RDRAY(NWDS,NST)
            IF (NST.GT.0) THEN
C     
C     READ ERROR
C     
               IALREAD=1
C     
C     GO PROCESS ERROR CONDITION
C     
               GOTO 495
            END IF
            IF (IBUF(9).GT.1) GOTO 400
            NLEN=NFLINP*NRG+ID(37)
c            NLEN=NFLINP*MAXRNG+ID(37)
            CALL WRRYDK(KPCK,KOUT,NST,LTMP,0,NLEN)
C     
C     *** BAD READ ***
C     
         ELSE IF (NST.NE.0) THEN
            IF (NST.EQ.1) IEOF=IEOF+1
C     
C     LIMIT NUMBER OF CONSECUTIVE END OF FILES TO 9
C     
            IF (IEOF.GE.10) THEN
               WRITE(*,*)'***TOO MANY EOFs--WRAPPING UP VOLUME***'
               IEOF=0
               GOTO 411
            END IF
C     
C     
C     IROV= -1 APPEND, 0 NORMAL PROCESSING, 1 RUNOVER
C     
            IF (IROV.EQ.0) THEN
               GOTO 411
            ELSE IF (IROV.GT.0 .AND. NST.EQ.3) THEN
C     
C     RUNSOVER ONTO NEXT TAPE
C     
               IROV=1-IABS(IROV)
               READ 503,KRD
 503           FORMAT (10A8)
               READ (KRD,504)KOMM
 504           FORMAT (A3)
               IF (KOMM.NE.'INP') THEN
                  PRINT 505
 505              FORMAT (5X,'***ERROR - ',
     X                 'INPUT CARD MUST FOLLOW RUNOVER PROCESS CARD')
                  STOP 3347
               ENDIF
               IJNK=1
               CALL INPFIL(KRD,NSTP,IJNK,AZCOR)
               IF (ISKP.GT.0) THEN
C
C     SKIP VOLUMES ON INPUT UNIT BEFORE PROCESSING
C
                  CALL SKPVOL(IUN,ISKP)
                  ISKP=0
               END IF

            ELSE IF (IROV.EQ.1) THEN
C     
C     HANDLE RUNOVER CASE
C     
               CALL NWRAY(NWDS,MBYTE,NST)
               IBUF(33)=INORM(IBUF(33))
               IF (NST.EQ.3) THEN
                  IALREAD=1
                  GOTO 495
               ELSE
                  GOTO 411
               END IF
            END IF
 507        CONTINUE
C     
C     CHECKS ON NEW VOLUME
C     
            NST=0
            OLDAZ=PAZ
            OLDEL=ELSAV
            LASTIM=ID(7)*10000 + ID(8)*100 + ID(9)
            IDIR=1
            IF (DIR.LT.0) IDIR=-1
            IKAZ=0
            IKNT=0
 510        CONTINUE
            DO 515 ILP1=1,1000000
C     
C     BODY OF REPEAT LOOP
C     
               IF(NST.EQ.3) GO TO 411
               CALL NWRAY(NWDS,MBYTE,NST)
               IBUF(33)=INORM(IBUF(33))
               IF (NST.EQ.3 .AND. IROV.EQ.-1) GOTO 411
               IF (NST.EQ.3 .AND. IROV.GT.0) THEN
                  IALREAD=1
                  GOTO 495
               END IF
               IF (NST.GT.0) THEN
                  IJ=1
                  GOTO 250
               ELSE
                  IJ = 0
               END IF
               IF (IKAZ.LE.0) THEN
                  IF (IKNT.EQ.0) THEN
                     IFUT=IBUF(32)
C                     CALL SHILBL(IFUT,1)
                     LFTIM=0
C     MARK BRADFORD PATCH TO REMOVE CONVERSION TO MDT
C     IF (IFUT.EQ.'UT') LFTIM=-6
                  ENDIF
                  IHR=IBUF(29)+LFTIM
                  IF (IHR.LT.0) IHR=IHR+24
                  IFTIM=10000*IHR + 100*IBUF(30) + IBUF(31)
                  IF(IFTIM.GT.KETIM) THEN
C     
C     ENDING PROCESS TIME SURPASSED WHILE ATTEMPTING AN APPEND.
C     WRAP UP CURRENT SCAN AND TERMINATE THE PROCESSING OF THE VOLUME.
C     
                     BACKSPACE IUN
                     IROV=0
                     NST=1
                     GO TO 411
                  END IF
                  IF (IKNT.EQ.0) PRINT 511,IFTIM
 511              FORMAT (5X,'     APPEND VOLUME AT TIME ',I6)
                  IKNT=IKNT+1
                  IF (IFTIM.LT.LASTIM) GOTO 515
C     
C     NORMALIZE ELEVATION/CHECK FOR NEGATIVES
C     
                  IBUF(36)=INORM(IBUF(36))
                  IBUF(36)=INANG(IBUF(36))
                  CUREL=IBUF(36)*SCALE
                  IF (ABS(CUREL-OLDEL).GT.FXSTOL) THEN
                     IF (NST.NE.0) THEN
                        WRITE(*,*)'***BAD STATE IN UFNCAR***'
                        STOP
                     END IF
                     IALREAD=1
                     GOTO 495
                  END IF
               END IF
 512           CONTINUE
               IKAZ=IKAZ+1
               ISWP=IBUF(10)
               IF (ICOPLANE.LT.4) THEN
                  CURAZ=IBUF(33)*SCALE + AZCOR
                  IF (CURAZ.GT.360.0) CURAZ=CURAZ-360.0
                  IF (CURAZ.LT.0.0) CURAZ=CURAZ+360.0
               ELSE IF (ICOPLANE.EQ.4) THEN
                  CURAZ=IBUF(34)*SCALE
               END IF
               IF (((CURAZ-OLDAZ)*IDIR).GT.0.0) THEN
                  IF (NST.NE.0) THEN
                     WRITE(*,*)'***BAD STATE IN UFNCAR***'
                     STOP
                  END IF
                  IALREAD=1
                  GOTO 495
               END IF
C     
C     UNTIL CONDITION
C     
 515        CONTINUE
         END IF
C     
C     THE FOLLOWING CONTINUE ENDS THE BIG LOOP OVER THE RAYS
C     
 495  CONTINUE
      
 411  CONTINUE
C     
C     WRAP UP THIS VOLUME AND ELEVATION SCAN
C     
      IF (IBEAM.LT.MNBEM .AND. IFXTAB.NE.1) THEN
c         CALL WRRYDK(KPCK,KOUT,INST,LTMP,-9,0)
         IF (IBEAM.GT.0) THEN
            CALL WRRYDK(KPCK,KOUT,INST,LTMP,(NSWPS+11),IBEAM)
            WRITE(*,234)NSWPS+1,IBEAM
         END IF
         NRAYS=NRAYS-IBEAM
      ELSE
         ELFIX=ELSAV
C     
C     RE-COMPUTE FIXED ANGLE IF REQUESTED
C     
         IF(IELCHK) THEN
            I1=IGNEL+1
            I2=IBEAM-IGNEL
            IF(I2.LT.I1) THEN
               I1=IBEAM/2+1
               I2=I1
            END IF
            SUM=0.0
            DO 421 I=I1,I2
               SUM=SUM+ELSCAN(I)
               J=NINT(ELSCAN(I)*10.0)+1
               IF(J.LT.1.OR.J.GT.MXL) GO TO 421
               ELSCAN(MXSCN+J)=ELSCAN(MXSCN+J)+1.0
 421        CONTINUE
            IF(IELT.EQ.1) THEN
               MODE=0
               ELMOD=0.0
               DO 422 I=1,MXL
                  IF(ELSCAN(MXSCN+I).LE.ELMOD) GO TO 422
                  MODE=I
                  ELMOD=ELSCAN(MXSCN+I)
 422           CONTINUE
               IF(MODE.NE.0) ELFIX=(MODE-1)*0.1
            ELSE IF (IELT.EQ.2) THEN
               ELFIX=SUM/(I2-I1+1)
            END IF
         END IF
         ID(IPTR)=NINT(ELFIX*ID(44))
         IF (ICOPLANE.EQ.4 .AND. .NOT.IELCHK .AND. 
     X        AZCOR.NE.0.0) THEN
C
C     ADJUST FIXED ANGLE BY AZIMUTH CORRECTION TOO FOR RHIS
C
            ELFIX=ELFIX+AZCOR
            ID(IPTR)=NINT(ELFIX*ID(44))
         END IF
         ID(IPTR+1)=SIGN(1.0,DIR)
C
C     REVERSE DIRECTION OF SCAN FOR RHI SCANS
C
         IF (ICOPLANE.EQ.4 .AND. ((ELFIX.LT.45.0 .AND.
     X        ELFIX.GE.0) .OR. (ELFIX.LT.315. .AND.
     X        ELFIX.GE.225.0) .OR. (ELFIX.LT.405 .AND.
     X        ELFIX.GE.315))) ID(IPTR+1)=-ID(IPTR+1)
         ID(IPTR+2)=NRAYS
         NSWPS=NSWPS+1
         IF (NSWPS.GT.MAXEL) THEN
            PRINT 471,NSWPS
 471        FORMAT (5X,'***ERROR - CANNOT HAVE ',I2,' SWEEPS')
            STOP 471
         ENDIF
         IF (ISKIPFX.EQ.0) THEN
            DELAZ=ABS(DIR)/(IBEAM-1)
            DELEL=ELSUM/IBEAM
         ELSE
            DELAZ=0.0
            DELEZ=0.0
         END IF
         PRINT 425,NSWPS,ID(IPTR+1),ELSAV,ELMIN,ELMAX,DELEL,BEGAZ,
     X        PAZ,AZMIN,AZMAX,DELAZ,IBEAM,NBDBM
 425     FORMAT(2(4X,I2),8X,4(F6.2,3X),4X,2(F6.1,4X),
     X        3(F6.2,3X),I7,I6)
         IPTR=IPTR+3
      ENDIF
C     
C     DO END OF VOLUME PROCESSING
C     
 500  CONTINUE
C     
C     VOLUME IS FINISHED
C     
      CALL WRRYDK(KPCK,KOUT,INST,LTMP,9,0)
      IF (NSWPS.LT.2 .AND. IPPI.EQ.0) THEN
         PRINT 501,NSWPS
 501     FORMAT(5X,'*** VOLUME DISCARDED - ONLY ',I1,
     X        ' ELEVATION SCANS')
C     
C     GO FIND A NEW VOLUME
C     
         GOTO 200
      ENDIF
      J=129
      NSWPM1 = NSWPS - 1
      ILSCHK=.FALSE.
      LAST=ID(J)
      DIFFE=(IABS(ID(J)-ID(J+3)))
      DO 600 I=1,NSWPM1
         K=J+3
         IF (ICOPLANE.EQ.1) THEN
            DIFFH=(IABS(ID(K)-LAST))
            IF (ABS(DIFFH-DIFFE).GT.(.2*ID(44))) THEN
               WRITE(*,3000)
 3000          FORMAT(5X,'***ERROR: COPLANE ANGLES OF DATA ARE NOT',
     X                'UNIFORMLY SPACED. REINTERPOLATE AND SPECIFY',
     X                'TARGET ANGLES EXPLICITLY WITH GRIDCPL CARD. ')
               STOP
            END IF
         END IF
         IF(ID(K).LE.LAST) THEN
            ILSCHK=.TRUE.
            GO TO 601
         END IF
         LAST=ID(K)
         J=K
 600  CONTINUE
 601  CONTINUE
      ID(36)=NRAYS
      ID(35)=NSWPS
      REWIND LTMP
      IELCHK=IELCHK.OR.ILSCHK
      IF (IFXTAB.EQ.1 .AND. (ISKIP.EQ.1 .OR. IACCPT.EQ.1)) IELCHK=.TRUE.
      CALL GENAZM(JPCK,IELCHK,MNBEM,ELTOL,NAST,ICOPLANE,BASANG)
      IF (NAST.NE.0) THEN
         PRINT 602
 602     FORMAT(5X,'*** VOLUME DISCARDED - INPUT DATA STRUCTURE ',
     X        'CANNOT BE INTERPOLATED.')
C     
C     GO FIND A NEW VOLUME
C     
         GOTO 200
      ENDIF
      CALL SETVOL
C     
C     PRINT OUT NUMBER OF DATA POINTS SET TO BAD DUE TO THRESHOLDING
C     
      WRITE(*,*)' '
      IF (NTHRSH.GT.0) THEN
         WRITE(*,730)
 730     FORMAT(15X,'POINTS SET TO BAD DUE TO INSIDE/OUTSIDE TESTS')
         WRITE(*,740)
 740     FORMAT(15X,'------ --- -- --- --- -- -------------- -----')
         WRITE(*,750)
 750     FORMAT(/,10X,'FIELD        % SET TO BAD  # SET TO BAD  ',
     X        'TOTAL # OF POINTS')
         WRITE(*,760)
 760     FORMAT(10X,'---------    ------------  ------------  ',
     X        '-----------------')
      END IF
      DO 700 I=1,NTHRSH
         THRPER=(FLOAT(ITHR(I))/FLOAT(ITHR(MAXFLD+1)))*100
         WRITE(*,710)TFIELD(1,I),THRPER,ITHR(I),ITHR(MAXFLD+1)
 710     FORMAT(10x,A8,7x,F8.1,7x,I8,6x,I8)
         ITHR(I)=0
 700  CONTINUE
      WRITE(*,*)' '
      NUFST=0

C
C     WRITE OUT THE NUMBER OF BEAMS DISCARDED DUE A CHANGE IN GATE
C     SPACING AND THE NUMBER THROWN OUT DUE TO A CHANGE IN THE # OF GATES
C
      IF (IDRGCHG.GT.0) THEN
         WRITE(*,2050)IDRGCHG
 2050    FORMAT(/,I5,' BEAMS DISCARDED DUE TO A CHANGE IN THE GATE ',
     X          'SPACING',/)
      END IF
      IF (INRNG.GT.0) THEN
         WRITE(*,2060)INRNG
 2060    FORMAT(/,I5,' BEAMS DISCARDED DUE TO A CHANGE IN THE # OF ',
     X          'RANGE GATES',/)
      END IF
         
      RETURN
      END
