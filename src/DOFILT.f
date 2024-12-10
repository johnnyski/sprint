      SUBROUTINE DOFILT(DATA,IFLDS,OFLDS,IFLTYP,IPROC,FSPACE,C3,C4,
     X                  NSCTP,NMFILT,ICOPLANE,ICART,NRCBF,NPREC,
     X                  MXCRT,IFLAT,ICTAB,DASANG,IFLTALL)
C
C     THIS SUBROUTINE PERFORMS 2-D FILTERING OF THE DATA BEFORE
C     INTERPOLATION. IT READS ALL THE DATA FOR A SINGLE SWEEP INTO
C     AN ARRAY AND FILTERS THE BEAMS, ONE BY ONE. AFTER EACH BEAM IS
C     FILTERED, IT SENDS IT OUT TO A NEW SCRATCH UNIT ON DISK.
C
C     DATA    -  WILL HOLD THE DATA FOR A SINGLE SWEEP
C     FFLDS   -  NAMES OF FIELDS TO BE FILTERED
C     VNYQ    -  NYQUIST VELOCITY TO BE USED FOR LOCAL UNFOLDING
C     IFLTYP  -  FILTER WEIGHTS (UNIFORM, TRIANGULAR, CRESSMAN,
C                QUADRATIC, OR EXPONENTIAL)
C     IPROC   -  INDICATES ANY SPECIAL PROCESSING (LINEAR, UNFOLD)
C     DRG,DA   -  RANGE GATE SPACING (KM), AVERAGE ANGLE SPACING (DEG)
C     FSPACE  -  FILTER SPACE('RADR' OR 'CART')
C     C3,C4   -  FILTER DIMENSIONS: RADR - (2*C3+1) GATES BY (2*C4+1)BEAMS
C                                   CART - LINEAR RADII (KM BY KM)
C     BDVAL   -  BDVAL FOR FIELDS
C     NSCTP   -  IF 'FILL' THEN FILL IN WHEN CENTRAL POINT IS BDVAL
C     NUMFILT -  NUMBER OF FIELDS TO FILTER
C     ICOPLANE-  TYPE OF SCAN AND OUTPUT COORDINATE SYSTEM
C     DDX,DDY -  RADIUS OF THE FILTER IS SQRT(DDX*DDX+DDY*DDY)
C                RADR - [DDX=(C3+1)*DRG, DDY=(C4+1)*DA*TORAD*RANGE]
C                CART - [DDX=C3,        DDY=C4]
C     DXX,DYY -  TOTAL DIMENSIONS OF FILTER AREA
C                RADR - [DXX=2*C3+1, DDY=2*C4+1] (GATES BY BEAMS)
C                CART - [DXX=2*C3,   DYY=2*C4]   (KM BY KM)
C
C     MAXBM   -  MAXIMUM NUMBER OF BEAMS THAT CAN BE STORED IN MEM AT ONCE
C
      PARAMETER(MAXRNG=768,MAXBM=51,MAXFLD=8,NID=296)
      PARAMETER(MAXYZ=512000)
      PARAMETER(MAXVAL=(MAXRNG+10)*MAXFLD)
      PARAMETER(BDVAL=-32768.)
      DIMENSION DATA(MAXVAL,MAXBM),ISPEC(MAXFLD)
      DIMENSION DMEA((MAXRNG+10)),DWT(MAXRNG+10),RNG(MAXRNG+10),
     X     XX(MAXRNG+10),TMP(MAXRNG+10)
      DIMENSION YY(MAXRNG+10),SMALR(MAXRNG+10),FAC(MAXRNG+10)
      DIMENSION KFAC(MAXRNG+10)
      DIMENSION KPCK2(85000)
      CHARACTER*8 IFLDS(MAXFLD),OFLDS(MAXFLD),NSCTP(MAXFLD)
      CHARACTER*8 FSPACE(MAXFLD),IPROC(MAXFLD),NAMTIM
      COMMON /IDBLK/  ID(NID)
      COMMON /SCRDSK/ LTMP
      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /FILT/ ISTFLG,NRAYS,IDIR,C,NLEN,NUMFILT,INPFLD(MAXFLD),
     X              SCLFLD(MAXFLD),MTMP
      COMMON /IO/KPCK(85000),KOUT(8500),IBUF(8500),NBUF(2125,4),
     X     IZ8(17),ILSTREC

      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ
     X     ,RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL

      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      DIMENSION IFLTYP(MAXFLD),VNYQUIST(MAXFLD),C3(MAXFLD),C4(MAXFLD)
      DIMENSION BMOUT(MAXVAL)
      INTEGER CNT,CNTMN,RCNT
      CHARACTER*8 CTEMP1
      DATA NAMTIM/'TIME'/
      DATA TORAD,CNTMN,EPS/0.017453293, 3, 1.0E-6/
C
C     THE FOLLOWING FUNCTION CALCULATES HOR. AZ. FROM COP. ANG. AND AZ. IN COP
C
      AZBM(C,A)=ATAN(COS(C*TORAD)*TAN(A*TORAD))/TORAD

      DATMXSIZ=NRCBF*8.
C
C     TRANSFER NUMBER OF FILTERS TO COMMON BLOCK VARIABLE
C
      NUMFILT =NMFILT
C
C     ENCODE CHAR. VARS AS INTS FOR VECTORIZATION PURPOSES
C
      DO 10 I=1,MAXFLD
         ISPEC(I)=0
         IF (IPROC(I).EQ.'UNFOLD') ISPEC(I)=1
         IF (IPROC(I).EQ.'LINEAR') ISPEC(I)=2
 10   CONTINUE

      NFLINP=NFLDS
      IF (IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
      IF (NUMFILT.GT.0) THEN
C
C     CLEAR ARRAY TO ZEROES
C
         DO 50 I=1,MAXBM
            DO 60 J=1,MAXVAL
               DATA(J,I)=0.0
 60         CONTINUE
 50      CONTINUE
         NSWPS=ID(35)
         NRHD=ID(37)
         NLEN=NRHD+(ID(75)*MAXRNG)
         NFLDS=ID(75)
         NOFLDS=NFLDS
C     
C     SCRATCH DISK IS UNIT 3
C
         MTMP=3
         WRITE(MTMP) MTMP
         REWIND MTMP
         
C
C     IDENTIFY ADDRESSES IN BEAMS AND SCALE FACTORS OF INPUT FIELDS
C
         WRITE(*,37)
 37      FORMAT(/5X,'FILTERING ABOUT TO BEGIN FOR FIELDS:'/)
         DO 55 I=1,NUMFILT
            WRITE(*,39)IFLDS(I)
 39         FORMAT(5X,A8)
            DO 75 J=1,NFLDS
               WRITE(CTEMP1,80)ID(76+(J-1)*5),ID(77+(J-1)*5)
 80            FORMAT(2A4)
               IF (CTEMP1.EQ.IFLDS(I)) GOTO 90
 75         CONTINUE
 90         IF (J.LE.NFLDS .AND. J.GT.0) THEN
               INPFLD(I)=ID(37) + (J-1)*MAXRNG + 1
               SCLFLD(I)=ID(80+(J-1)*5)
               VNYQUIST(I)=ID(78+(J-1)*5)/100.0
            ELSE
               WRITE(*,120)IFLDS(I)
 120           FORMAT(//,5X,'+++ERROR LOCATING FIELD:',A8,' FOR ',
     X                'FILTERING+++')
               STOP
            END IF
 55      CONTINUE

         NRAYS=0
         CALL WRRYDK(KPCK2,KOUT,NST,MTMP,-9,NLEN)
         DO 100 I=1,NSWPS
            IF (I.EQ.1) THEN
               NRAYS=ID(131)
            ELSE
               NRAYS=ID(131+3*(I-1))-ID(131+3*(I-2))
            END IF

C
C     READ IN FIRST CHUNK OF BEAMS
C
            IOP=0
            CALL LOADBMS(IOP,I,DATA)
            NREAD=MIN(MAXBM,NRAYS)
C     
C     CALCULATE AVERAGE HORIZONTAL AZIMUTH SPACING
C     
            IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4) 
     X           THEN
               AZ1=DATA(1,1)
               AZ2=DATA(1,MIN(MAXBM,NRAYS))
            ELSE IF (ICOPLANE.EQ.1 .OR. ICOPLANE.EQ.2) THEN
               AZ1=AZBM(C,DATA(1,1))
               AZ2=AZBM(C,DATA(1,MIN(MAXBM,NRAYS)))
            END IF
            IF (IDIR.EQ.1 .AND. (AZ2.GE.AZ1)) THEN
               DIFF=AZ2-AZ1
            ELSE IF (IDIR.EQ.-1 .AND. (AZ2.GE.AZ1)) THEN
               DIFF = 360.0 - (AZ2-AZ1)
            ELSE IF (IDIR.EQ.1 .AND. (AZ2.LT.AZ1)) THEN
               AZ2=AZ2+360.0
               DIFF = (AZ2-AZ1)
            ELSE
               AZ2=AZ2+360.0
               DIFF = 360.0 - (AZ2-AZ1)
            END IF
            DA=ABS(DIFF)/MIN(MAXBM,NRAYS)
            

C
C     NOW DO THE FILTERING
C     
            INDXBM=0
            DO 500 J=1,NRAYS
               INDXBM=INDXBM+1
               IF (INDXBM.GT.MAXBM) THEN
C
C     HANDLES CASE FOR RANGE ONLY FILTERING 
C
                  IOP=1
                  CALL LOADBMS(IOP,I,DATA)
                  NREAD=NREAD+1
                  INDXBM=INDXBM-1
               END IF
               IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. ICOPLANE.EQ.4)
     X              THEN
                  SINIJ=SIN(DATA(1,INDXBM)*TORAD)
                  COSIJ=COS(DATA(1,INDXBM)*TORAD)
               ELSE
C     
C     GET HORIZONTAL AZIMUTH 
C     
                  IF (DATA(1,INDXBM).NE.90.0) THEN
                     AZ=AZBM(C,DATA(1,INDXBM))
                  ELSE
                     AZ=90.0
                  END IF
                  SINIJ=SIN(AZ*TORAD)
                  COSIJ=COS(AZ*TORAD)
               END IF
               DO 540 INIT=1,NLEN
                  BMOUT(INIT)=DATA(INIT,INDXBM)
 540           CONTINUE
               NRG=INT(DATA(8,INDXBM))
               DO 575 LR=1,NRG
C
C     SET ALL FIELDS TO BE FILTERED TO BAD INITIALLY
C
                     DO 525 K=1,NUMFILT
                        BMOUT(LR+INPFLD(K)-1)=BDVAL
 525                 CONTINUE
                     RIJ=RG1 + (LR-1)*DRG
                     XIJ=RIJ*SINIJ
                     YIJ=RIJ*COSIJ
                     IF (RIJ.LE.0.0) GOTO 575
                          
                     IF (FSPACE(1).EQ.'RADR') THEN
                        IDEL=NINT(C3(1))
                        IF (IDEL.LE.0) THEN
                           DDX=0.0
                        ELSE
                           DDX=(C3(1)+1.0)*DRG
                        END IF
                        JDEL=NINT(C4(1))
C
C     LIMIT JDEL TO HALF THE MAX. # OF BEAMS IN MEMORY AT A TIME
C
                        JDEL=MIN(JDEL,(MAXBM-1)/2)
                        IF (JDEL.LE.0) THEN
                           DDY=0.0
                        ELSE
                           DDY=(C4(1)+1.0)*DA*TORAD*RIJ
                        END IF
                     ELSE
                        IDEL=NINT(C3(1)/DRG)
                        IF (IDEL.LE.0) THEN
                           DDX=0.0
                        ELSE
                           DDX=C3(1)
                        END IF
                        JDEL=NINT(C4(1)/(RIJ*DA*TORAD))
                        JDEL=MIN(JDEL,(MAXBM-1)/2)
                        IF (JDEL.LE.0) THEN
                           DDY=0.0
                        ELSE
                           DDY=C4(1)
                        END IF
                     END IF
                     
                     I1=LR-IDEL
                     I2=LR+IDEL
                     IF (I1.LT.1) I1=1
                     IF (I2.LT.1) I2=1
                     IF (I1.GT.NRG) I1=NRG
                     IF (I2.GT.NRG) I2=NRG
                     I1SAV=I1
                     I2SAV=I2
                     
                     J1=INDXBM-JDEL
                     J2=INDXBM+JDEL
                     IF (J1.LT.1) J1=1
                     IF (J2.LT.1) J2=1
                     IF (J1.GT.NRAYS) J1=NRAYS
                     IF (J2.GT.NRAYS) J2=NRAYS
                     IF (J2.GT.MAXBM) THEN
                        IF (NREAD.LT.NRAYS) THEN
                           IOP=1
                           CALL LOADBMS(IOP,I,DATA)
                           NREAD=NREAD+1
                           INDXBM=INDXBM-1
                           J1=J1-1
                           J2=J2-1
                           IF (J1.LT.1 .OR. J2.GT.MAXBM .OR. 
     X                          J2.GT.NRAYS) THEN
                              WRITE(*,65)J1,J2,MAXBM,NRAYS
 65                           FORMAT(/,5X,' +++INVALID STATE IN DOFILT.'
     X                            ,' CONTACT PROGRAMMER+++')
                              STOP
                           END IF
                        ELSE
                           J2=MAXBM
                        END IF
                     END IF
                     BIGR=DDX*DDX+DDY*DDY
                     IF (BIGR.EQ.0.0) THEN
                        WRITE(*,610)
 610                    FORMAT(//,5X,'+++ERROR: CHECK FILTER SPECS,',
     X                       'BIGR IS ZERO+++'/)
                        STOP
                     END IF
                     DO 550 K=1,NUMFILT
                        L=LR+INPFLD(K)-1
                        IOP=1
                        IF (DATA(L,INDXBM).EQ.BDVAL .AND. 
     X                       NSCTP(K).NE.'FILL') GOTO 550
                        IF (ISPEC(K).EQ.1 .AND. DATA(L,INDXBM)
     X                       .NE.BDVAL) THEN
                           VEST=DATA(L,INDXBM)
                        ELSE
                           VEST=0.0
                        END IF
c                        I1=I1SAV+INPFLD(K)-1
c                        I2=I2SAV+INPFLD(K)-1
C     
C     EXECUTE THE INNER SUMMATIONS TO OBTAIN WEIGHTED SUM OF
C     SAMPLES WITHIN SQRT(BIGR) OF THE CURRENT (I,J) SAMPLE POINT.
C     
                     CNT=0
                     SUMW=0.0
                     WSUM=0.0
                     DO 600 JJ=J1,J2
                        IOP=1
                        IF (ICOPLANE.EQ.0 .OR. ICOPLANE.EQ.3 .OR. 
     X                       ICOPLANE.EQ.4) THEN
                           SINA=SIN(DATA(1,JJ)*TORAD)
                           COSA=COS(DATA(1,JJ)*TORAD)
                        ELSE
                           IF (DATA(1,JJ).NE.90.0) THEN
                              AZ=AZBM(C,DATA(1,JJ))
                           ELSE
                              AZ=90.0
                           END IF
                           SINA=SIN(AZ*TORAD)
                           COSA=COS(AZ*TORAD)
                        END IF
                        RCNT=0
                        DO 620 II=I1,I2
                           TMP(II)=DATA(II+INPFLD(K)-1,JJ)
                           IF (TMP(II).NE.BDVAL) THEN
                              RNG(II)=RG1+(II-1)*DRG
                              XX(II)=RNG(II)*SINA
                              YY(II)=RNG(II)*COSA
                              SMALR(II)=(XX(II)-XIJ)**2+
     X                             (YY(II)-YIJ)**2
                            IF (SMALR(II).LE.BIGR .AND. RNG(II).GT.0.0)
     X                             THEN
C     
C     PROCEED WITH SUMMATION OF (II,JJ) LOCATIONS SURROUNDING CURRENT
C     LOCATION.
C     
                           IF (ISPEC(K).EQ.2) THEN
                              DMEA(II)=10.0**(0.1*TMP(II))
                           ELSE IF (ISPEC(K).EQ.1) THEN
                              IF(ABS(TMP(II)-VEST).GT.VNYQUIST(K))
     X                             THEN
                                 FAC(II)=(VEST-TMP(II))/
     X                                (2.0*VNYQUIST(K))
                                 IF(FAC(II).GE.0.0)KFAC(II)=FAC(II)+0.5
                                 IF(FAC(II).LT.0.0)KFAC(II)=FAC(II)-0.5
                                 DMEA(II)=TMP(II)+2.0*KFAC(II)*
     X                                VNYQUIST(K)
                              ELSE
                                 DMEA(II)=TMP(II)
                              END IF
                           ELSE
                              DMEA(II)=TMP(II)
                           END IF
                           
                           IF (IFLTYP(K).EQ.1) THEN
                              DWT(II)=1.0
                           ELSE IF (IFLTYP(K).EQ.2) THEN
                              DWT(II)=1.0 - SQRT(SMALR(II)/BIGR)
                           ELSE IF (IFLTYP(K).EQ.3) THEN
                              DWT(II)=(BIGR-SMALR(II))/(BIGR+SMALR(II))
                           ELSE IF (IFLTYP(K).EQ.4) THEN
                              DWT(II)=1-(SMALR(II)/BIGR)
                           ELSE
                              DWT(II)=EXP(-4.0*SMALR(II)/BIGR)
                           END IF
                           RCNT=RCNT+1
                           CNT=CNT+1
c     WSUM=WSUM+DMEA*DWT
                           WSUM=WSUM+DMEA(II)*DWT(II)
                           SUMW=SUMW+DWT(II)
                        ENDIF
                     ENDIF
 620              CONTINUE
 600           CONTINUE
               
               IF (CNT.LT.CNTMN .OR. SUMW.LT.EPS) GOTO 550
               BMOUT(L)=WSUM/SUMW
C     
C     UNDO ANY SPECIAL PRE-FILTER TRANSFORMATION
C     
               IF (ISPEC(K).EQ.2) THEN
                  IF (BMOUT(L).GT.0.0) THEN
                     BMOUT(L)=10.0*ALOG10(BMOUT(L))
                  ELSE
                     BMOUT(L)=BDVAL
                  END IF
               END IF
 550        CONTINUE
 575     CONTINUE
C     
C     A BEAM IS FINISHED; SCALE THINGS BACK
C     
         NRG=NINT(BMOUT(8))
         DO 700 IF=1,NUMFILT
            ISTART=INPFLD(IF)
            DO 725 IRN=1,NRG
               IF (BMOUT(ISTART+IRN-1).NE.BDVAL) THEN
                  BMOUT(ISTART+IRN-1)=(BMOUT((ISTART+IRN-1))*
     X                 SCLFLD(IF))
               END IF
 725        CONTINUE
 700     CONTINUE
         
C     
C     SCALE AZIMUTH
C     
         KOUT(1)=NINT(BMOUT(1)*64.)
C
C     TRANSFER TO KOUT AND "COMPRESS" BACK TO ACTUAL NUMBER OF GATES
C
         DO IT=2,ID(37)
            KOUT(IT)=NINT(BMOUT(IT))
         END DO
         DO IT=1,NFLINP
            ISTRT=(IT-1)*KOUT(8) + ID(37)
            JSTRT=(IT-1)*MAXRNG  + ID(37)
            DO K=1,NRG
               KOUT(ISTRT+K)=NINT(BMOUT(JSTRT+K))
            END DO
         END DO
C     
C     WRITE OUT BEAM
C     
         NLEN=ID(37) + NFLINP*(KOUT(8))
         CALL WRRYDK(KPCK2,KOUT,NST,MTMP,0,NLEN)
         

 500  CONTINUE
 100  CONTINUE
C
C     FLUSH LAST RECORD TO DISK
C
      CALL WRRYDK(KPCK2,KOUT,NST,MTMP,9,NLEN)
C     
C     SWAP FILE POINTERS
C     
      LTMP=MTMP
      REWIND LTMP
      

      END IF
      
      END 
