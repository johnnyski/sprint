      SUBROUTINE INPFIL(KRD,INPTST,IAZFLG,AZCOR)
C+++++LISTING ENABLED+++++
C     CDIR$ LIST
C
C        FILLS COMMON /CINP/
C        FORMAT OF INP CARD
C
C
C   VARIABLE  DESCRIPTION                  FIELD  NOTES
C   --------  -----------                  -----  -----
C
C   KOMM      'INP'                         P1    COMMAND
C   IUN       UNIT NUMBER                   P2
C   ITAP      TAPE NAME                     P3
C   ISKP      FILES TO SKIP BEFORE READING  P4
C   IEXP      EXPERIMENT NUMBER             P5    1- CCOPE (DEFAULT)
C                                                 2- JAWS
C                                                 3- CYCLES
C                                                 4- MAYPOLE-83
C                                                 5- LAKE SNOW-84
C                                                 6- MAYPOLE-84
C                                                 7- PHOENIX-84
C                                                 8- NIMROD-78
C                                                 9- SOCORRO-84
C                                                10- PRESTORM-85
C                                                11- GALE-86
C                                                12- MIST-86
C                                                13- CINDE-87
C                                                14- GERMAN
C                                                15- TAMEX
C                                                16- PROFS
C                                                17- TDWR
C                                                18- HARP
C                                                19- WISP90
C                                                20- WISP91
C                                                21- CAPE
C                                                22- FEST92
C                                                23- TOGA COARE
C                                                24- TRMM
C                                                25- NEXRAD
C   NMRAD     RADAR NUMBER                  P6    1 = CHILL
C                                                 2 = CP-2
C                                                 3 = CP-3
C                                                 4 = CP-4
C                                                 5 = NOAA-C
C                                                 6 = NOAA-D
C                                                 7 = NOAA-K
C                                                 8 = SWR-75
C                                                 9 = FL-2
C                                                10 = UND
C                                                11 = NOR
C                                                12 = CIM
C                                                13 = POLD
C                                                14 = TOGA
C                                                15 = CCAA
C                                                16 = SPAN
C                                                17 = MIT
C                                                18 = MHR
C                                                19 = ELDR-AFT
C                                                20 = ELDR-FOR
C                                                21 = MELB
C                                                22 = DARW-TOGA
C                                                23 = DARW-LAS
C                                                24 = CAMANO
C                                                25 = WICHITA
C                                                26 = KWAJELEIN
C                                                27 = EUREKA
C                                                28 = OKLA CITY
C                                                29 = TAMPA-BAY
C                                                30 = MIAMI
C                                                31 = JACKSONVILLE
C                                                32 = KEY-WEST
C                                                33 = TALLAHASSEE
C                                                34 = HOUSTON
C                                                35 = CORPUS-CRISTI
C                                                36 = BROWNSVILLE
C                                                37 = GRANGER AFB
C                                                38 = NEW BRAUNFELS
C                                                39 = TEXAS A&M ADRAD
C                                                40 = LAKE-CHARLES
C                                                41 = ISRAEL
C                                                42 = OM KOI (THAILAND)
C                                                43 = PHUKET (THAILAND)
C                                                44 = DARW-GUNN-PT
C                                                45 = HAWAII
C                                                46 = SAO-PAULO (BRAZIL)
C                                                47 = GUAM
C                                                48 = TAIWAN
C                                                49 = NOAA, RON H. BROWN
C                                                50 = EUREKA
C                                                51 = AMARILLO
C                                                52 = ST. LOUIS
C                                                53 = NORFOLK
C                                                54 = SANTA ANA MTNS
C                                                55 = LOS ANGELES
C                                                56 = STERLING
C                                                57 = DOVER AFB
C
C
      PARAMETER (NRMX=188,NID=296,NEXP=25,MAXEL=80)
      DIMENSION HTRAD(NRMX,NEXP),XCOORD(NRMX,NEXP),YCOORD(NRMX,NEXP),
     X     AZCORT(NRMX,NEXP)
      CHARACTER*4 NAMES(NRMX)

      COMMON /CINP/IUN,ISKP,ORLAT,ORLON
      COMMON /CINPC/ NMRAD,ITAP
      CHARACTER*4 NMRAD
      CHARACTER*8 ITAP

      COMMON /AIRBRN/ ALATS(MAXEL),ALONS(MAXEL),IALTFLG
      COMMON /IDBLK/ID(NID)
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM
      CHARACTER*8 KRD(10)
      CHARACTER KEXP(NEXP+1)*12
      CHARACTER*3 REWSTR
C--------0---------0---------0---------0---------0---------0---------0--
      DATA HTRAD/815.,788.,768.,812.,815.,837.,812.,802.,180*0.,
     X           0.,1602.,1570.,1619.,184*0.,
     X                                 188*0.,
     X             0.,1602.,           186*0.,
     X             0.,0.,213.,209.,    184*0.,
     X             0.,1750.,0.,1540.,  184*0.,
     X             0.,0.,1598.,1540.,1598.,1513., 182*0.,
     X             241.,0.,228.,200., 184*0.,
     X             0.,0.,1783.,2136.,3214.,1802., 182*0.,
     X             0.,0.,489.,438., 6*0., 370.,413.,176*0.,
     X             0.,0., 10., 10., 11*0., 100., 30., 171*0.,
     X             0.,264.,184.,205., 4*0., 202.,213., 178*0.,
     X             0.,1750.,1620.,0.,1676.,1665.,2*0.,1725.,1604.,178*0,
     X             12*0.0, 605., 175*0.,
     X             3*0., 9., 9*0., 206., 27., 173*0.,
     X             0.,1750., 186*0.,
     X             8*0.,1727.,1603.,178*0.,
     X             188*0.,
     X             2*0.,1646.,14*0.,1585.,170*0.,
     X             1420.,1646.,1646.,1646.,5*0.,1505.,7*0.0,1585.,170*0,
     X             0.0,9.0,12.0,12.0,4*0.0,45.0,46.0,6*0.0,35.0,171*0.,
     X             2*0.0,423.0,351.0,184*0.,
     X             188*0.0,
     X             20*0.,34.,30.,40.,165*0.,
     X             20*0.,34.,0.,0.,184.,164*0./

      DATA XCOORD/26.54,0.,74.91,83.03,53.3,51.77,28.14,2.48,180*0.,
     X             0.,0.,14.15,10.43,184*0.,
     X             0.,0.,7.66,0.,   184*0.,
     X                              188*0.,
     X             0.,0.,-32.91,0., 184*0.,
     X             0.,0.,0.,27.929, 184*0.,
     X             0.,0.,0.,10.8,0.,4.46, 182*0.,
     X             57.107,0.,0.,41.273, 184*0.,
     X             0.,0.,5.775,-19.997,-0.52,6.193, 182*0.,
     X             0.,0.,-27.509, 7*0., 0.,-31.724, 176*0.,
     X             0.,0.,-38.800, 0., 11*0., 2.198, -221.183,171*0.,
     X             0.,0.,-9.62,0.4, 4*0., 2.42,-8.53, 178*0.,
     X             0.,-27.2,0.,0.,27.62,12.0,0.,0.,11.401,8.664,178*0.,
     X             12*0.0, 0.0 ,175*0.,
     X             3*0., 0.0, 9*0., -14.661, 45.191, 173*0.,
     X             188*0.0,
     X             8*0.,0.0,-2.6,178*0., 
     X             2*0.0,10.56,185*0.0,
     X             2*0.0,-37.2,185*0.,
     X             10.8,-37.2,-37.2,-37.2,5*0.0,36.8,178*0.,
     X             0.,-3.5,-19.4,0.,4*0.,-57.7,-44.4,6*0.0,-62.6,171*0.,
     X             2*0.0,0.0,44.25,184*0.,
     X             188*0.,
     X             188*0.,
     X             188*0./
      DATA YCOORD/52.2,0.,45.76,17.41,27.99,0.42,24.95,-2.15,180*0.,
     X             0.,0.,-11.19,-25.45,   184*0.,
     X             0.,0.,-42.4,0.,        184*0.,
     X                                    188*0.,
     X             0.,0.,20.82,0.,        184*0.,
     X             0.,0.,0.,6.321,        184*0.,
     X             0.,0.,0.,1.4,0.,12.58, 182*0.,
     X             -18.099,0.,0.,37.795, 184*0.,
     X             0.,0.,16.640,11.141,0.651,-13.682, 182*0.,
     X             0.,0.,53.373, 7*0., 0.,26.65, 176*0.,
     X             0.,0.,-14.71, 0., 11*0., 291.593, -138.718,171*0.,
     X             0.,0.,-22.83,-14.11, 4*0., -21.71,-11.72, 178*0.,
     X             0.,20.9,0.,0.,2.63,-0.02,0.,0.,-7.786,12.656,178*0.,
     X             12*0.0, 0.0, 175*0.,
     X             3*0., 0.0, 9*0., -41.255, 48.173, 173*0.,
     X             188*0.0,
     X             8*0.,0.0,20.5,178*0.,
     X             2*0.0,-13.96,185*0.0,
     X             2*0.,8.4,185*0.,
     X             63.5,8.4,8.4,8.4,5*0.,26.8,178*0.,
     X             0.0,58.4,12.6,0.0,4*0.0,13.3,22.0,6*0.0,29.8,171*0.,
     X             2*0.0,0.0,27.36,184*0.,
     X             188*0.,
     X             188*0.,
     X             188*0./
      DATA AZCORT/-.254,0.,-.725,-.925,-.508,-.499,-.270,-.024,180*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X           -0.4536,0.,0.,-0.3337, 184*0.,
     X            0.0,0.0,0.0,0.0,0.0,1.55, 182*0.,
     X            0.,0.,0.1945, 185*0.,
     X            0.0,0.0,.246, 185*0.,
     X                188*0.,
     X            0.0,0.0,0.0,0.0,-1.1,0.0, 182*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0.,
     X                188*0./
      DATA NAMES/'CHIL','CP-2','CP-3','CP-4','NOAC','NOAD','NOAK',
     X           'SKWR','FL-2','UND ','NOR ','CIM ','POLD','TOGA',
     X           'CCAA','SPAN','MIT ','MHR ','ELDA','ELDF','KMLB',
     X           'DART','DARL','KATX','KICT','KWAJ','KBHX','KTLX',
     X           'KTBW','KAMX','KJAX','KBYX','KTLH','KHGX','KCRP',
     X           'KBRO','KGRK','KEWX','TAMU','KLCH','ISRL','OMKO',
     X           'PHKT','DARG','HAWA','SAOP','GUAM','TAIW','NRHB',
     X           'ERKA','KAMA','KLSX','KAKQ','KSOX','KVTX','KLWX',
     X           'KDOX','KABR','KABX','KAMA','KAPX','KARX','KATX',
     X           'KBBX','KBGM','KBIS','KBLX','KBMX','KBOX','KBUF',
     X           'KCAE','KCBW','KCBX','KCCX','KCLE','KCLX','KCXX',
     X           'KCYS','KDAX','KDDC','KDFX','KDIX','KDLH','KDMX',
     X           'KDTX','KDVN','KDYX','KEAX','KEMX','KENX','KEOX',
     X           'KEPZ','KESX','KEVX','KEWX','KEYX','KFCX','KFDR',
     X           'KFDX','KFFC','KFSD','KFSX','KFTG','KFWS','KGGW',
     X           'KGJX','KGLD','KGRB','KGRR','KGSP','KGWX','KGYX',
     X           'KHDX','KHNX','KHPX','KICT','KILN','KILX','KIND',
     X           'KINX','KIWA','KJAN','KJGX','KJKL','KLBB','KLIX',
     X           'KLNX','KLOT','KLRX','KLSX','KLTX','KLVX','KLZK',
     X           'KMAF','KMAX','KMBX','KMHX','KMKX','KMOB','KMPX',
     X           'KMQT','KMRX','KMSX','KMTX','KMUX','KMVX','KMXX',
     X           'KNKX','KNQA','KOAX','KOHX','KOKX','KOTX','KPAH',
     X           'KPBZ','KPDT','KPOE','KPUX','KRAX','KRGX','KRIW',
     X           'KRLX','KRMX','KRTX','KSFX','KSGF','KSHV','KSJT',
     X           'KSOX','KTFX','KTLX','KTWX','KUDX','KUEX','KVAX',
     X           'KVBX','KVNX','KVTX','KYUX','PAHG','PAIH','PAPD',
     X           'PGUA','PHKI','RKSG','RODN','KDGX','KHTX'/
      DATA LSTRD,LSTXP/0,0/
      DATA KEXP/'CCOPE','JAWS','CYCLES','MAYPOLE-83',
     X          'LAKE SNOW-84','MAYPOLE-84','PHOENIX-84','NIMROD-78',
     X          'SOCORRO-84','PRESTORM-85','GALE-86','MIST-86',
     X         'CINDE-87','GERMAN','TAMEX','PROFS','TDWR','HARP',
     X         'WISP-90','WISP-91','CAPE','FEST92','TOGA COARE',
     X         'TRMM','NEXRAD','NONE'/

C     FLAG SPECIFIES IF ALTITUDE USED IS ACTUAL OR IF IT IS TO
C     BE SET TO A USER SPECIFIED VALUE
      IALTFLG=0


      READ (KRD,101)TMPUN,ITAP,TMPSKP,TMPEXP,TMPRAD,REWSTR,XRAD,YRAD,
     X     ZRAD
 101  FORMAT(/F8.0/A8/F8.0/F8.0/F8.0/A1/F8.0/F8.0/F8.0)
      IUN = TMPUN
      ISKP = TMPSKP
      IEXP = TMPEXP
      NUMRAD = TMPRAD
C
      ISKP = MAX0(ISKP,0)
      IF(IEXP.LE.1) THEN
         IF (IEXP.EQ.1) THEN
            ORLAT=462555.6
            ORLON=1055610.5
            IF(NUMRAD.LE.0.OR.NUMRAD.GT.8) THEN
               PRINT 103
 103           FORMAT(/1X,'+++  FATAL ERROR, RADAR NUMBER ',
     X              'OUT OF RANGE  +++')
               PRINT 105,'NO.RADAR',NUMRAD
               STOP
            END IF
         ELSE IF (IEXP.EQ.0) THEN
            IEXP=1
            PRINT 106
            ORLAT=462555.6
            ORLON=1055610.5
            IF(NUMRAD.LE.0.OR.NUMRAD.GT.8) THEN
               PRINT 103
               PRINT 105,'NO.RADAR',NUMRAD
               STOP
            END IF
         ELSE IF (IEXP.LT.0) THEN
            IEXP=NEXP+1
            PRINT 107
 107        FORMAT(/5X,'+++ TAKING RADAR COORD. FROM INPUT CARD +++'/)
            ORLAT=0.0
            ORLON=0.0
         END IF
      ELSE IF(IEXP.EQ.2) THEN
         ORLAT=0.0
         ORLON=0.0
         IF(NUMRAD.LT.2.OR.NUMRAD.GT.4) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.3) THEN
         ORLAT=0.0
         ORLON=0.0
         IF(NUMRAD.LT.3.OR.NUMRAD.GT.4) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.4) THEN
         ORLAT=0.0
         ORLON=0.0
         IF(NUMRAD.NE.2) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.5) THEN
         ORLAT=431551.0
         ORLON=860116.0
         IF(NUMRAD.LT.3.OR.NUMRAD.GT.4) THEN
         PRINT 105,'NO.RADAR',NUMRAD
         STOP
         ENDIF
      ELSE IF(IEXP.EQ.6) THEN
         ORLAT = 395700.3
         ORLON = 1051140.7
         IF (NUMRAD.NE.2.AND.NUMRAD.NE.4) THEN
         PRINT 105,'NO. RADAR',NUMRAD
         STOP
         END IF
      ELSE IF(IEXP.EQ.7) THEN
         ORLAT=0.0
         ORLON=0.0
         IF(NUMRAD.LE.2.OR.NUMRAD.GE.7) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.8) THEN
         ORLAT = 413638.92
         ORLON = 882407.92
         IF(NUMRAD.NE.1.AND.NUMRAD.NE.3.AND.NUMRAD.NE.4) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.9) THEN
         ORLAT = 335710.8
         ORLON = 1070629.05
         IF(NUMRAD.LT.3.OR.NUMRAD.GT.6) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.10) THEN
         IF(NUMRAD.EQ.3.OR.NUMRAD.EQ.4) THEN
            ORLAT = 374432.0
            ORLON = 974619.0
         ELSE IF(NUMRAD.EQ.11.OR.NUMRAD.EQ.12) THEN
            ORLAT = 351411.43
            ORLON = 972747.9
         ELSE
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.11) THEN
         ORLAT = 351410.0
         ORLON = -753218.0
         IF(NUMRAD.NE.3.AND.NUMRAD.NE.4.AND.NUMRAD.NE.16
     X        .AND.NUMRAD.NE.17)  THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.12) THEN
         ORLAT = 0.0
         ORLON = 0.0
         IF((NUMRAD.LT.2.OR.NUMRAD.GT.4).AND.
     X      (NUMRAD.LT.9.OR.NUMRAD.GT.10)) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.13) THEN
         ORLAT = 0.0
         ORLON = 0.0
         IF(NUMRAD.EQ.1.OR.NUMRAD.EQ.7.OR.NUMRAD.EQ.8.OR.
     X      NUMRAD.GT.10) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.14) THEN
         ORLAT = -480500.0
         ORLON = 111700.0
         IF(NUMRAD.NE.13) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.15) THEN
         ORLAT = 243844.0
         ORLON = -1204540.0
         IF(NUMRAD.NE.4.AND.NUMRAD.NE.14.AND.NUMRAD.NE.15) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.16) THEN
         ORLAT = 394139.0
         ORLON = -1044425.0
         IF(NUMRAD.NE.2) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.17) THEN
         ORLAT = 395700.0
         ORLON = -1051141.0
         IF(NUMRAD.NE.9.AND.NUMRAD.NE.10) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.18) THEN
         ORLAT = 194322.0
         ORLON = 1550249.0
         IF(NUMRAD.NE.3.AND.NUMRAD.NE.4) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF(IEXP.EQ.19) THEN
         ORLAT = 0.0
         ORLON = 0.0
         IF (NUMRAD.NE.3 .AND. NUMRAD.NE.18) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.20) THEN
         ORLAT = 0.0
         ORLON = 0.0
         IF (NUMRAD.NE.1 .AND. NUMRAD.NE.2 .AND. NUMRAD.NE.3 .AND.
     X       NUMRAD.NE.4 .AND. NUMRAD.NE.10 .AND. NUMRAD.NE.18) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.21) THEN
         ORLAT=281346.0
         ORLON=804408.0
         IF (NUMRAD.NE.2 .AND. NUMRAD.NE.3 .AND. NUMRAD.NE.4 .AND.
     X       NUMRAD.NE.9 .AND. NUMRAD.NE.10 .AND. NUMRAD.NE.17) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.22) THEN
         ORLAT=392751.32
         ORLON=960240.38
         IF (NUMRAD.NE.3 .AND. NUMRAD.NE.4) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.23) THEN
         IF (NUMRAD.NE.19 .AND. NUMRAD.NE.20) THEN
            PRINT 105, 'NO RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.24) THEN
         ORLAT=0.0
         ORLON=0.0
         IF (.NOT.(NUMRAD.EQ.14
     X         .OR. NUMRAD.EQ.21 .OR. NUMRAD.EQ.22 .OR. NUMRAD.EQ.23 
     X         .OR. NUMRAD.EQ.26
     X         .OR. (NUMRAD.GE.29 .AND. NUMRAD.LE.NRMX))) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF (IEXP.EQ.25) THEN
         ORLAT=0.0
         ORLON=0.0
         IF (NUMRAD.NE.21 .AND. NUMRAD.NE.24 .AND. NUMRAD.NE.25
     X         .AND. NUMRAD.NE.27 .AND. NUMRAD.NE.28 .AND. NUMRAD.NE.29
     X         .AND. NUMRAD.NE.30 .AND. NUMRAD.NE.31 .AND. NUMRAD.NE.32
     X         .AND. NUMRAD.NE.33 .AND. NUMRAD.NE.34 .AND. NUMRAD.NE.35
     X         .AND. NUMRAD.NE.36 .AND. NUMRAD.NE.37 .AND. NUMRAD.NE.38
     X         .AND. NUMRAD.NE.40) THEN
            PRINT 105, 'NO. RADAR', NUMRAD
            STOP
         END IF
      ELSE IF((IEXP.GT.NEXP)) THEN
         IEXP=1
         PRINT 106
 106     FORMAT(/5X,'+++  CCOPE EXPERIMENT WILL BE ASSUMED  +++'
     X        //5X,'CONTACT BILL ANDERSON IF YOUR EXPERIMENT IS NOT',
     X        ' IN THE DOCUMENTATION.'/)
         ORLAT=462555.6
         ORLON=1055610.5
         IF(NUMRAD.LE.0.OR.NUMRAD.GT.8) THEN
            PRINT 103
            PRINT 105,'NO.RADAR',NUMRAD
            STOP
         END IF
      END IF
      IF(NUMRAD.NE.LSTRD.OR.IEXP.NE.LSTXP) INPTST=1
      LSTRD1=NUMRAD
      LSTXP=IEXP
      IF (IUN.GT.0) GOTO 30
         PRINT 104
104      FORMAT(/1X,'+++  FATAL ERROR IN THE UNIT NUMBER  +++')
         PRINT 105,'UNIT NO.',IUN
         STOP
 30   CONTINUE
      IF(REWSTR(1:1).EQ.'Y') THEN
         REWSTR='YES'
         IREW=1
         CALL INIT_COS(IUN,IREW)
      ELSE
         REWSTR='NO'
         IREW=0
         CALL INIT_COS(IUN,IREW)
      END IF
C      CALL SKPVOL(IUN,ISKP)
C
C     IF ORIGINAL EXP # WAS < 0 THEN USE USER SUPPLIED COORD. OF RADAR
C
      IF (IEXP.EQ.(NEXP+1)) THEN
         ID(46)=ZRAD*1000
         ID(47)=XRAD*100
         ID(48)=YRAD*100
         ID(49)=0.0
         NMRAD=NAMES(NUMRAD)
         IALTFLG=1
      ELSE IF (IEXP.GE.1 .AND. IEXP.LE.NEXP) THEN
         ID(46)=HTRAD(NUMRAD,IEXP)
         ID(47)=XCOORD(NUMRAD,IEXP)*100
         ID(48)=YCOORD(NUMRAD,IEXP)*100
         IF (IAZFLG.EQ.0) THEN
C
C     SET AZIMUTH CORRECTION FROM TABLE
C
            ID(49)=AZCORT(NUMRAD,IEXP)*1000
            AZCOR=AZCORT(NUMRAD,IEXP)
         END IF
         NMRAD=NAMES(NUMRAD)
      ELSE
         WRITE(*,*)'*** INVALID STATE REACHED IN INPFIL ***'
         STOP
      END IF
      PRINT 885
885   FORMAT(//5X,'SUMMARY OF INPUT COMMAND ')
      PRINT 887
887   FORMAT(5X,'------- -- ----- ------- ')
      PRINT 888,IUN,ITAP,REWSTR,ISKP,KEXP(IEXP),NMRAD
888   FORMAT(/20X,'         UNIT NUMBER: ',I5
     X       /20X,'           TAPE NAME: ',A6
     X       /20X,'         REWIND TAPE: ',A3
     X       /20X,'NUMBER FILES TO SKIP: ',I5
     X       /20X,'     EXPERIMENT NAME: ',A12
     X       /20X,'          RADAR NAME: ',A4//)
      IF (NMRAD.EQ.'CHIL') PRINT 889
 889  FORMAT (//5X,'+++ WARNING - CHILL VELOCITIES WILL BE RESCALED',
     X   ' AND THE SIGNS REVERSED +++'//)
      RETURN
 35   CONTINUE
         PRINT 105,'SKP FILS',ISKP
 105     FORMAT (5X,'***ERROR IN INPUT COMMAND  ',A10,'=',I10)
         STOP
      END
