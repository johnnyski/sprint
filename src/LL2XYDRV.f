      SUBROUTINE LL2XYDRV(PLAT, PLON, X, Y, ORLAT, ORLON, ANGXAX)
C
C     DRIVER ROUTINE FOR CONVERTING THE SEPARATION OF TWO POINTS
C     SPECIFIED IN LAT, LON TO SEPARATION IN X, Y IN KM. 
C     THIS ROUTINE (AND LL2XY) WILL WORK FOR POINTS IN ANY PART OF
C     THE GLOBE WITH SOME RESTRICTIONS (SEE BELOW). THE ANGLE CONVENTIONS ARE:
C     0   < LAT < 90   ==>  NORTHERN HEMISPHERE
C     -90 < LAT < 0    ==>  SOUTHERN HEMISPHERE
C     
C     0    < LON < 180 ==>  WESTERN HEMISPHERE
C     -180 < LON < 0   ==>  EASTERN HEMISPHERE
C     
C     PLAT  - LAT. OF POINT FOR WHICH X,Y IS DESIRED
C     PLON  - LON. OF POINT FOR WHICH X,Y IS DESIRED
C     X     - OUTPUT X VALUE RELATIVE TO ORLAT, ORLON IN KM
C     Y     - OUTPUT Y VALUE RELATIVE TO ORLAT, ORLON IN KM
C     ORLAT - LAT. OF ORIGIN
C     ORLON - LON. OR ORIGIN
C     ANGXAX- ANGLE OF X-AXIS REL. TO TRUE NORTH (USUALLY 90.0)
C
C     KNOWN RESTRICTIONS AND LIMITATIONS:
C
C     1) ||PLAT| - |ORLAT|| <= 90.0
C     2) ||PLON| - |ORLON|| <= 90.0
C     3) NO INPUT LAT OR LON VALUE SHOULD BE EQUAL TO EXACTLY ZERO
C     4) THE CODE IS NOT SETUP TO HANDLE CROSSING OVER BOTH HEMISPHERE 
C     BOUNDARIES AT ONCE; IT CAN HANDLE CROSSING EITHER THE NORTH/SOUTH 
C     HEMISPHERE BOUNDARY OR THE WEST/EAST BOUNDARY, BUT NOT BOTH AT ONCE. 
C     FOR EXAMPLE, YOU CAN'T HAVE AN ORIGIN AT (1.0 deg, 1.0 deg) AND TRY 
C     TO FIND THE X,Y OF A POINT AT (-1.0 deg, -1.0 deg). YOU COULD FIND 
C     THE X,Y OF A POINT AT (-1.0 deg, 1.0 deg), HOWEVER.
C     5) CODE WON'T WORK IF YOU TRY TO CROSS A POLE
C
      
      PARAMETER (EPS=0.0001, DEGRAD=0.01745329)
      
      ICROSS=0
C
C     DETERMINE IF A HEMISPHERE BOUNDARY HAS BEEN CROSSED
C
      IF (SIGN(1.0,ORLAT).GT.SIGN(1.0,PLAT)) ICROSS=1
      IF (SIGN(1.0,ORLAT).LT.SIGN(1.0,PLAT)) ICROSS=2
      IF (SIGN(1.0,ORLON).GT.SIGN(1.0,PLON)) THEN
         IF (ICROSS.NE.0) THEN
            WRITE(*,10)
 10         FORMAT(/,5X,'+++ CANNOT HANDLE DUAL HEMISPHERE CROSSOVER ',
     X           '+++')
            STOP
         ELSE
            ICROSS=3
         END IF
      ELSE IF (SIGN(1.0,ORLON).LT.SIGN(1.0,PLON)) THEN
         IF (ICROSS.NE.0) THEN
            WRITE(*,10)
            STOP
         ELSE
            ICROSS=4
         END IF
      END IF
         
      IF (ORLAT.GT.0.0) THEN
         INHEM=1
      ELSE
         INHEM=0
      END IF
      IF (ORLON.GT.0.0) THEN
         IWHEM=1
      ELSE
         IWHEM=0
      END IF
      IF (ICROSS.EQ.0) THEN
C
C     NO HEMISPHERE CROSSOVER; JUST CALL LL2XY
C
      
C
C     MAKE SIGNED VALUES POSITIVE 
C
         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)         
         
         CALL LL2XY(DEGLAT,DEGLON,X,Y,SWLAT,SWLON)

C
C     SWITCH SIGNS IF NOT IN NORTHERN OR WESTERN HEMISPHERES
C
         IF (INHEM.EQ.0) Y=-Y
         IF (IWHEM.EQ.0) X=-X

      ELSE IF (ICROSS.EQ.1) THEN
C
C     +LAT -> -LAT
C

         DEGLAT=EPS
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)
         
         IF (IWHEM.EQ.0) X1=-X1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =EPS
         SWLON =ABS(PLON)
         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=-(ABS(Y1)+ABS(Y2))
         X=X1
         
      ELSE IF (ICROSS.EQ.2) THEN
C
C     -LAT -> +LAT
C
         DEGLAT=EPS
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)
         
         IF (IWHEM.EQ.0) X1=-X1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =EPS
         SWLON =ABS(PLON)
         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=(ABS(Y1)+ABS(Y2))
         X=X1
         
      ELSE IF (ICROSS.EQ.3) THEN
C
C     +LON -> -LON
C
         DEGLAT=ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            DEGLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            DEGLON=180.0-EPS
         END IF
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)

         IF (INHEM.EQ.0) Y1=-Y1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            SWLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            SWLON=180.0-EPS
         END IF

         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=Y1
         X=(ABS(X1)+ABS(X2))

      ELSE IF (ICROSS.EQ.4) THEN
C
C     -LON -> +LON
C
         DEGLAT=ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            DEGLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            DEGLON=180.0-EPS
         END IF
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)

         IF (INHEM.EQ.0) Y1=-Y1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            SWLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            SWLON=180.0-EPS
         END IF

         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=Y1
         X=-(ABS(X1)+ABS(X2))
         
      END IF

C
C     ROTATE, IF NECESSARY
C
      IF (ANGXAX.NE.90.0) THEN
         THETA=(ANGXAX-90.0)*DEGRAD
         XT=X
         YT=Y
         X=XT*COS(THETA) - YT*SIN(THETA)
         Y=XT*SIN(THETA) + YT*COS(THETA)
      END IF
      

      RETURN

      END
