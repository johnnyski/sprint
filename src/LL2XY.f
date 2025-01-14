      SUBROUTINE  LL2XY (DEGLAT, DEGLON, X, Y, SWLAT, SWLON)
C
C  TO CONVERT LAT.,LONG. TO X,Y IN KM WITH RESPECT TO SWLAT,SWLON
C  PASCAL BY P. JOHNSON, 17-FEB-81.  FORTRAN TRANS R. VAUGHAN 9/81.
C  FINAL REPAIR, M. BRADFORD, 4/88
C  WARNING!  WORKS ONLY IN NORTHERN/WESTERN HEMISPHERES!
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      PARAMETER (PI=3.141592654)
      PARAMETER (R=6380.12)
      PARAMETER (DEGRAD=0.01745329)
C
      REAL DEGLAT,DEGLON,X,Y,SWLON,SWLAT

      X=0.0
      Y=0.0
      ALAT = SWLAT * DEGRAD
      CALAT = COS(ALAT)
      SALAT = SIN(ALAT)
      ALONG = ABS(SWLON * DEGRAD)
      BLAT = DEGLAT*DEGRAD
      BLONG = ABS(DEGLON)*DEGRAD
      CBLAT = COS(BLAT)
      SBLAT = SIN(BLAT)
      DLON = ALONG-BLONG
      DLAT=ABS(DEGLAT-SWLAT)*DEGRAD
      IF(DLAT.LT.1.0E-7.AND.ABS(DLON).LT.1.0E-7) GO TO 90
      CDLON = COS(DLON)
      AZA = ATAN(SIN( DLON)/(CALAT*SBLAT/CBLAT-SALAT*CDLON))
      AZB = ATAN(SIN(-DLON)/(CBLAT*SALAT/CALAT-SBLAT*CDLON))
C
C  GET BEARING
C
      IF(BLAT .LT. ALAT) AZA = AZA+PI
      IF(ALAT .LT. BLAT) AZB = AZB+PI
      IF(AZA .LT. 0) AZA = AZA + 2.*PI
      IF(AZB .LT. 0) AZB = AZB + 2.*PI
C
      SINN =  DLAT
      IF(DLON.NE.0.0)
     .       SINN = SIN(DLON) * SIN(PI/2.0-BLAT) / SIN(AZA)
C
      COSN = SALAT*SBLAT + CALAT*CBLAT*CDLON
      S = R * ATAN(SINN/COSN)
      X = S * SIN(AZA)
      Y = S * COS(AZA)
C
   90 CONTINUE
      RETURN
C
      END









