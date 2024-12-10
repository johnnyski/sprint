      FUNCTION FEETCL(NFLDS,NLEV,NPLANE,NBLK)
C
C        CALCULATE FEET USED ON MAGNETIC TAPE
C
      PARAMETER (FPBY=1./6250.,CITF=1./12.)
      DATA EOF,EOR,ILHED,IVHED/6.0,0.6,10,510/
 
C
C        CALCULATE DISTANCE EACH LEVEL
C
      NGAPS=NFLDS*((NPLANE-1)/NBLK+1) + 1
      NBYTES=2*(NPLANE*NFLDS+ILHED)
C
C
C
      NBYTOT=IVHED*2 + NLEV*NBYTES
      FEETCL=(NLEV*NGAPS*EOR + FPBY*NBYTOT +EOF) * CITF
      RETURN
      END
