      SUBROUTINE LOADBMS(IOP,ISWP,DATA)
C
C     THIS SUBROUTINE LOADS BEAMS INTO THE ARRAY DATA.
C     IOP = 0  ==>  READ IN MIN(NRAYS, MAXBM) RAYS AND UNPACK INTO DATA
C     IOP = 1  ==>  SHIFT BEAMS IN DATA ARRAY AND PUT A BEAM ON THE END
C     ISWP   - SWEEP NUMBER
C     DATA   - ARRAY THAT HOLDS DATA FOR BEAMS
C
      PARAMETER(MAXRNG=768,MAXBM=51,MAXBM2=1000,MAXFLD=8,NID=296)
      PARAMETER(MAXVAL=(MAXRNG+10)*MAXFLD)
      PARAMETER(BDVAL=-32768.)

      COMMON /FORMAT/ IRP,IBLOCK
      COMMON /IDBLK/  ID(NID)
      COMMON /SCRDSK/ LTMP
      COMMON /IO/KPCK(85000),KOUT(8500),IBUF(8500),NBUF(2125,4),
     X     IZ8(17),ILSTREC
      COMMON /FILT/ ISTFLG,NRAYS,IDIR,C,NLEN,NUMFILT,INPFLD(MAXFLD),
     X              SCLFLD(MAXFLD),MTMP

      COMMON /CFLD/NFLDS
      COMMON /CFLDC/ IFIELD(MAXFLD), INTINF(MAXFLD,3)
      CHARACTER*8 INTINF,IFIELD

      COMMON /CRNG/ELTUS,MNBEM,RUSR1,RUSR2,RG1,RG2,DRG,NRG,VNYQ
     X     ,RNOTUS,DRGUS,CFAC1,CFAC2,CFAC3
      COMMON /CRNGC/ IRCFXL
      CHARACTER*8 IRCFXL

      CHARACTER*8 NAMTIM
      COMMON /TRANS/ X1,X2,XD,Y1,Y2,YD,Z1,Z2,ZD,NX,NY,NZ,XORG,YORG,
     X   ANGXAX,ZRAD,AZLOW,BAD,ASNF,ACSF,IAXORD(3),NPLANE,EDIAM

      DIMENSION DATA(MAXVAL,MAXBM)
      DATA NAMTIM/'TIME'/

      NFLINP=NFLDS
      IF (IFIELD(NFLDS).EQ.NAMTIM) NFLINP=NFLINP-1
      IF (IOP.EQ.0) THEN
         IDIR=ID(130+3*(ISWP-1))
         C=FLOAT(ID(129+3*(ISWP-1)))/FLOAT(ID(44))
         
         CALL RDRYDK(KPCK,KOUT,NST,LTMP,1,NLEN)
         DO J=1,MAXBM
            DO I=1,MAXVAL
               DATA(MAXVAL,MAXBM)=BDVAL
            END DO
         END DO
         
         DO 137 K=1,ID(37)
            DATA(K,1)=FLOAT(KOUT(K))
 137     CONTINUE
         DO I=1,NFLINP
            ISTRT=(I-1)*KOUT(8) + ID(37)
            JSTRT=(I-1)*MAXRNG  + ID(37)
            DO K=1,MAXRNG
               IF (K.LE.KOUT(8)) THEN 
                  DATA(JSTRT+K,1)=FLOAT(KOUT(ISTRT+K))
               ELSE
                  DATA(JSTRT+K,1)=BDVAL
               END IF
            END DO
         END DO
                  
         DO 125 J=2, MIN(NRAYS,MAXBM)
            CALL RDRYDK(KPCK,KOUT,NST,LTMP,0,NLEN)
            DO K=1,ID(37)
               DATA(K,J)=FLOAT(KOUT(K))
            END DO
            DO I=1,NFLINP
               ISTRT=(I-1)*KOUT(8) + ID(37)
               JSTRT=(I-1)*MAXRNG  + ID(37)
               DO K=1,MAXRNG
                  IF (K.LE.KOUT(8)) THEN 
                     DATA(JSTRT+K,J)=FLOAT(KOUT(ISTRT+K))
                  ELSE
                     DATA(JSTRT+K,J)=BDVAL
                  END IF
               END DO
            END DO
 125     CONTINUE
C     
C     UNSCALE ALL FIELDS THAT WILL BE FILTERED
C     
         DO 400 J=1,NUMFILT
            ISTART=INPFLD(J)
            DO 425 L=1, MIN(NRAYS,MAXBM)
               NRG=DATA(8,L)
               DO 450 K=1,NRG
                  IF (DATA(ISTART+K-1,L).NE.BDVAL)
     X                 DATA(ISTART+K-1,L)=DATA(ISTART+K-1,L)/
     X                 SCLFLD(J)
 450           CONTINUE
 425        CONTINUE
 400     CONTINUE
            
C     
C     UNSCALE AZIMUTHS
C     
         DO 475 L=1,MIN(NRAYS,MAXBM)
            DATA(1,L)=DATA(1,L)/64.
 475     CONTINUE
            

      ELSE IF (IOP.EQ.1) THEN

C
C     SHIFT THE DATA FIRST
C
         DO I=2,MAXBM
            DO J=1,MAXVAL
               DATA(J,I-1) = DATA(J,I)
            END DO
         END DO
         
C
C     READ IN A BEAM AND UNPACK AND UNSCALE IT
C
         CALL RDRYDK(KPCK,KOUT,NST,LTMP,0,NLEN)
         DO K=1,ID(37)
            DATA(K,MAXBM)=FLOAT(KOUT(K))
         END DO
         DO I=1,NFLINP
            ISTRT=(I-1)*KOUT(8) + ID(37)
            JSTRT=(I-1)*MAXRNG  + ID(37)
            DO K=1,MAXRNG
               IF (K.LE.KOUT(8)) THEN
                  DATA(JSTRT+K,MAXBM)=FLOAT(KOUT(ISTRT+K))
               ELSE
                  DATA(JSTRT+K,MAXBM)=BDVAL
               END IF
            END DO
         END DO

         NRG=KOUT(8)
         DO J=1,NUMFILT
            ISTART=INPFLD(J)
            DO K=1,NRG
               IF (DATA(ISTART+K-1,MAXBM).NE.BDVAL)
     X              DATA(ISTART+K-1,MAXBM)=DATA(ISTART+K-1,MAXBM)/
     X              SCLFLD(J)
            END DO
         END DO
         DATA(1,MAXBM)=DATA(1,MAXBM)/64.

      END IF

      RETURN

      END
