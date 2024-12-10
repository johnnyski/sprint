      SUBROUTINE IGETCP2(IWRD,IAZ,IX,IY)
C
C     UNPACKS INITIAL POSITIONING INFORMATION FROM A CARTESIAN LOCATION
C     THIS INFORMATION IS STORED IN THE UPPER 32-BITS OF IWRD.
C     THIS ROUTINE IS CURRENTLY ONLY USED WITH THE ARRAY ICART WHICH
C     ONLY REQUIRES 32 BITS.
C     
C
C      DATA MASK08,MASK16/ O'377', O'177777'/
      CALL GBYTES(IWRD,IAZ,0,16,0,1)
      CALL GBYTES(IWRD,IY,16,8,0,1)
      CALL GBYTES(IWRD,IX,24,8,0,1)

C      IX =ICEDAND(IWRD,MASK08)
C      IY =ICEDAND(ICEDSHFT(IWRD,-8),MASK08)
C      IAZ=ICEDAND(ICEDSHFT(IWRD,-16),MASK16)
      RETURN
      END
