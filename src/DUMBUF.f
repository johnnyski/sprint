      SUBROUTINE DUMBUF(MBUF,LNBF,ISTR,IGON)
 
 
      DIMENSION MBUF(LNBF)
      DATA JREP/1/
 
      IF (JREP.GT.2 .AND. IGON.NE.1) GOTO 901
      NPAG=0
      PRINT 998
 998  FORMAT(///,19X,'CHAR',5X,'DECIMAL',5X,'OCTAL',11X,'HEX')
      DO 900 IRP=1,LNBF
         NPAG=NPAG+1
         IF (NPAG.GT.60) THEN
            PRINT 998
            NPAG=0
            ENDIF
         KTEM=MBUF(IRP)
         PRINT 999,ISTR,IRP,KTEM,KTEM,KTEM,KTEM
 999     FORMAT (3X,A2,2X,I4,5X,A4,5X,I7,5X,O7,5X,Z20)
 900  CONTINUE
      JREP=JREP+1
 901  CONTINUE
      RETURN
      END
