      SUBROUTINE BLHED(CVAL,K,NFLDS,ITER,LHED,LHD8,LHD9)
 
      DIMENSION LHED(10)
 
      CHARACTER*2 CTEMP
 
      WRITE(CTEMP,10)'LE'
      READ(CTEMP,10)LHED(1)
 10   FORMAT(A2)
      WRITE(CTEMP,10)'VE'
      READ(CTEMP,10)LHED(2)
      WRITE(CTEMP,10)'L '
      READ(CTEMP,10)LHED(3)
      LHED(4)=CVAL*100
      LHED(5)=K
      LHED(6)=NFLDS
      LHED(7)=ITER
      LHED(8)=LHD8
      LHED(9)=LHD9
      LHED(10)=0
      RETURN
      END
