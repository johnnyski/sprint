      SUBROUTINE SHIRBL(NAME,NNAM)
C
C        REVERSE SHILBL
C
      INCLUDE 'SPRINT.INC'
      DIMENSION NAME(NNAM)
 
      ISHFAMT=-(WORDSZ-16)
      DO 100 I=1,NNAM
         NAME(I)=ICEDSHFT(NAME(I),ISHFAMT)
 100  CONTINUE
      RETURN
      END
