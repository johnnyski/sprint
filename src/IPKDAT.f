      SUBROUTINE IPKDAT(IWRD,IDAT,N)
C
C        PACKS 16-BIT INTEGERS INTO A 64-BIT WORD
C
      DIMENSION IDAT(1)
      CALL SBYTES(IWRD,IDAT,0,16,0,N)
      RETURN
      END
