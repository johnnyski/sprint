      SUBROUTINE SWAPEM (ITEM1, ITEM2)
C
C   BY: MARTA ORYSHCHYN
C
C     SWAPS THE LOCATIONS OF ANY TWO ITEMS
C   PASSED INTO THE ROUTINE.
C
      ITEMPH = ITEM1
      ITEM1 = ITEM2
      ITEM2 = ITEMPH
C
      RETURN
      END
