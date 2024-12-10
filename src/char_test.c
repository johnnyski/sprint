#include <stdio.h>

void CFUN (i1,fcd, i2,fcd2)
     char fcd[3][8];
     int *i1, *i2;
     char fcd2[3][8];
{
  char cpp[3][9];
  char cpp1[3][9];
  int i, j;

  strncpy(&(cpp[0][0]), &(fcd[0][0]), 8);
  strncpy(&(cpp[1][0]), &(fcd[1][0]), 8);
  strncpy(&(cpp[2][0]), &(fcd[2][0]), 8);
  strncpy(&(cpp1[0][0]), &(fcd2[0][0]), 8);
  strncpy(&(cpp1[1][0]), &(fcd2[1][0]), 8);
  strncpy(&(cpp1[2][0]), &(fcd2[2][0]), 8);
  cpp[0][8]='\0';
  cpp[1][8]='\0';
  cpp[2][8]='\0';
  cpp1[0][8]='\0';
  cpp1[1][8]='\0';
  cpp1[2][8]='\0';
  

  printf("on entering, i1 = %d, i2 = %d, string0 = %s, string1 = %s, string2 = %s, string3 = %s, string4 = %s, string5 = %s\n", *i1, *i2, &(cpp[0][0]), &(cpp[1][0]), &(cpp[2][0]), &(cpp1[0][0]), &(cpp1[1][0]), &(cpp1[2][0]));

  strcpy(&(fcd[0][0]),"cp-3    ");
  strcpy(&(fcd[1][0]),"cp-4    ");
  strcpy(&(fcd[2][0]),"fore    ");
  strcpy(&(fcd2[0][0]),"aft     ");
  strcpy(&(fcd2[1][0]),"cp-4    ");
  strcpy(&(fcd2[2][0]),"raus    ");

  *i1 = 777;
  *i2 = 888;
  
/*  printf("string0 = %s, string1 = %s\n",&(cpp[0][0]), &(cpp[1][0])); */

  return;
}
  
