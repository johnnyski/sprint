#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>
#include "cedric.h"

#define BYTSWRD 4

extern FILE *fp;  /* file pointer */

#if defined (IBMRISC) || defined (HP)
void rdsunrec(rp, ilen)
#elif defined (CRAY)
void RDSUNREC( rp, ilen )
#else
void rdsunrec_( rp, ilen )
#endif
     char *rp;			/* record pointer */
     int *ilen;
{

  int i, length;
  char reclength[5];

  /* read in first four bytes */

  i = fread(&length, BYTSWRD, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }

  length = ntohl(length);
  length = length >> (WORD_SIZE -32);
  if (length <= 0) {
    *ilen = 0;
    return;
  }
  
  /* read in record */

  i = fread(rp, length, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }

  /* read tail of record */

  i = fread(reclength, BYTSWRD, 1, fp);
  if( i <= 0 ) {
    *ilen = 0;
    return;
  }


  *ilen = length;

  return;

}
