#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>
#include "cedric.h"

struct files *open_files;  /* linked list of open file information */
struct files *head;        /* head of linked list */
FILE *fp;
extern int cos_size;


FILE *uncompress_pipe (FILE *fp);



#if defined (IBMRISC) || defined (HP)
void init_cos(IUN, IREW)
#elif defined (CRAY)
     void INIT_COS(IUN, IREW)
#elif defined (linux)
     void init_cos__(IUN, IREW) /* Why the double __? This is the only
								 * file that f2c produced this bizzaar
								 * symbol. 
								 */
#else
     void init_cos_(IUN, IREW) 
#endif
     int *IUN;       /* unit number of file to open */
     int *IREW;      /* rewind flag */
{
  
  char filename[8];
  int num1, num2, rval;
  void init_cbs();
  
  num1 = *IUN / 10;
  num2 = *IUN % 10;
  
  /* construct filename */
  filename[0] = 'f';
  filename[1] = 'o';
  filename[2] = 'r';
  filename[3] = 't';
  filename[4] = '.';
  filename[5] = num1 + 48;  /* convert to ascii */
  filename[6] = num2 + 48;  /* convert to ascii */
  filename[7] = '\0';
  
  printf("FILENAME opened INIT_COS: is %s\n", filename);
  if (head == NULL) {  /* start linked list of open files */
    open_files = (struct files *)malloc(sizeof(struct files));
    if (open_files == NULL) { /* error getting address */
      printf("\n+++OUT OF MEMORY IN CINITCOS+++\n");
      exit(-1);
    }
    head = open_files;
    open_files->next = NULL;
    open_files->unit = -99;
  } 
  open_files = head;
  while (open_files->next != NULL  &&  open_files->unit != *IUN) {
    open_files = open_files->next;
  }
  
  if (open_files->unit != *IUN) { /* file is not already in list;open it*/
    if (*IREW == 1) *IREW = 0;
    fp = fopen(filename,"r");
    if (fp == NULL) {
      printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
      exit(-1);
    }
	fp = uncompress_pipe(fp);  /* PIPE IS NOT SEEKABLE. */
    cos_size = 0;  /* reset buffer position used in rdcosrec */
    init_cbs();
    if (open_files->unit > 0) {
      /* add new open file to list of open files */
      open_files->next = (struct files *) malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("\n+++OUT OF MEMORY IN CINITCOS+++\n");
	exit(-1);
      }
      open_files = open_files->next;
    }
    open_files->unit = *IUN;
    open_files->fps  = fp;
    open_files->next = NULL;
  }
  else {
    fp = open_files->fps;
    if (*IREW == 1) {
      rval = fseek(fp, 0, 0);  /* go to first volume */
      if (rval != 0) {
	printf("\n+++ERROR SEEKING ON %s +++\n",filename);
	exit(-1);
      }
      cos_size = 0;  /* reset buffer position used in rdcosrec */
      init_cbs();
    }
  }
  
  return;
}

/* perform byte swapping for 32 bit words */
/* Rewritten for portability -- JHM 2/13/96. */
void
#if defined (IBMRISC) || defined (HP)
swap32
#elif defined (CRAY)
SWAP32
#else
swap32_
#endif
(unsigned int a[], int *n)

{
  int i;
  for (i=0; i<*n; i++) /* For each 32 bit word. */
	a[i] = ntohl(a[i]);
  return;
}

/* perform byte swapping for 64 bit words*/
/* Rewritten for portability -- JHM 2/13/96. */
void
#if defined (IBMRISC) || defined (HP)
swap64
#elif defined (CRAY)
SWAP64
#else
swap64_
#endif
(unsigned char c[], int *n)
{
  unsigned char temp;
  int i;
#if defined (HP) || defined (SUN) || defined (SGI)
  return;
#else

  for (i=0; i<(*n)*8; i+=8) { /* Little Endian swap. */
	temp = c[i+0];
	c[i+0] = c[i+7];
	c[i+7] = temp;
	temp = c[i+1];
	c[i+1] = c[i+6];
	c[i+6] = temp;
	temp = c[i+1];
	c[i+2] = c[i+5];
	c[i+5] = temp;
	temp = c[i+1];
	c[i+3] = c[i+4];
	c[i+4] = temp;
  }
  return;
#endif
}

/* swap characters for little Endian machines*/
/* Rewritten: What is this?  Is this swap 16 bit words?  There
 * is no such thing as swap characters. --JHM 2/13/96
 *
 * However, this function is oddly named.  The original function
 * skipped two bytes, swapped two bytes, skipped two, swapped two, etc.
 * This all comes from how the character strings are loaded into the
 * original input (4 byte word size) integer array.
 */ 
void
#if defined (IBMRISC) || defined (HP)
swapchar
#elif defined (CRAY)
SWAPCHAR(INJ, OUJ, NUM)
#else
swapchar_
#endif
/*(unsigned short c[], unsigned short co[], int *n)*/
(unsigned int c[], unsigned int co[], int *n)
{
  union {
	unsigned int ival;
	unsigned short half[2];
  } word;

  int i;
  unsigned char ctmp;
  for (i=0; i<*n; i++) {
	word.ival = c[i];
	word.half[0] = ntohs(word.half[0]);
	word.half[1] = ntohs(word.half[1]);
	co[i] = word.ival;
  }
  
  return;
}

