/* Cray YMP version */

#include <stdio.h>
#include "cedric.h" 

struct files *open_files, *head;
FILE *fp1;   /* pointer to output file */
FILE *uncompress_pipe (FILE *fp);
int swapchar_(unsigned int c[], unsigned int co[], int *n);


/* the following function initializes the output file for writing;
 * "initialization" includes opening the file, if necessary, and
 * positioning it as requested
 */
#if defined (IBMRISC) || defined (HP)
void cout(L1,L2,IPOS,ISKP,IBUF)
#elif defined (CRAY)
void COUT(L1,L2,IPOS,ISKP,IBUF)
#else
void cout_(L1,L2,IPOS,ISKP,IBUF)
#endif
     int *L1, *L2;
     int *IPOS, *ISKP;
     int IBUF[1];
{
  char filename[8];
  char *ced = CED;
  char inp[5];
  int byte_order = SPRINT_BYTE_ORDER;
  int start = FIRST_VOL;
  int zero = 0 , swap, ival;
  int byte, i, jval, count, ipos, jpos, inunit;
  
  inp[4] = '\0';
  byte_order = byte_order << (WORD_SIZE - 32);
  start = start << (WORD_SIZE - 32); /* shift the integer */
  inunit = *L1*10 + *L2;
  
  filename[0] = 'f';
  filename[1] = 'o';
  filename[2] = 'r';
  filename[3] = 't';
  filename[4] = '.';
  filename[5] = *L1 + 48;    /* convert to ascii */
  filename[6] = *L2 + 48;    /* convert to ascii */
  filename[7] = '\0';
  
  
  
  if (head == NULL) { /* create a list */
    head = (struct files*)malloc(sizeof(struct files));
    if (head == NULL) {
      printf("\n+++OUT OF MEMORY IN COUT+++\n");
      exit(-1);
    }
    head->next = NULL;
  }
  if (*IPOS == 1) {  /* create for writing */
    open_files = head;
    while (open_files->next != NULL  &&  open_files->unit != inunit) {
      open_files = open_files->next;
    }
    if (open_files->unit == inunit) {
#if !defined(__sgi)
      fclose(open_files->fps);
#endif
    }
    else { /* add a new entry to the linked list */
      open_files->next = (struct files*)malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("\n+++OUT OF MEMORY IN COUT+++\n");
	exit(-1);
      }
      open_files = open_files->next;
      open_files->unit = inunit;
      open_files->next = NULL;
    }      
    
    fp1 = fopen(filename,"w+");
    if (fp1 == NULL)  {
      printf("\n+++ERROR OPENING %s FOR WRITING+++\n",filename);
      exit(-1);
    }
    
    ival = fwrite(ced, 1, 4, fp1);
    if (ival <= 0) {
      printf("\n+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }
    
    ival = fwrite(&byte_order, 1, 4, fp1);  /* write out byte_order flag */
    if (ival <= 0) {
      printf("\n+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }
    

    fseek(fp1,8,1); /* skip past length information */

    ival = fwrite(&start, 1, 4, fp1);  /* write out starting byte location
					  of first cedric volume */
    if (ival <= 0) {
      printf("\n+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }
    
    for (i = 0; i < 24; i++) {  /* set other fields to zero */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO %s +++\n",filename);
	exit(-1);
      }
      
    }

    ival = fwrite(IBUF, 1, 56, fp1);  /* write out descriptive string */
    if (ival <= 0) {
      printf("\n+++ERROR WRITING TO %s +++\n",filename);
      exit(-1);
    }

    for (i = 0 ; i < (24*14); i++) { /* zero out other string positions */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO %s +++\n",filename);
	exit(-1);
      }
    }      

    for (i = 0; i < 6; i++) {  /* zero out reserved fields */
      ival = fwrite(&zero, 1, 4, fp1);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO %s +++\n",filename);
	exit(-1);
      }
    }

    /* update linked list with new info */
    open_files->fps = fp1;
    open_files->curr_vol = 1;
    open_files->swap = FALSE;
    
  }
  else if (*IPOS == 2) {  /* add to end of existing file */
    open_files = head;
    while (open_files->next != NULL  &&  open_files->unit != inunit) {
      open_files = open_files->next;
    }
    if (open_files->unit != inunit) {
      fp1 = fopen(filename,"r+");
      if (fp1 == NULL)  {
	printf("\n+++ERROR OPENING %s FOR APPENDING+++\n",filename);
	exit(-1);
      }
/*
 * Pass the input file through gunzip, transparently.  But, only after
 * I elimate the 'fseek' calls.  -JHM 2/15/96
 */
/*	  fp1 = uncompress_pipe(fp1); */
      open_files->next = (struct files *)malloc(sizeof(struct files));
      if (open_files->next == NULL) {
	printf("\n+++OUT OF MEMORY IN COUT+++\n");
	exit(-1);
      }
      open_files = open_files->next;
      open_files->unit = inunit;
      open_files->fps = fp1;
      open_files->next = NULL;
    }
    else {
      fp1 = open_files->fps;
      ival = fseek(fp1,0,0);
      if (ival != 0) {
	printf("\n+++ERROR SEEKING ON UNIT %d +++\n",inunit);
	exit(-1);
      }
    }
    
    ival = fread(inp, 1, 4, fp1);
    if (ival <= 0) {
      printf("\n+++ERROR READING CEDRIC FILE %s +++\n",filename);
      exit(-1);
    }
    
    if (strcmp(inp, CED) != 0) {
      printf("\n+++INPUT FILE FORMAT NOT RECOGNIZED ON %s +++\n",filename);
      exit(-1);
    }
    
    ival = fread(&byte, 1, 4, fp1);
    byte = byte >> (WORD_SIZE - 32);
    if (byte != SPRINT_BYTE_ORDER) {   /* byte swapping needs to be done */
      printf("\n+++ERROR: CANNOT HAVE TWO DIFFERENT BYTE ORDERINGS IN SAME FILE. WRITE TO A DIFFERENT OUTPUT FILE+++\n");
      exit(-1);
    }
    
    fseek(fp1,8,1);  /* skip past length info */
    
    /* find next available slot in output file */
    
    ival = fread(&jval, 4, 1, fp1);
    jval = jval >> (WORD_SIZE - 32);   
    count = 1;
    
    while (jval != 0) {
      ival = fread(&jval, 4, 1, fp1);
      if (ival <= 0) {
	printf("\n+++ERROR READING FROM FILE %s+++\n",filename);
	exit(-1);
      }
      jval = jval >> (WORD_SIZE - 32);   
      count = count +1;
      if (count > MAXVOL) { /* external file is too big */
	printf("\n+++ERROR: OUTPUT FILE CANNOT CONTAIN MORE THAN %d CEDRIC VOLUMES+++\n",MAXVOL);
	exit(-1);
      }
    }
    
    ipos = ftell(fp1);
    ipos = ipos - 4;
    
    fseek(fp1,0,2);
    jpos = ftell(fp1) + 1;
    fseek(fp1,ipos,0);
    jpos = jpos << (WORD_SIZE - 32);   /* shift to upper 32 bits */
    ival = fwrite(&jpos, 4, 1, fp1);
    
    /* now output the descriptive character string for this volume */
    for (i = 0 ; i < (25 - count); i++) {
      fseek(fp1,4,1);
    }

    for (i = 0; i < (count - 1); i++) {
      fseek(fp1,56,1);
    }

    ival = fwrite(IBUF,56,1,fp1);
    if (ival <= 0) {
      printf("\n+++ERROR WRITING TO FILE %s +++\n", filename);
      exit(-1);
    }

    jpos = jpos >> (WORD_SIZE - 32);
    fseek(fp1,jpos,0); /* go to end of file for adding additional volume*/
    jpos = ftell(fp1);
    
    open_files->curr_vol = count;
  }    
  return;
}



#ifdef linux
void two_byte_swap(unsigned int a[], int n)
{
  /* Swap pairs of two bytes:  aBCd becomes CdaB. */
  union {
	unsigned char byte[4];
	unsigned int val;
  } word;

  int i;
  unsigned char tmp0, tmp1;

  for (i=0; i<n; i++) {
	word.val = a[i];
	tmp0 = word.byte[0];
	tmp1 = word.byte[1];
	word.byte[0] = word.byte[2];
	word.byte[1] = word.byte[3];
	word.byte[2] = tmp0;
	word.byte[3] = tmp1;
	a[i] = word.val;
  }
  return;
}
#endif


/* this function writes 16 bit int values to disk */
#if defined (IBMRISC) || defined (HP)
void cwrite(IARRAY,NVAL)
#elif defined (CRAY)
void CWRITE(IARRAY,NVAL)
#else
void cwrite_(IARRAY,NVAL)
#endif
     int IARRAY[1];
     int *NVAL;
{
  int ival, jval, fval;
  float test;
  int diff;
  int n;
  
  fval = 0;
  test = (float) *NVAL/(WORD_SIZE/16.0);
  jval = (int) *NVAL/(WORD_SIZE/16);
  diff = (test - (float)jval)*(WORD_SIZE/16);

  if (((float)jval != test) && SPRINT_BYTE_ORDER) {
	printf("CWRITE: nval is not integral of wordsize. NVAL=%d\n", *NVAL);
  
	n = *NVAL-diff;
#ifdef linux
	two_byte_swap(IARRAY, n); /* To avoid using this routine, you must
							   * determine where the flaw is in how
							   * values are loaded into IARRAY;
							   * the flaw is for little endian machines.
							   */
#endif
	ival = fwrite(IARRAY, 2, *NVAL-diff, fp1);
    if (ival <= 0) {
      printf("\n+++ERROR WRITING CEDRIC FILE TO DISK+++\n");
      exit(-1);
    }
    fval = IARRAY[jval] >> (WORD_SIZE - 16*diff);
    ival = fwrite(&fval, 2, diff, fp1);
    if (ival <= 0) {
      printf("\n+++ERROR WRITING CEDRIC FILE TO DISK+++\n");
      exit(-1);
    }
  } 
  else {
#ifdef linux
	two_byte_swap(IARRAY, *NVAL); /* Must find the flaw on how the 
								   * values are loaded into IARRAY.
								   * The flaw is for little endian machines.
								   */
#endif
    ival = fwrite(IARRAY, 2, *NVAL, fp1);
    if (ival <= 0) {
      printf("\n+++ERROR WRITING CEDRIC FILE TO DISK+++\n");
      exit(-1);
    }

  }
  return;
}

/* this function updates the length field for the file */
void CLEN()
{
  int ival;
  int jpos;

  jpos = ftell(fp1);
  jpos = jpos << (WORD_SIZE - 32);
  fseek(fp1,12,0);
  ival = fwrite(&jpos,1,4,fp1);
  jpos = jpos >> (WORD_SIZE - 32);
  fseek(fp1,jpos,0);

  return;
}

/* this function returns the byte ordering of the machine */
#if defined (IBMRISC) || defined (HP)
void cbyte(MBYTE)
#elif defined (CRAY)
void CBYTE(MBYTE)
#else
void cbyte_(MBYTE)
#endif
     int *MBYTE;
{
  *MBYTE = SPRINT_BYTE_ORDER;

  return;
}

/* this function closes all open files */
#if defined (IBMRISC) || defined (HP)
void cclose()
#elif defined (CRAY)
void CCLOSE()
#else
void cclose_()
#endif
{
  int ret;
  
  while (open_files != NULL) {
#if !defined(__sgi)
    ret = fclose(open_files->fps);
    if (ret != 0) {
      printf("\n+++ERROR CLOSING OUTPUT FILE+++\n");
      exit(-1);
    }
#endif
    open_files = open_files->next;
  }
  
  return;
}

