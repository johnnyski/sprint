#include <stdio.h>
#include "dorade.h"

extern struct metadata *data_info[MXRAD];

/* The following function reads a beams worth of data from a dorade or
 * an eldora field format dataset. Note that a beam is several dorade
 * blocks so this function will read the number of blocks necessary
 * for a whole beam. This function assumes that a beam is read in when
 * we have read in a RYIB (Ray info block), an ASIB, and an FRAD
 * consecutively in that order. This assumption may have to be different
 * when reading eldora field format vs. "true" dorade.
 */
void assemble_beam(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib, ryib, 
		   asib, frad, rdat, iprinth, iprintr, time, reqtime, flddat, 
		   istat, iunpck, cfldnam, write_to_file)
     FILE *fpw;
     int fd;
     struct vold_blk *vold;
     struct radd_blk *radd;
     struct cfac_blk *cfac;
     struct parm_blk *parm;
     struct cspd_blk *cspd;
     struct celv_blk *celv;
     struct swib_blk *swib;
     struct ryib_blk *ryib;
     struct asib_blk *asib;
     struct frad_blk *frad;
     struct rdat_blk *rdat;
     int iprinth, iprintr;
     float time;
     float *reqtime;
     float flddat[MXFLD][MXGAT];
     int *istat, iunpck;
     char cfldnam[MXFLD][9];
     int *write_to_file;
{
  int i, j, k, ij;
  char blknam[5];
  char block[3][5];
  void read_block();


  blknam[4] = '\0';
/* start reading in blocks. Read in enough blocks to have a beam defined. */
  i = 0;
  while ( i < MXBLOCKS ) {
    read_block(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib, ryib, asib, 
               frad, rdat, iprinth, iprintr, blknam, time, reqtime, flddat, 
	       istat, iunpck, cfldnam, write_to_file);
    if (*istat > 2) return;
    
/* we have a ray when we have read in a RYIB, an ASIB, and several RDAT 
 * descriptors in that order. Thus, we keep searching for such consecutive 
 * blocks.
 */
    
    strcpy(block[0],block[1]);
    strcpy(block[1],block[2]);
    strcpy(block[2],blknam);

    if ((strcmp(block[0],"RYIB")==0) && (strcmp(block[1],"ASIB")==0)
     && (strcmp(block[2],"RDAT")==0)) { /* got start of a beam */
      for ( j = 1; j < data_info[0]->numflds; j++) {
	read_block(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib, ryib, 
		   asib, frad, rdat, iprinth, iprintr, blknam, time, reqtime, 
		   flddat, istat, iunpck, cfldnam, write_to_file);
	if (*istat > 2) return;
	if (strncmp(blknam, "RDAT", 4)) {
	  printf("\n +++ Error: could not find a sequence of dorade blocks that define");
	  printf(" a beam.\n");
	  *istat = 4;
	  return;
	}
      }
      return;
    }
    i++;
  }

  printf("\n +++ Error: could not find a sequence of dorade blocks that define");
  printf(" a beam.\n");
  *istat = 4;
  return;
}
    
    
  
