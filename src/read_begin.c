#include <stdio.h>
#include "dorade.h"

extern char arr_ryib[MXBYTES];
extern char arr_asib[MXBYTES];
extern char arr_frad[MXBYTES]; 
extern char arr_rdat[MXBYTES];
extern char arr_volhead[MXBYTES];
extern int  len_ryib;
extern int  len_asib;
extern int  len_frad;
extern int  len_rdat;
extern int  len_volhead;
extern int  skipping;
extern struct radd_blk *radds[MXRAD];
extern struct cspd_blk *cspds[MXRAD];
extern struct metadata *data_info[MXRAD];
extern int blknum;

/*
 * The following function reads in blocks, determines the type (name) and
 * calls a function for further processing, depending on the name. This
 * function assumes that the input stream is positioned at the start of
 * a dorade block.
 */
void read_block(fd, fpw, vold, radd, cfac, parm, cspd, celv, swib, ryib, asib, 
                frad, rdat, iprinth, iprintr, blknam, time, reqtime, flddat, 
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
     char blknam[5];
     float time, *reqtime;
     float flddat[MXFLD][MXGAT];
     int *istat;
     int iunpck;
     char cfldnam[MXFLD][9];
     int *write_to_file;
{
  
  int rval, len, iread, ival, ij;
  static int radd_cnt = 0;
  static int radd_cur = 0;
  static int parm_cnt = 0;
  static int new_vol = 0;    
  int it1,it2,it3,it4;
  int i;
  char array[MXBYTES];
  char ctemp[8];
  struct vold_blk read_vold();
  struct radd_blk read_radd();
  struct cfac_blk read_cfac();
  struct parm_blk read_parm();
  struct cspd_blk read_cspd();
  struct celv_blk read_celv();
  struct swib_blk read_swib();
  struct ryib_blk read_ryib();
  struct asib_blk read_asib();
  struct frad_blk read_frad();
  struct rdat_blk read_rdat();
  
  
  blknam[4]='\0';
  
  
  rval = myread(array, 8, fd);
  if (rval <= 0) {
    rval = myread(array, 8, fd);
    if (rval <= 0) {
      *istat = 3;
      return;
    }
  }     
  
  memcpy(ctemp, array, 8);

  /* transfer name of block to another array */
  blknam[0]=array[0];
  blknam[1]=array[1];
  blknam[2]=array[2];
  blknam[3]=array[3];
  
  
  if (strcmp(blknam, "VOLD") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    *vold = read_vold(fd, array, *vold, iprinth, istat);
    /* turn on flag to create a new dorade output volume
     * if following conditions are met; used to extract a subset
     * of a larger dorade dataset
     */
    if (*istat == 2 && skipping == 0 && fpw) *write_to_file = 1;
    if (*istat > 2) return;
    /* reset counters so that radar params will be saved correctly */
    radd_cnt = -1;
    parm_cnt = -1;
    for (i = 0; i < MXRAD; i++) {
      data_info[i]->numflds = 0;
    }
    printf("*write_to_file=%d\n", *write_to_file);
    if (*write_to_file) {
      len_volhead = 0;
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
  else if (strcmp(blknam, "RADD") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    *radd = read_radd(fd, array, *radd, iprinth, istat);
    if (*istat > 2) return;
    /* save radar parms for this radar */
    radd_cnt++;
    if ((radd_cnt + 1) > MXRAD) {
      printf("\n  +++Too many radars in volume. Max. allowed = %d\n",
	     MXRAD);
      *istat = 4;
      return;
    }
    parm_cnt = -1;
    strcpy(radds[radd_cnt]->radnam, radd->radnam);
    radds[radd_cnt]->scan_mode = radd->scan_mode;
    radds[radd_cnt]->scan_rate = radd->scan_rate;
    radds[radd_cnt]->tot_par   = radd->tot_par;
    radds[radd_cnt]->rad_long  = radd->rad_long;
    radds[radd_cnt]->rad_lat   = radd->rad_lat;
    radds[radd_cnt]->rad_alt   = radd->rad_alt;
    radds[radd_cnt]->nyq       = radd->nyq;
    radds[radd_cnt]->max_range = radd->max_range;
    radds[radd_cnt]->num_freq  = radd->num_freq;
    radds[radd_cnt]->num_ip    = radd->num_ip;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
  else if (strcmp(blknam, "PARM") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    *parm = read_parm(fd, array, *parm, iprinth, istat);
    if (*istat > 2) return;
    /* save field info for this radar */
    parm_cnt++;
    if ((parm_cnt + 1) > MXFLD) {
      printf("\n   +++Error. Too many fields in dataset. Max. allowed = %d\n",
	     MXFLD);
      exit(1);
    }
    strcpy(data_info[radd_cnt]->fields[parm_cnt], parm->name);
    data_info[radd_cnt]->fld_typ[parm_cnt] = parm->parm_type;
    data_info[radd_cnt]->scale[parm_cnt]   = parm->scale;
    data_info[radd_cnt]->offset[parm_cnt]  = parm->offset;
    data_info[radd_cnt]->bad[parm_cnt]     = parm->bad;
    data_info[radd_cnt]->numflds++;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
  else if (strcmp(blknam, "CELV") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    *celv = read_celv(fd, array, *celv, iprinth, istat);
    if (*istat > 2) return;
    data_info[radd_cnt]->ngates  = 0;   /* will be set in read_rdat */
    data_info[radd_cnt]->frstgat = celv->dist_to_fir;
    data_info[radd_cnt]->gatspac = (float) celv->spacing;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
    
  else if (strcmp(blknam, "CSPD") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    *cspd = read_cspd(fd, array, *cspd, iprinth, istat);
    if (*istat > 2) return;
    data_info[radd_cnt]->ngates = 0;
    for (i = 0; i < cspd->num_seg; i++) {
      data_info[radd_cnt]->ngates += cspd->num_cell[i];
      if (i > 0 && cspd->num_cell[i] > 0 && cspd->spacing[i] != 
	  cspd->spacing[i-1] ) {
	printf("\n     +++GATE SPACING NOT CONSTANT IN A GIVEN BEAM+++\n");
	exit(1);
      }
    }
    data_info[radd_cnt]->frstgat = cspd->dist_to_fir;
    data_info[radd_cnt]->gatspac = cspd->spacing[0];
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      memcpy(&(arr_volhead[len_volhead]), ctemp, 8);
      len_volhead += 8;
      memcpy(&(arr_volhead[len_volhead]), array, (len - 8));
      len_volhead += (len - 8);
    }
  }
  else if (strcmp(blknam, "SWIB") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    *swib = read_swib(fd, array, *swib, iprinth, istat);
    radd_cur = -1;
    for (i = 0; i <= radd_cnt; i++) {
      if (strcmp(swib->radname, radds[i]->radnam) == 0) {
	radd_cur = i;
	break;
      }
    }
    if (radd_cur == -1) {
      printf("\n  +++Error finding radar %s in list.+++\n", swib->radname);
      exit(1);
    }
    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
    }      
  }
  else if (strcmp(blknam, "RYIB") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    iread = 0;
    *ryib = read_ryib(fd, array, iread, *ryib, iprintr, istat);

    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
/*
      memcpy(arr_ryib, ctemp, 8);
      memcpy(&(arr_ryib[8]), array, (len - 8));
      len_ryib = len;
*/
    }
  }
  else if (strcmp(blknam, "ASIB") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    *asib = read_asib(fd, array, *asib, iprintr, istat);

    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
/*      memcpy(arr_asib, ctemp, 8);
      memcpy(&(arr_asib[8]), array, (len - 8));
      len_asib = len;
*/
    }
  }
  else if (strcmp(blknam, "FRAD") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);

    /* in order to speed up for skipping purposes, only unpack data if
     * we are in requested time window 
     */
    time = ryib->hour*10000. + ryib->min*100 + ryib->sec + ryib->msec/1000. ;
    if (time < *reqtime) iunpck = 0;
    *frad = read_frad(fd, array, *frad, iprintr, iunpck, flddat, istat);
    if (*istat > 2) return;
    if (*write_to_file) {
      memcpy(arr_frad, ctemp, 8);
      memcpy(&(arr_frad[8]), array, (len - 8));
      len_frad = len;
    }
  }
  else if (strcmp(blknam, "RDAT") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);

    /* in order to speed up for skipping purposes, only unpack data if
     * we are in requested time window 
     */
    time = ryib->hour*10000. + ryib->min*100 + ryib->sec + ryib->msec/1000. ;
    if (time < *reqtime) iunpck = 0;

    *rdat = read_rdat(fd, array, *rdat, iprintr, iunpck, flddat, istat,
		      radd_cur);
    if (*istat > 2) return;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
/*      memcpy(arr_rdat, ctemp, 8);
      memcpy(&(arr_rdat[8]), array, (len - 8));
      len_rdat = len;
*/
    }
  }    
  else if (strcmp(blknam, "RYIB") == 0) {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    iread = 1;
    *ryib = read_ryib(fd, array, iread, *ryib, iprintr, istat);
    if (*istat > 2) return;
    iread = 0;
    if (*write_to_file) {
      ival = fwrite(ctemp, 1, 8, fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
      ival = fwrite(array, 1, (len - 8), fpw);
      if (ival <= 0) {
	printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	exit(-1);
      }
/*      memcpy(arr_ryib, ctemp, 8);
      memcpy(&(arr_ryib[8]), array, (len - 8));
      len_ryib = len;
*/
    }
  }
  else {
    len = four_bytes_2_int(array[4],array[5],array[6],array[7]);
    if (len <= 0 || len > 1000000) {/* kludge to deal with record filler in data */
      len = find_ryib(fd, blknam, array, len, istat, ctemp);
      if (*istat > 2) return;
      blknam[0]='R';
      blknam[1]='Y';
      blknam[2]='I';
      blknam[3]='B';
      iread = 1;
      *ryib = read_ryib(fd, array, iread, *ryib, iprintr, istat);
      if (*istat > 2) return;
      iread = 0;
      if (*write_to_file) {
	ival = fwrite(ctemp, 1, 8, fpw);
	if (ival <= 0) {
	  printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	  exit(-1);
	}
	ival = fwrite(array, 1, (len - 8), fpw);
	if (ival <= 0) {
	  printf("\n+++ERROR WRITING TO OUTPUT UNIT +++\n");
	  exit(-1);
	}
/*	memcpy(arr_ryib, ctemp, 8);
	memcpy(&(arr_ryib[8]), array, (len - 8));
	len_ryib = len;
*/
      }
    }
    else {
      rval = myread(array, (len-8), fd);
/*      rval = lseek(fd, (len-8), SEEK_CUR); */
/*      rval=fseek(fp,(len-8),1);  */
/*      if (rval != 0) { */
      if (rval <= 0) {
	rval = myread(array, (len-8), fd);
	if (rval <= 0) {
	  *istat = 4;
	  return;
	}
      }
      blknum++;
      if (iprinth || iprintr)
      printf("\n%s descriptor found, but not dumped. length=%d bytes\n",blknam,len); 
    }
  }    
  
  
    return;
}
