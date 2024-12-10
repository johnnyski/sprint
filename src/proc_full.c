#include <stdio.h>
#include "dorade.h"

extern int blknum;
extern struct metadata *data_info[MXRAD];
extern struct radd_blk *radds[MXRAD];
extern struct cspd_blk *cspds[MXRAD];

/* The following function controls the processing of dorade blocks. 
 * Every block is printed out in the order it appears in the input
 * dataset. No attemp to define a beam (as in proc_summ) is made.
 */

void proc_full(fd, fpw, skpnum, skpunits, procnum, prcevry, reqtime, reqrad,
	       printstr)
     FILE *fpw;
     int fd;
     int skpnum, procnum, prcevry;
     float reqtime;
     char *skpunits, reqrad[], printstr[];
{
  int i, j, k;
  int ngates, numflds, rnum;
  float time = 0.0;
  struct vold_blk vold;
  struct radd_blk radd;
  struct cfac_blk cfac;
  struct parm_blk parm;
  struct cspd_blk cspd;
  struct celv_blk celv;
  struct swib_blk swib;
  struct ryib_blk ryib;
  struct asib_blk asib;
  struct frad_blk frad;
  struct rdat_blk rdat;
  float flddat[MXFLD][MXGAT];
  char cfldnam[MXFLD][9];
  void read_block();
  char blknam[5];
  int istat = 0;
  int icnt, iprinth = 1, iprintr = 0;
  int iunpck;
  static int write_to_file = 0;
  
  icnt    = 0;
  iprintr = 0;
  iunpck  = 0;
  if (strcmp(skpunits,"DORBLCKS") == 0) {
    /* process (skip, dump) by Dorade block */
    while (icnt < skpnum) { /* skip over blocks */
      read_block(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, &swib, &ryib, 
		 &asib, &frad, &rdat, iprinth, iprintr, blknam, time, 
		 &reqtime, flddat, &istat, iunpck, cfldnam, &write_to_file);
      if (istat == 3 || istat == 4) {
	printf("\n +++ End of Data or I/O Error +++\n");
	exit(1);
      }
      icnt++;
    }
    icnt = 0;
    iprintr = 1;
    iunpck = 1;
    while (icnt < procnum) { /* dump blocks */
      if ((icnt % prcevry) == 0) {
	iprintr = 1;
      }
      else {
	iprintr = 0;
      }
      read_block(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, &swib, &ryib, 
		 &asib, &frad, &rdat, iprinth, iprintr, blknam, time, 
		 &reqtime, flddat, &istat, iunpck, cfldnam, &write_to_file);
      if (istat == 3 || istat == 4) {
	printf("\n +++ End of Data or I/O Error +++\n");
	exit(1);
      }
      icnt++;
    }      
  }
  else {  
    /* process by beams */
    if (skpnum > 0) {
      while (icnt < skpnum) { 
	assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, 
		      &swib, &ryib, &asib, &frad, &rdat, iprinth, iprintr, 
		      time, &reqtime, flddat, &istat, iunpck, cfldnam, &write_to_file);
	if (istat == 3 || istat == 4) {
	  printf("\n +++ End of Data or I/O Error +++\n");
	  exit(1);
	}
	if (strcmp(swib.radname, reqrad) == 0 || reqrad[0]=='\0') {
	  icnt++;
	}
      }
    }
    else if (reqtime > 0.0) { /* skip to requested time */
      while (time < reqtime) {
	assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, 
		      &swib, &ryib, &asib, &frad, &rdat, iprinth, iprintr, 
		      time, &reqtime, flddat, &istat, iunpck, cfldnam, &write_to_file);
	if (istat == 3 || istat == 4) {
	  printf("\n +++ End of Data or I/O Error +++\n");
	  exit(1);
	}	
	if (strcmp(swib.radname, reqrad) == 0 || reqrad[0]=='\0') {
	  time = ryib.hour*10000. + ryib.min*100 + ryib.sec + ryib.msec/1000. ;
	}
      }
    }
    
    
    icnt    = 0;
    iprintr = 0;
    iunpck  = 1;
    while (icnt < procnum) {
      if (reqtime == 0.0) {/* only read beam if we didn't first skip by beam */
	assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, 
		      &swib, &ryib, &asib, &frad, &rdat, iprinth, iprintr, 
		      time, &reqtime, flddat, &istat, iunpck, cfldnam, &write_to_file);
	if (istat == 3 || istat == 4) {
	  printf("\n +++ End of Data or I/O Error +++\n");
	  exit(1);
	}
      }
      else {
	reqtime = 0.0;
      }
      if (strcmp(swib.radname,reqrad) == 0 || reqrad[0]=='\0') {
	if ((icnt % prcevry) == 0) { 
	  
	  rnum = -1;
	  for (i = 0; i < MXRAD; i++) {
	    if (strcmp(swib.radname, radds[i]->radnam) == 0) rnum = i;
	  }
	  if (rnum == -1) {
	    printf("\n    +++Error. Could not find radar %s in radar list.\n",
		   swib.radname);
	    exit(1);
	  }
	  
	  ngates  = data_info[rnum]->ngates;
	  numflds = data_info[rnum]->numflds;
	  
	  printf("\nRay Info Block (dorade block # %d):\n",(blknum-2));
	  printstr[0] = '\0';
	  strcat(printstr,"   swpnum=%8d      day=%8d     hour=%8d      min=%8d");
	  strcat(printstr,"      sec=%8d     msec=%8d       az=%8.3f\n     elev=%8.3f");
	  strcat(printstr,"    pkpwr=%8.3f     rate=%8.3f   status=%8d\n");
	  
	  printf(printstr ,ryib.swpnum, ryib.day, ryib.hour, ryib.min, ryib.sec,
		 ryib.msec, ryib.az, ryib.elev, ryib.pkpwr, ryib.rate, ryib.status);
          printf("\nPlatform Info Block (dorade block # %d):\n",(blknum-1));
	  printstr[0] = '\0';
	  strcat(printstr,"     long=%8.3f      lat=%8.3f pres_alt=%8.3f  alt_gnd=%8.3f");
	  strcat(printstr," gndsp_ew=%8.3f gndsp_ns=%8.3f  ver_vel=%8.3f\n");
	  strcat(printstr,"  heading=%8.3f     roll=%8.3f    pitch=%8.3f    drift=%8.3f");
	  strcat(printstr,"  rot_ang=%8.3f tilt_ang=%8.3f wndsp_ew=%8.3f\n");
	  strcat(printstr," wndsp_ns=%8.3f   verspd=%8.3f  headchg=%8.3f");
	  strcat(printstr," pitchchg=%8.3f\n");
	  
	  printf(printstr, asib.lon, asib.lat, asib.pres_alt, asib.alt_gnd,
		 asib.gndspd_ew, asib.gndspd_ns, asib.ver_vel, asib.heading, asib.roll,
		 asib.pitch, asib.drift, asib.rot_ang, asib.tilt_ang, asib.wndspd_ew,
		 asib.wndspd_ns, asib.verspd, asib.head_rate, asib.ptch_rate);
	  printf("\nRDAT block!!\n");


	  
	  /* commented out; use only for field format data 	
	   *	  printf("\nField Radar Block (dorade block # %d):\n",blknum);
	   *	  printf("  radname=%8s   raycnt=%8d firstgat=%8d  lastgat=%8d",
	   *		 frad.radname, frad.ray_count,frad.firs_gate,frad.last_gate);
	   for (i = 0; i < numflds; i++) {
	   printf("\n\n    field=%8s    scale=%8.3f   offset=%8.3f      bad=%8.3f\n",
	   data_info[rnum]->fields[i], data_info[rnum]->scale[i],
	   data_info[rnum]->offset[i],data_info[rnum]->bad[i]);
	   printf("data values = \n");
	   for (j = 0 ; j < ngates; j++) {
	   printf("%5.0f ",flddat[i][j]);
	   if (((j+1) % 20) == 0) printf("\n");  
	}
      }
      */
	  printf("\n");
	}
	icnt++;
      }
    }
  }
  return;
}


