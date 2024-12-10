# include <stdio.h>
# include "dorade.h"


struct radd_blk *radds[MXRAD];  /* one for each radar */
struct cspd_blk *cspds[MXRAD];
struct metadata *data_info[MXRAD];
char arr_ryib[MXBYTES];
char arr_asib[MXBYTES];
char arr_frad[MXBYTES];
char arr_rdat[MXBYTES];
char arr_volhead[MXBYTES];
int  len_ryib;
int  len_asib;
int  len_frad;
int  len_rdat;
int  len_volhead;
int skipping = 0;       /* 1 -> in process of skipping; used when writing
			 * out new volumes.
			 */

/* The following C function initiates the reading of a beam from a
 * DORADE or ELDORA field format data file. 
 *
 * Input:
 * cradnam   - Radar name in C format (as opposed to FORTRAN format)
 * 
 * Output:
 * istat     - Status flag (0: normal, 1: new sweep, 2: new dorade volume, 
 *                          3: EOD, 4: other)
 *
 */

void rdbeam2(cradnam, reqtime, inunit, irewnd, istat, ivolnum, 
	     iyr, mon, iday, ihr, min, isec, msec, cfltnum, numrads, 
	     radcon, iscnmode, scanrate, nflds, radlong, radlat, 
	     altmsl, altgnd, presalt, vnyq, rngmx, numfreq, numipp,
	     frstgat, gatspac, ngates, iswpnum, julday, azim, elev, 
	     irystat, gndspdew, gndspdns, vervel, heading, roll, 
	     pitch, drift, rotang, tilt, uair, vair, wair, hedchgrt, 
	     pitchgrt, flddat, cfldnam, scale, offset, bad, fxang)
     
     int *istat, *irewnd, *inunit;
     float  *reqtime;
     
     int *ivolnum, *iyr, *mon, *iday, *ihr, *min, *isec, *msec;
     int *numrads, *iscnmode, *nflds, *numfreq, *numipp, *ngates;
     int *iswpnum, *julday, *irystat;
     
     float  *radcon, *scanrate, *radlong, *radlat, *vnyq, *rngmx;
     float  *frstgat, *gatspac, *azim, *elev, *altmsl, *presalt, *altgnd;
     float  *gndspdew, *gndspdns, *vervel, *heading, *roll, *pitch, *drift;
     float  *rotang, *tilt, *uair, *vair, *wair, *hedchgrt;
     float  *pitchgrt, flddat[MXFLD][MXGAT], scale[MXFLD], offset[MXFLD];
     float  *bad, *fxang;
     char cfltnum[9], cradnam[9], cfldnam[MXFLD][9];
{
  int i,j,k,i1,i2,ij;
  static FILE  *fpw;
  static int fd;
  int iunpck = 2;                        /* unpack the data into meteo. units */
  int rval, rnum;
  int iprinth = 0, iprintr = 0;
  char lastrad[9], filename[8], radname[9];
  static struct files *open_files;       /* linked list of open file info */
  static struct files *head;             /* head of linked list */
  static int lastunit = -1;
  static float  time = 0.0;
  static int year, month, day;
  struct vold_blk vold;
  struct radd_blk radd;
  struct cfac_blk cfac;
  struct parm_blk parm;
  struct cspd_blk cspd;
  struct celv_blk celv;
  static struct swib_blk swib;
  struct ryib_blk ryib;
  struct asib_blk asib;
  struct frad_blk frad;
  struct rdat_blk rdat;
  static int nrads;
  static int write_to_file = 0;
  
  
  fpw = NULL;
  

  if (lastunit != *inunit) { /* get file pointer for requested unit number */
    lastrad[0] = '\0';
    lastunit = *inunit;
    time = 0.0;
    
    if (*inunit < 10 || *inunit > 99) {
      printf("\n +++ Invalid unit number %d +++\n",*inunit);
      exit(1);
    }
    i1 = *inunit / 10 ;
    i2 = *inunit % 10 ;
    
    filename[0] = 'f';
    filename[1] = 'o';
    filename[2] = 'r';
    filename[3] = 't';
    filename[4] = '.';
    filename[5] = i1 + 48;  /* convert to ascii */
    filename[6] = i2 + 48;  /* convert to ascii */
    filename[7] = '\0';
    printf("filename=%s\n",filename);
    
    if (head == NULL) {  /* start linked list of open files */
      open_files = (struct files *)malloc(sizeof(struct files));
      if (open_files == NULL) { /* error getting address */
	printf("\n+++ OUT OF MEMORY IN RDBEAM2+++\n");
	exit(-1);
      }
      head = open_files;
      open_files->next = NULL;
      open_files->unit = -99;
    } 
    open_files = head;
    while (open_files->next != NULL  &&  open_files->unit != *inunit) {
      open_files = open_files->next;
    }
    if (open_files->unit != *inunit) { /* file is not already in list;open it*/
      
      
      fd = myopen(filename);
      printf("fd = %d\n", fd);
      if (fd < 0) {
	printf("\n+++ERROR OPENING %s FOR READING+++ \n",filename);
	exit(-1);
      }
      if (open_files->unit > 0) {
	/* add new open file to list of open files */
	open_files->next = (struct files *) malloc(sizeof(struct files));
	if (open_files->next == NULL) {
	  printf("\n+++OUT OF MEMORY IN RDBEAM2+++\n");
	  exit(-1);
	}
	open_files = open_files->next;
      }
      open_files->unit = *inunit;
      open_files->fds = fd;
      open_files->next = NULL;
    }
    else {
      fd = open_files->fds;
    }
    /* transfer volume and radar info */
    for (i = 0; i < MXRAD; i++) {
      radds[i] = &(open_files->radds[i]);
      cspds[i] = &(open_files->cspds[i]);
      data_info[i] = &(open_files->data_info[i]);
    }
    *ivolnum = open_files->volnum;
    *iyr     = open_files->year;
    year     = *iyr;
    *mon     = open_files->month;
    month    = *mon;
    *iday    = open_files->day;
    day      = *iday;
    *numrads = open_files->numrads;
    strcpy(cfltnum, vold.flt_number);
  }
  
  
  if (*irewnd) { /* reposition stream to beginning */
    rval = lseek(fd, 0, 0);
    time = 0.0;
    if (rval != 0) {
      *istat = 4;
      return;
    }
  }
  


  /* if a beam from a different radar is being sought, reset time so that
   * time from last radar doesn't screw up search for time for this radar
   */
  if (strcmp(cradnam, lastrad) != 0) time = 0.0;
  
  /* get into requested time window for requested radar */
  assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, &swib, 
		&ryib, &asib, &frad, &rdat, iprinth, iprintr, time, 
		reqtime, flddat, istat, iunpck, cfldnam, &write_to_file);
  if (*istat > 2) return;

  if (time < *reqtime) {
    while (time < *reqtime) { /* skip past beams until we find time we want */
      assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, &swib, 
		    &ryib, &asib, &frad, &rdat, iprinth, iprintr, time, 
		    reqtime, flddat, istat, iunpck, cfldnam, &write_to_file);
      if (*istat > 2) return;
      
      
      if (strcmp(swib.radname, cradnam) == 0 || strcmp(cradnam, "") == 0) {
	/* if right radar */
	time = ryib.hour*10000. + ryib.min*100 + ryib.sec + ryib.msec/1000. ;
      }
    }
  }
  else {
    while (strcmp(swib.radname, cradnam) && strcmp(cradnam, "")) {
      
      assemble_beam(fd, fpw, &vold, &radd, &cfac, &parm, &cspd, &celv, &swib, 
		    &ryib, &asib, &frad, &rdat, iprinth, iprintr, time, 
		    reqtime, flddat, istat, iunpck, cfldnam, &write_to_file);
      if (*istat > 2) return;

    }
  }



/* At this point, we presumably have our beam. If a new volume header was
 * read, update all the volume info. 
 */
  if (*istat == 2) {/* update volume info since a new volume header was found */
    lastrad[0] = '\0';
    open_files->volnum  = vold.volnum;
    open_files->year    = vold.year;
    open_files->month   = vold.month;
    open_files->day     = vold.day;
    open_files->numrads = vold.num_sens;
    strcpy(open_files->cfltnum, vold.flt_number);
    
    *ivolnum  = vold.volnum;
    *iyr      = vold.year;
    year      = *iyr;
    *mon      = vold.month;
    month     = *mon;
    *iday     = vold.day;
    day       = *iday;
    *numrads  = vold.num_sens;
    strcpy(cfltnum, vold.flt_number);
    
  }
  

  /* see if the radar info we're returning needs to be updated (in case
   * the current beam is from a different radar than the previous one).
   */
/*  if (strcmp(swib.radname, lastrad) != 0) { */
    rnum = -1;
    for (i = 0; i < MXRAD; i++) {
      if (strcmp(swib.radname, radds[i]->radnam) == 0) rnum = i;
    }
    if (rnum == -1) {
      printf("\n   +++Error locating radar %s in data set.+++\n",cradnam);
      printf("      Radars in dataset:\n");
      for (i = 0; i < *numrads; i++) {
	printf("      '%s'\n", radds[i]->radnam);
      }
      *istat = 4;
      return;
    }
    else { /* found radar, transfer data */


      strcpy(lastrad, swib.radname);
      *radcon   = radds[rnum]->radcon;
      *iscnmode = radds[rnum]->scan_mode;
      *nflds    = radds[rnum]->tot_par;
      *radlong  = radds[rnum]->rad_long;
      *radlat   = radds[rnum]->rad_lat;
      *vnyq     = radds[rnum]->nyq;
      *rngmx    = radds[rnum]->max_range;
      *numfreq  = radds[rnum]->num_freq;
      *numipp   = radds[rnum]->num_ip;
      
      /* calc. range gate info */
      *ngates  = data_info[rnum]->ngates;
      *frstgat = data_info[rnum]->frstgat;
      *gatspac = data_info[rnum]->gatspac;
      
      for (i = 0; i < *nflds ; i++) { /* transfer field names */
	strcpy(cfldnam[i], data_info[rnum]->fields[i]);
      }
      
      *iyr = year;
      *mon = month;
      *iday= day;

    }
/*  } */
  

  /* transfer beam specific information to variables to be returned */
  *iswpnum = ryib.swpnum;
  *julday  = ryib.day;
  *ihr     = ryib.hour;
  *min     = ryib.min;
  *isec    = ryib.sec;
  *msec    = ryib.msec;
  *azim    = ryib.az;
  *elev    = ryib.elev;
  *irystat = ryib.status;
  *fxang   = swib.fix_ang;
  
  *radlat   = asib.lat;
  *radlong  = asib.lon;
  *presalt  = asib.pres_alt;
  *altgnd   = asib.alt_gnd;
  *gndspdew = asib.gndspd_ew;
  *gndspdns = asib.gndspd_ns;
  *vervel   = asib.ver_vel;
  *heading  = asib.heading;
  *roll     = asib.roll;
  *pitch    = asib.pitch;
  *drift    = asib.drift;
  *rotang   = asib.rot_ang;
  *tilt     = asib.tilt_ang;
  *uair     = asib.wndspd_ew;
  *vair     = asib.wndspd_ns;
  *wair     = asib.verspd;
  *hedchgrt = asib.head_rate;
  *pitchgrt = asib.ptch_rate;
  

  return;
  
}


