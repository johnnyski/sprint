#include <stdio.h>
/*
 * This include file defines structures for the various blocks of the
 * dorade format. There is one structure per block. Other miscellaneous
 * declarations are also present in this file.
 */

#define MXBYTES    65536 /* largest dorade block able to be read in */
#define MXBLOCKS   50    /* max. # of dor. blocks to read in search of a beam*/
#define MXRAD      2     /* max. number of radars allowed in a single vol. */
#define MXFLD      8     /* max. # of fields */
#define MXGAT      768   /* max # of gates */
#define IEEEFLOAT  1     /* 1 ==> IEEE float,  0 ==> CRAY */
#define WORDSIZE   32



struct vold_blk {
  int ver;
  int volnum;
  int maxbyt;
  char proj_name[21];
  int year;
  int month;
  int day;
  int hour;
  int min;
  int sec;
  char flt_number[9];
  char gen_fac[9];
  int gen_yr;
  int gen_mon;
  int gen_day;
  int num_sens;
 } ;
struct radd_blk {
  char radnam[9];
  float radcon;
  float nompkpw;
  float nomnspw;
  float rcvgain;
  float antgain;
  float radgain;
  float horbmwth;
  float verbmwth;
  int rad_type;
  int scan_mode;
  float scan_rate;
  float start_ang;
  float stop_ang;
  int tot_par;
  int tot_des;
  int data_comp;
  int data_red;
  float data_red1;
  float data_red2;
  float rad_long;
  float rad_lat;
  float rad_alt;
  float nyq;
  float max_range;
  int num_freq;
  int num_ip;
  float freq1;
  float freq2;
  float freq3;
  float freq4;
  float freq5;
  float ipp1;
  float ipp2;
  float ipp3;
  float ipp4;
  float ipp5;
};

struct cfac_blk {
  float azim;
  float elev;
  float rng_del;
  float lon;
  float lat;
  float prs_alt;
  float alt;
  float gspd_ew;
  float gspd_ns;
  float ver_vel;
  float heading;
  float roll;
  float pitch;
  float drift;
  float rot_ang;
  float tilt;
};

struct parm_blk {
  char name[9];
  char desc[41];
  char units[9];
  int ipp;
  int tran_freq;
  float rec_band;
  int pul_wid;
  int polar;
  int num_sam;
  int parm_type;
  char thr_field[9];
  float thr_value;
  float scale;
  float offset;
  int bad;
};

struct cspd_blk {
  int num_seg;
  int dist_to_fir;
  int spacing[6];
  int num_cell[6];
};

struct celv_blk {
  int dist_to_fir;
  int num_cells;
  int spacing;
};

struct swib_blk {
  char radname[9];
  int swpnum;
  int nray;
  float start_ang;
  float stop_ang;
  float fix_ang;
  int filt;
};

struct ryib_blk {
  int swpnum;
  int day;
  int hour;
  int min;
  int sec;
  int msec;
  float az;
  float elev;
  float pkpwr;
  float rate;
  int status;
};

struct asib_blk {
  float lon;
  float lat;
  float pres_alt;
  float alt_gnd;
  float gndspd_ew;
  float gndspd_ns;
  float ver_vel;
  float heading;
  float roll;
  float pitch;
  float drift;
  float rot_ang;
  float tilt_ang;
  float wndspd_ew;
  float wndspd_ns;
  float verspd;
  float head_rate;
  float ptch_rate;
};


/* field radar data block. this block contains the data and some related
 * information. only the related information (e.g., radar name, ray counter)
 */
struct frad_blk {
  char radname[9];
  int ray_count;
  int firs_gate;
  int last_gate;
};


/* dorade radar data block. this block contains the actual data.
 */
struct rdat_blk {
  char par_name[9];
};


/* the following structure definition will be used to declare a global 
 * structure whose information will be used to unpack the actual data.
 */
struct metadata {
  int numradd;               /* number of radar descriptors encountered */
  char fields[MXFLD][9];    /* field names */
  int numflds;               /* number of fields  */
  int ngates;                /* number of range gates */
  float frstgat;             /* position to first gate */
  float gatspac;             /* gate spacing */
  float scale[MXFLD];       /* scale for field */
  float offset[MXFLD];      /* offset for field */
  float bad[MXFLD];         /* bad value for field */
  int fld_typ[MXFLD];       /* field type (i.e., 8-bit integer, 16-bit int,
                              * float, etc.)
                              */
};

struct files {
  int unit;
  int fds;
  struct radd_blk radds[MXRAD];
  struct cspd_blk cspds[MXRAD];
  struct metadata data_info[MXRAD];
  int volnum;
  int year;
  int month;
  int day;
  int numrads;
  char cfltnum[9];
  struct files *next;
};

