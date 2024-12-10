# include <stdio.h>
#ifdef hp
/*#include <sys/types.h>
#include <netinet/in.h>
*/
#endif
# include "dorade.h"

#if defined (IBMRISC) || defined (HP)
void rdbeam
#elif defined (CRAY)
void RDBEAM
#elif defined (linux)
void rdbeam_
#else
void rdbeam_
#endif
(radnam, reqtime, inunit, irewnd, istat, ivolnum, 
             iyr, mon, iday, ihr, min, isec, msec, fltnum, numrads, 
             radcon, iscnmode, scanrate, nflds, radlong, radlat, 
             altmsl, altgnd, presalt, vnyq, rngmx, numfreq, numipp, 
             frstgat, gatspac, ngates, iswpnum, julday, azim, elev, 
             irystat, gndspdew, gndspdns, vervel, heading, 
	     roll, pitch, drift, rotang, tilt, uair, vair, wair, 
	     hedchgrt, pitchgrt, flddat, fldnam, scale, offset, bad,
	     fxang)

     int *istat;
     float  *reqtime;
     char fltnum[8], radnam[8], fldnam[MXFLD][8];

     int *ivolnum, *iyr, *mon, *iday, *ihr, *min, *isec, *msec;
     int *numrads, *iscnmode, *nflds, *numfreq, *numipp, *ngates;
     int *iswpnum, *julday, *irystat;

     float *radcon, *scanrate, *radlong, *radlat, *vnyq, *rngmx;
     float *frstgat, *gatspac, *azim, *elev, *altmsl, *presalt, *altgnd;
     float *gndspdew, *gndspdns, *vervel, *heading, *roll, *pitch, *drift;
     float *rotang, *tilt, *uair, *vair, *wair, *hedchgrt;
     float *pitchgrt, flddat[MXFLD][MXGAT], scale[MXFLD], offset[MXFLD];
     float *bad, *fxang;
     int *inunit, *irewnd;
{

  int i, j, ij;
  static char cradnam[9];
  char cfltnum[9];
  static char cfldnam[MXFLD][9];
  void rdbeam2();
  
/* convert FORTRAN char. to C */

  
  cradnam[8]='\0';
  for (i = 0; i < 8; i++) {
/*    if (radnam[i] != ' ') { */
      cradnam[i] = radnam[i];
/*    }
    else {
      cradnam[i] = '\0';
    }
*/  
  }
  *istat = 0;

/* go and read next beam */


  rdbeam2(cradnam, reqtime, inunit, irewnd, istat, ivolnum, 
	  iyr, mon, iday, ihr, min, isec, msec, cfltnum, numrads, 
	  radcon, iscnmode, scanrate, nflds, radlong, radlat, 
	  altmsl, altgnd, presalt, vnyq, rngmx, numfreq, numipp, 
	  frstgat, gatspac, ngates, iswpnum, julday, azim, elev, 
	  irystat, gndspdew, gndspdns, vervel, heading, 
	  roll, pitch, drift, rotang, tilt, uair, vair, wair, 
	  hedchgrt, pitchgrt, flddat, cfldnam, scale, offset, bad, 
          fxang);

/* convert C chars to FORTRAN */

  strncpy(fltnum,"        ",8);
  strncpy(fltnum,cfltnum,8);
/*  printf("cfltnum=%s\n",cfltnum); */

  for (i = 0; i < *nflds; i++) {
    strncpy(fldnam[i], "        ", 8);
    strcpy(fldnam[i], cfldnam[i]);
  }

/*  for (ij=0; ij < 7; ij++) {
    printf("cfldnam=%s\n",cfldnam[ij]);
    printf("fldnam=%s\n",fldnam[ij]);
  }
*/


  return;

}
