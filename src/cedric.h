/* header file for CEDRIC on Linux */

#define FALSE 0
#define TRUE  1
/*
 * BYTE_ORDER = 0 for big endian machines.
 *              1 for little endian machines.
 *
 *  -- This could be done portably via ntohl routines.
 *  -- As it is, I don't think the output is done properly;
 *  -- some header values are swapped as pairs of 16-bit values!
 *  -- Also there are some strange things happening,
 *  -- see the file SUNFOR.c and the swap32 routine in CINITCOS.c.
 *  -- However, data-wise, the output on Linux matches the HP.
 *  -- JHM 2/11/96.
 */
#include "config.h"
#ifdef WORDS_BIGENDIAN
  #define SPRINT_BYTE_ORDER 0
#else
  #define SPRINT_BYTE_ORDER 1
#endif

#define WORD_SIZE 32
#define CED "CED1"
#define FIRST_VOL 1540
#define MAXVOL    25 
#define EDIT_NAME ".cededit"
#define REMAP_NAME ".cedremap"

struct files {
  int unit;
  FILE *fps;
  int curr_vol;
  short swap;
  struct files *next;
} ;

#define EDIT_SIZE 16   /* size of edit volume divided by 1e+06 */
