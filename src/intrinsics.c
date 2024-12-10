/*
 *
 * C interface to fortran intrinsic routines that f2c does not provide.
 *   
 */

int
#if defined(__linux) || defined(__sun) || defined(__sgi)
 ixor_
#else
 ixor
#endif
(int *w1, int *w2)
{
  return *w1 ^ *w2;
}


