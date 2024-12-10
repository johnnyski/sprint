/*

 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1988                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR GRAPHICS V2.00                         *
*                                                                      *
***********************************************************************/

/*
	Logical operations for 32-bit systems.
*/
long

#if defined (IBMRISC) || defined (_HPUX_SOURCE)
    ishift(i, nshift)
#elif defined (CRAY)
    ISHIFT(i, nshift)
#else
    ishift_(i, nshift)
#endif

/* shift the 4 byte integer i by nshift bits  */
/* if nshift is negative, right shift end off zero fill  */
/* if nshift is positive, left shift end around  */
/* the routine behaves properly if magnitude of nshift > 32  */
	long           *i, *nshift;
{
	long            jshift, nbits;
	if (*nshift < 0) {
		nbits = (*nshift < -32 ? 32 : -*nshift);
		jshift = (*i >> nbits) & (017777777777 >> (nbits - 1));
	} else {
		nbits = *nshift % 32;
		jshift = (*i << nbits) | ((*i >> (32 - nbits))
					  & (~(037777777777 << nbits)));
	}
	return (jshift);
}

/* integer valued function to return logical AND of i and j */
/* i and j are assumed to be 4 byte integers  */
long
iand(i, j)
	long	*i, *j;
{
	return (*i & *j);
}

/* integer valued function to return logical OR of i and j  */
/* i and j are assumed to be 4 byte integers  */
long
ior(i, j)
	long	*i, *j;
{
	return (*i | *j);
}
