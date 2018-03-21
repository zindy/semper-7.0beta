#include "ftypes.h"
#include "params.h"

#define all_zeros (F_bit)  0
#define all_ones  (F_bit) ~0
#define one       (F_bit)  1

subroutine bclear_ ( M1, M2, SOURCE, N, R )
/*=========================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
    int m1, m2, n, r, w, dw, fw, lw, i1, i2, w1, w2, b1, b2;
    F_bit mask;

    if ( *M1 > *N || *M2 < 1 || *M1 > *M2 || *R < 1 ) return;

    m1 = *M1;
    m2 = *M2;
    n  = *N;
    r  = *R;

    dw = ( ( n - 1 ) >> 5 ) + 1;
    lw = r * dw;

    i1 = ( m1 > 1 ? m1 : 1 ) - 1;
    i2 = ( m2 < n ? m2 : n ) - 1;

    w1 = i1 >> 5;
    w2 = i2 >> 5;

    b1 = i1 & 31;
    b2 = i2 & 31;

    if ( w1 == w2 )
    {
        mask = ~ ( ( all_ones << b1 ) & ( all_ones >> ( 31 - b2 ) ) );

        for ( w = w1; w < lw; w += dw ) SOURCE [ w ] &= mask;
    }

    else
    {
        mask = ~ ( all_ones << b1 );

        for ( w = w1; w < lw; w += dw ) SOURCE [ w ] &= mask;

        for ( fw = w1 + 1; fw <= w2 - 1; fw++ )
        {
            for ( w = fw; w < lw; w += dw ) SOURCE [ w ] = all_zeros;
        }

        mask = ~ ( all_ones >> ( 31 - b2 ) );

        for ( w = w2; w < lw; w += dw ) SOURCE [ w ] &= mask;
    }
}
subroutine bset_ ( M1, M2, SOURCE, N, R )
/*=======================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
    int m1, m2, n, r, w, dw, fw, lw, i1, i2, w1, w2, b1, b2;
    F_bit mask;

    if ( *M1 > *N || *M2 < 1 || *M1 > *M2 || *R < 1 ) return;

    m1 = *M1;
    m2 = *M2;
    n  = *N;
    r  = *R;

    dw = ( ( n - 1 ) >> 5 ) + 1;
    lw = r * dw;

    i1 = ( m1 > 1 ? m1 : 1 ) - 1;
    i2 = ( m2 < n ? m2 : n ) - 1;

    w1 = i1 >> 5;
    w2 = i2 >> 5;

    b1 = i1 & 31;
    b2 = i2 & 31;

    if ( w1 == w2 )
    {
        mask = ( all_ones << b1 ) & ( all_ones >> ( 31 - b2 ) );

        for ( w = w1; w < lw; w += dw ) SOURCE [ w ] |= mask;
    }

    else
    {
        mask = all_ones << b1;

        for ( w = w1; w < lw; w += dw ) SOURCE [ w ] |= mask;

        for ( fw = w1 + 1; fw <= w2 - 1; fw++ )
        {
            for ( w = fw; w < lw; w += dw ) SOURCE [ w ] = all_ones;
        }

        mask = all_ones >> ( 31 - b2 );

        for ( w = w2; w < lw; w += dw ) SOURCE [ w ] |= mask;
    }
}
subroutine bfill_ ( P, M1, M2, SOURCE, N, R )
/*===========================================*/

F_integer *P;
F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
    if ( *P ) bset_ ( M1, M2, SOURCE, N, R );
    else      bclear_ ( M1, M2, SOURCE, N, R );
}

subroutine brep_ ( M, M1, M2, SOURCE, N, R )
/*==========================================*/

F_integer *M;
F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
    int i, j, m, m1, m2, n, r, dw, ww, bb, i1, i2, w1, w2, b1, b2;
    F_bit mask, mask1, mask2;
    F_bit_array src, dest;

    if ( *M < 1 || *M > *N || *M1 > *N || *M2 < 1 || *M1 > *M2 || *R < 1 ) return;

    m  = *M;
    m1 = *M1;
    m2 = *M2;
    n  = *N;
    r  = *R;

    ww = ( m - 1 ) >> 5;
    bb = ( m - 1 ) & 31;

    dw = ( ( n - 1 ) >> 5 ) + 1;

    i1 = ( m1 > 1 ? m1 : 1 ) - 1;
    i2 = ( m2 < n ? m2 : n ) - 1;

    w1 = i1 >> 5;
    w2 = i2 >> 5;

    b1 = i1 & 31;
    b2 = i2 & 31;

    mask1 = all_ones << b1;
    mask2 = all_ones >> ( 31 - b2 );

    if ( w1 == w2 )
    {
        mask = mask1 & mask2;

	src  = SOURCE + ww;
	dest = SOURCE + w1;

        for ( j = r; j > 0; j--, dest += dw, src += dw )
        {
            if ( ( *src >> bb ) & one )
            {
                *dest |= mask;
            }

            else
            {
                *dest &= ~ mask;
            }
        }
    }

    else
    {
        n = ( w2 - 1 ) - ( w1 + 1 ) + 1;

	src = SOURCE + ww;

        for ( j = r; j > 0; j--, w1 += dw, src += dw )
        {
	    dest = SOURCE + w1;

            if ( ( *src >> bb ) & one )
            {
                *dest++ |= mask1;

                for ( i = n; i > 0; i-- ) *dest++ = all_ones;

                *dest |= mask2;
            }

            else
            {
                *dest++ &= ~ mask1;

                for ( i = n; i > 0; i-- ) *dest++ = all_zeros;

                *dest &= ~ mask2;
            }
        }
    }
}

logicalfunction bdiff_ ( M1, M2, SOURCE1, SOURCE2, N, R )
/*=======================================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE1;
F_bit_array SOURCE2;
F_integer *N;
F_integer *R;
{
    int m1, m2, n, r, w, dw, fw, lw, i, i1, i2, w1, w2, b1, b2;
    F_bit mask;
    F_bit_array src1, src2;

    if ( *M1 > *N || *M2 < 1 || *M1 > *M2 || *R < 1 ) return ( F_FALSE );

    m1 = *M1;
    m2 = *M2;
    n  = *N;
    r  = *R;

    dw = ( ( n - 1 ) >> 5 ) + 1;
    lw = r * dw;

    i1 = ( m1 > 1 ? m1 : 1 ) - 1;
    i2 = ( m2 < n ? m2 : n ) - 1;

    w1 = i1 >> 5;
    w2 = i2 >> 5;

    b1 = i1 & 31;
    b2 = i2 & 31;

    if ( w1 == w2 )
    {
        mask = ( all_ones << b1 ) & ( all_ones >> ( 31 - b2 ) );

        for ( w = w1; w < lw; w += dw )
        {
            if (( SOURCE1 [ w ] ^ SOURCE2 [ w ] ) & mask) return ( F_TRUE );
        }
    }

    else
    {
        mask = all_ones << b1;

        for ( w = w1; w < lw; w += dw )
        {
            if (( SOURCE1 [ w ] ^ SOURCE2 [ w ] ) & mask) return ( F_TRUE );
        }

        n = ( w2 - 1 ) - ( w1 + 1 ) + 1;

	for ( fw = w1 + 1; fw < lw; fw += dw )
	{
	    src1 = SOURCE1 + fw;
	    src2 = SOURCE2 + fw;

	    for ( i = n; i > 0; i-- )
            {
                if ( *src1++ != *src2++ ) return ( F_TRUE );
            }
	}

        mask = all_ones >> ( 31 - b2 );

        for ( w = w2; w < lw; w += dw )
        {
            if (( SOURCE1 [ w ] ^ SOURCE2 [ w ] ) & mask) return ( F_TRUE );
        }
    }

    return ( F_FALSE );
}

subroutine bscan_ ( M1, M2, SOURCE, N, R, BMIN, BMAX )
/*====================================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
F_integer *BMIN;
F_integer *BMAX;
{
    int m1, m2, n, r, w, dw, fw, lw, i, i1, i2, w1, w2, b1, b2;
    F_bit mask, test0, test1;
    F_bit_array src;

    *BMIN = 1;
    *BMAX = 0;

    if ( *M1 > *N || *M2 < 1 || *M1 > *M2 || *R < 1 ) return;

    m1 = *M1;
    m2 = *M2;
    n  = *N;
    r  = *R;

    dw = ( ( n - 1 ) >> 5 ) + 1;
    lw = r * dw;

    i1 = ( m1 > 1 ? m1 : 1 ) - 1;
    i2 = ( m2 < n ? m2 : n ) - 1;

    w1 = i1 >> 5;
    w2 = i2 >> 5;

    b1 = i1 & 31;
    b2 = i2 & 31;

    test0 = all_zeros;
    test1 = all_zeros;

    *BMIN = 0;
    *BMAX = 1;

    if ( w1 == w2 )
    {
        mask = ( all_ones << b1 ) & ( all_ones >> ( 31 - b2 ) );

        for ( w = w1; w < lw; w += dw )
        {
            test0 |= ~ SOURCE [ w ] & mask;
            test1 |=   SOURCE [ w ] & mask;
	    if (test0 && test1) return;
        }
    }

    else
    {
        mask = all_ones << b1;

        for ( w = w1; w < lw; w += dw )
        {
            test0 |= ~ SOURCE [ w ] & mask;
            test1 |=   SOURCE [ w ] & mask;
	    if (test0 && test1) return;
        }

        n = ( w2 - 1 ) - ( w1 + 1 ) + 1;

	for ( fw = w1 + 1; fw < lw; fw += dw )
	{
	    src = SOURCE + fw;

	    for ( i = n; i > 0; i-- )
            {
                test0 |= ~ *src;
                test1 |=   *src++;
	        if (test0 && test1) return;
            }
	}

        mask = all_ones >> ( 31 - b2 );

        for ( w = w2; w < lw; w += dw )
        {
            test0 |= ~ SOURCE [ w ] & mask;
            test1 |=   SOURCE [ w ] & mask;
	    if (test0 && test1) return;
        }
    }

    if ( test0 ) *BMAX = 0;
    if ( test1 ) *BMIN = 1;
}

subroutine bcount_ ( M1, M2, SOURCE, N, R, NBITS )
/*================================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
F_longinteger *NBITS;
{
    int nbits = 0;
    int m1, m2, n, r, w, dw, fw, lw, i, i1, i2, w1, w2, b1, b2;
    F_bit mask, word;
    F_bit_array src;

    static int count[256] = {
                              0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
                              1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
                              1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
                              2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
                              1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
                              2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
                              2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
                              3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
                              1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
                              2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
                              2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
                              3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
                              2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
                              3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
                              3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
                              4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
                            };

    *NBITS = -1;

    if ( *M1 > *N || *M2 < 1 || *M1 > *M2 || *R < 1 ) return;

    m1 = *M1;
    m2 = *M2;
    n  = *N;
    r  = *R;

    dw = ( ( n - 1 ) >> 5 ) + 1;
    lw = r * dw;

    i1 = ( m1 > 1 ? m1 : 1 ) - 1;
    i2 = ( m2 < n ? m2 : n ) - 1;

    w1 = i1 >> 5;
    w2 = i2 >> 5;

    b1 = i1 & 31;
    b2 = i2 & 31;

    if ( w1 == w2 )
    {
        mask = ( all_ones << b1 ) & ( all_ones >> ( 31 - b2 ) );

        for ( w = w1; w < lw; w += dw )
        {
            word = SOURCE [ w ] & mask;

            nbits += count [   word         & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
        }
    }

    else
    {
        mask = all_ones << b1;

        for ( w = w1; w < lw; w += dw )
        {
            word = SOURCE [ w ] & mask;

            nbits += count [   word         & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
        }

        n = ( w2 - 1 ) - ( w1 + 1 ) + 1;

	for ( fw = w1 + 1; fw < lw; fw += dw )
	{
	    src = SOURCE + fw;

	    for ( i = n; i > 0; i-- )
            {
                word = *src++;

                nbits += count [   word         & 255 ];
                nbits += count [ ( word >>= 8 ) & 255 ];
                nbits += count [ ( word >>= 8 ) & 255 ];
                nbits += count [ ( word >>= 8 ) & 255 ];
            }
	}

        mask = all_ones >> ( 31 - b2 );

        for ( w = w2; w < lw; w += dw )
        {
            word = SOURCE [ w ] & mask;

            nbits += count [   word         & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
            nbits += count [ ( word >>= 8 ) & 255 ];
        }
    }

    *NBITS = nbits;
}

integerfunction bvalue_ ( I, J, SOURCE, N, R )
/*============================================*/

F_integer *I;
F_integer *J;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
    int i, j, n, r, w, b;

    if ( *I < 1 || *I > *N || *J < 1 || *J > *R ) return ( 0 );

    i = *I;
    j = *J;
    n = *N;
    r = *R;

    w = ( ( i - 1 ) >> 5 ) + ( j - 1 ) * ( ( ( n - 1 ) >> 5 ) + 1 );
    b = ( i - 1 ) & 31;

    return ( ( SOURCE [ w ] >> b ) & one );
}

subroutine blogic_ ( IOP, SOURCE, DEST, N, R )
/*============================================*/

F_integer *IOP;
F_bit_array SOURCE;
F_bit_array DEST;
F_integer *N;
F_integer *R;
{
    int iop, n, r, nw, i;
    F_bit_array src, dest;

    if ( *N < 1 ) return;

    iop  = *IOP;
    src  = SOURCE;
    dest = DEST;
    n    = *N;
    r    = *R;

    nw = r * ( ( ( n - 1 ) >> 5 ) + 1 );

    switch ( iop )
    {
        case  0:   /*  dest = all zeros  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ = all_zeros;
        }
        break;

        case  1:   /*  dest = and(not(source),not(dest))  */
        {
            for ( i = nw; i > 0; i--, dest++ ) *dest = ~ *src++ & ~ *dest;
        }
        break;

        case  2:   /*  dest = and(not(source),dest)  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ &= ~ *src++;
        }
        break;

        case  3:   /*  dest = not(source)  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ = ~ *src++;
        }
        break;

        case  4:   /*  dest = and(source,not(dest))  */
        {
            for ( i = nw; i > 0; i--, dest++ ) *dest = *src++ & ~ *dest;
        }
        break;

        case  5:   /*  dest = not(dest)  */
        {
            for ( i = nw; i > 0; i--, dest++ ) *dest = ~ *dest;
        }
        break;

        case  6:   /*  dest = xor(source,dest) = xor(not(source),not(dest))  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ ^= *src++;
        }
        break;

        case  7:   /*  dest = or(not(source),not(dest))  */
        {
            for ( i = nw; i > 0; i--, dest++ ) *dest = ~ *src++ | ~ *dest;
        }
        break;

        case  8:   /*  dest = and(source,dest)  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ &= *src++;
        }
        break;

        case  9:   /*  dest = xor(source,not(dest)) = xor(not(source),dest)  */
        {
            for ( i = nw; i > 0; i--, dest++ ) *dest = ~ *src++ ^ *dest;
        }
        break;

        case 10:   /*  dest = dest  (i.e. do nothing!)  */
        {
        /*  for ( i = nw; i > 0; i--, dest++ ) *dest = *dest;  */
        }
        break;

        case 11:   /*  dest = or(not(source),dest)  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ |= ~ *src++;
        }
        break;

        case 12:   /*  dest = source  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ = *src++;
        }
        break;

        case 13:   /*  dest = or(source,not(dest))  */
        {
            for ( i = nw; i > 0; i--, dest++ ) *dest = *src++ | ~ *dest;
        }
        break;

        case 14:   /*  dest = or(source,dest)  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ |= *src++;
        }
        break;

        case 15:   /*  dest = all ones  */
        {
            for ( i = nw; i > 0; i-- ) *dest++ = all_ones;
        }
    }
}

subroutine bshift_ ( M, SOURCE, DEST, N, R )
/*==========================================*/

F_integer *M;
F_bit_array SOURCE;
F_bit_array DEST;
F_integer *N;
F_integer *R;
{
    int i, m, n, r, lw, w1, b1;
    F_bit_array src, dest;

    if ( *N < 1 || *R < 1 ) return;

    m = *M;
    n = *N;
    r = *R;

    lw = r * ( ( ( n - 1 ) >> 5 ) + 1 ) - 1;

    src  = SOURCE;
    dest = DEST;

    if ( m < 0 )
    {
        if ( m > -n )
        {
            w1 = -m >> 5;
            b1 = -m & 31;

            src += w1;

            if ( b1 )
            {
                for ( i = lw - w1; i > 0; i--, src++ )
                {
                    *dest++ = ( src [ 0 ] >> b1 ) |
                              ( src [ 1 ] << ( 32 - b1 ) );
                }

                *dest = *src >> b1;
            }

            else
            {
                for ( i = lw - w1; i >= 0; i-- ) *dest++ = *src++;
            }
        }
    }

    else if ( m > 0 )
    {
        if ( m < n )
        {
            w1 = m >> 5;
            b1 = m & 31;

            src  += lw - w1;
            dest += lw;

            if ( b1 )
            {
                for ( i = lw - w1; i > 0; i--, src-- )
                {
                    *dest-- = ( src [ -1 ] >> (32 - b1) ) |
                              ( src [  0 ] << b1 );
                }

                *dest = *src << b1;
            }

            else
            {
                for ( i = lw - w1; i >= 0; i-- ) *dest-- = *src--;
            }
        }
    }

    else
    {
        for ( i = lw; i >= 0; i-- ) *dest++ = *src++;
    }
}

subroutine bform_ ( IOP, BITS, DATA, FORM, N )
/*============================================*/

F_integer *IOP;
F_bit_array BITS;
F_array DATA;
F_integer *FORM;
F_integer *N;
{
    F_bit_array     bits;
    F_byte_array    byte;
    F_integer_array integer;
    F_real_array    real;
    F_complex_array complex;

    int iop, form, n, w, b, i, j;
    F_bit word, mask;

    if ( *N < 1 ) return;

    iop  = *IOP;
    form = *FORM;
    n    = *N;

    w = ( n - 1 ) >> 5;
    b = ( n - 1 ) & 31;

    switch ( iop )
    {
        case 1:
        {
            bits = BITS;
        }
        break;

        case 2:
        {
            bits = BITS + w;
        }
    }

    switch ( form )
    {
        case NFMBYT:
        {
            switch ( iop )
            {
                case 1:
                {
                    byte = (F_byte_array) DATA;

                    for ( j = w; j > 0; j-- )
                    {
                        word = all_zeros;
                        mask = one;

                        for ( i = 31; i >= 0; i--, mask <<= 1 )
                        {
                            if ( *byte++ ) word |= mask;
                        }

                        *bits++ = word;
                    }

                    word = all_zeros;
                    mask = one;

                    for ( i = b; i >= 0; i--, mask <<= 1 )
                    {
                        if ( *byte++ ) word |= mask;
                    }

                    *bits = word;
                }
                break;

                case 2:
                {
                    byte = (F_byte_array) DATA + n;

                    word = *bits--;
                    mask = one << b;

                    for ( i = b; i >= 0; i--, mask >>= 1 )
                    {
                        if ( word & mask ) *--byte = 1;
                        else               *--byte = 0;
                    }

                    for ( j = w; j > 0; j-- )
                    {
                        word = *bits--;
                        mask = one << 31;

                        for ( i = 31; i >= 0; i--, mask >>= 1 )
                        {
                            if ( word & mask ) *--byte = 1;
                            else               *--byte = 0;
                        }
                    }
                }
            }
        }
        break;

        case NFMINT:
        {
            switch ( iop )
            {
                case 1:
                {
                    integer = (F_integer_array) DATA;

                    for ( j = w; j > 0; j-- )
                    {
                        word = all_zeros;
                        mask = one;

                        for ( i = 31; i >= 0; i--, mask <<= 1 )
                        {
                            if ( *integer++ ) word |= mask;
                        }

                        *bits++ = word;
                    }

                    word = all_zeros;
                    mask = one;

                    for ( i = b; i >= 0; i--, mask <<= 1 )
                    {
                        if ( *integer++ ) word |= mask;
                    }

                    *bits = word;
                }
                break;

                case 2:
                {
                    integer = (F_integer_array) DATA + n;

                    word = *bits--;
                    mask = one << b;

                    for ( i = b; i >= 0; i--, mask >>= 1 )
                    {
                        if ( word & mask ) *--integer = 1;
                        else               *--integer = 0;
                    }

                    for ( j = w; j > 0; j-- )
                    {
                        word = *bits--;
                        mask = one << 31;

                        for ( i = 31; i >= 0; i--, mask >>= 1 )
                        {
                            if ( word & mask ) *--integer = 1;
                            else               *--integer = 0;
                        }
                    }
                }
            }
        }
        break;

        case NFMFP:
        {
            switch ( iop )
            {
                case 1:
                {
                    real = (F_real_array) DATA;

                    for ( j = w; j > 0; j-- )
                    {
                        word = all_zeros;
                        mask = one;

                        for ( i = 31; i >= 0; i--, mask <<= 1 )
                        {
                            if ( *real++ ) word |= mask;
                        }

                        *bits++ = word;
                    }

                    word = all_zeros;
                    mask = one;

                    for ( i = b; i >= 0; i--, mask <<= 1 )
                    {
                        if ( *real++ ) word |= mask;
                    }

                    *bits = word;
                }
                break;

                case 2:
                {
                    real = (F_real_array) DATA + n;

                    word = *bits--;
                    mask = one << b;

                    for ( i = b; i >= 0; i--, mask >>= 1 )
                    {
                        if ( word & mask ) *--real = 1.0;
                        else               *--real = 0.0;
                    }

                    for ( j = w; j > 0; j-- )
                    {
                        word = *bits--;
                        mask = one << 31;

                        for ( i = 31; i >= 0; i--, mask >>= 1 )
                        {
                            if ( word & mask ) *--real = 1.0;
                            else               *--real = 0.0;
                        }
                    }
                }
            }
        }
        break;

        case NFMCOM:
        {
            switch ( iop )
            {
                case 1:
                {
                    complex = (F_complex_array) DATA;

                    for ( j = w; j > 0; j-- )
                    {
                        word = all_zeros;
                        mask = one;

                        for ( i = 31; i >= 0; i--, mask <<= 1 )
                        {
                            if ( (*complex++).re ) word |= mask;

                            real++;
                        }

                        *bits++ = word;
                    }

                    word = all_zeros;
                    mask = one;

                    for ( i = b; i >= 0; i--, mask <<= 1 )
                    {
                        if ( (*complex++).re ) word |= mask;

                        real++;
                    }

                    *bits = word;
                }
                break;

                case 2:
                {
                    complex = (F_complex_array) DATA + n;

                    word = *bits--;
                    mask = one << b;

                    for ( i = b; i >= 0; i--, mask >>= 1 )
                    {
                        if ( word & mask ) (*--complex).re = 1.0;
                        else               (*--complex).re = 0.0;

                        (*complex).im = 0.0;
                    }

                    for ( j = w; j > 0; j-- )
                    {
                        word = *bits--;
                        mask = one << 31;

                        for ( i = 31; i >= 0; i--, mask >>= 1 )
                        {
                            if ( word & mask ) (*--complex).re = 1.0;
                            else               (*--complex).re = 0.0;

                            (*complex).im = 0.0;
                        }
                    }
                }
            }
        }
    }
}
subroutine bclear ( M1, M2, SOURCE, N, R )
/*=========================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
         bclear_ ( M1, M2, SOURCE, N, R ) ;
}
subroutine bset ( M1, M2, SOURCE, N, R )
/*=======================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
        bset_ ( M1, M2, SOURCE, N, R );
}
subroutine bfill ( P, M1, M2, SOURCE, N, R )
/*===========================================*/

F_integer *P;
F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
        bfill_ ( P, M1, M2, SOURCE, N, R ) ;
}
subroutine brep ( M, M1, M2, SOURCE, N, R )
/*==========================================*/

F_integer *M;
F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
        brep_ ( M, M1, M2, SOURCE, N, R );
}
logicalfunction bdiff ( M1, M2, SOURCE1, SOURCE2, N, R )
/*=======================================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE1;
F_bit_array SOURCE2;
F_integer *N;
F_integer *R;
{
        return bdiff_ ( M1, M2, SOURCE1, SOURCE2, N, R );
}


subroutine bscan ( M1, M2, SOURCE, N, R, BMIN, BMAX )
/*====================================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
F_integer *BMIN;
F_integer *BMAX;
{
        bscan_ ( M1, M2, SOURCE, N, R, BMIN, BMAX );
}
subroutine bcount ( M1, M2, SOURCE, N, R, NBITS )
/*================================================*/

F_integer *M1;
F_integer *M2;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
F_longinteger *NBITS;
{
        bcount_ ( M1, M2, SOURCE, N, R, NBITS );
}
integerfunction bvalue ( I, J, SOURCE, N, R )
/*============================================*/

F_integer *I;
F_integer *J;
F_bit_array SOURCE;
F_integer *N;
F_integer *R;
{
        return bvalue_ ( I, J, SOURCE, N, R );
}
subroutine blogic ( IOP, SOURCE, DEST, N, R )
/*============================================*/

F_integer *IOP;
F_bit_array SOURCE;
F_bit_array DEST;
F_integer *N;
F_integer *R;
{
        blogic_ ( IOP, SOURCE, DEST, N, R );
}

subroutine bshift ( M, SOURCE, DEST, N, R )
/*==========================================*/

F_integer *M;
F_bit_array SOURCE;
F_bit_array DEST;
F_integer *N;
F_integer *R;
{
        bshift_ ( M, SOURCE, DEST, N, R );
}
subroutine bform ( IOP, BITS, DATA, FORM, N )
/*============================================*/

F_integer *IOP;
F_bit_array BITS;
F_array DATA;
F_integer *FORM;
F_integer *N;
{
        bform_ ( IOP, BITS, DATA, FORM, N );
}


