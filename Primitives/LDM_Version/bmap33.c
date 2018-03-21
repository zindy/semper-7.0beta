#include "ftypes.h"

#define all_zeros (F_bit) 0;
#define one       (F_bit) 1;


subroutine bmap33_ ( NH1, NH2, NH3, CHANGE, MAP, N, R )
/*=====================================================*/

F_bit_array NH1;
F_bit_array NH2;
F_bit_array NH3;
F_bit_array CHANGE;
F_integer_array MAP;
F_integer *N;
F_integer *R;
{
    int i, j, n, r;
    F_bit c, n1, n2, n3, nhood, bit;
    F_bit_array nh1, nh2, nh3, change;

    nh1    = NH1;
    nh2    = NH2;
    nh3    = NH3;
    change = CHANGE;
    n      = *N;
    r      = *R;

    nhood = all_zeros;

    for ( i = r * ( ( ( n - 1 ) >> 5 ) + 1 ); i > 0; i-- )
    {
        n1 = *nh1++;
        n2 = *nh2++;
        n3 = *nh3++;

        c = all_zeros;
        bit = one;

        for ( j = 31; j >= 0; j-- )
        {
            nhood >>= 3;

            if ( n1 & bit ) nhood |= 64;
            if ( n2 & bit ) nhood |= 128;
            if ( n3 & bit ) nhood |= 256;

            if ( MAP[ nhood ] ) c |= bit;

            bit <<= 1;
        }

        *change++ = c;
    }
}
subroutine bmap33 ( NH1, NH2, NH3, CHANGE, MAP, N, R )
/*=====================================================*/

F_bit_array NH1;
F_bit_array NH2;
F_bit_array NH3;
F_bit_array CHANGE;
F_integer_array MAP;
F_integer *N;
F_integer *R;
{
        bmap33_ ( NH1, NH2, NH3, CHANGE, MAP, N, R );
}
