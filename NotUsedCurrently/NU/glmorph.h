$add glmorph.syntax
Keys:
   [FROM]         Source  picture number [SELECT]
   TO             Destination picture [FROM]
   RADIUS [7] -   Radius of circle/square/octagon/diamond. Total width of 
                  mask is RAD*2+1. Only accepts a radius upto MAXLIN/2-2,
                  which is hardware dependant.  For DOS systems using
                  SYNAPSE and SYNERGY and VGA the maximum value of RADIUS is 29. 

Options:
 LMEAN          - Generates an output image which is the mean of input over
                  the mask radius.  Equivalent to LMEAN but better
 [CIRCLE]       - Mask is true Euclidean circle of radius RADIUS
 SQUARE         - Mask used is square with edge length RAD*2+1 
 OCTAGON        - Mask used is octagonal of effective radius RADIUS
 DIAMOND        - Mask used is a diamond with maximum dimension - 2*RAD +1
 BORDER         - makes o/p pic same size as input picture and fills
                  boundary with blanks.  The default has output picture
                  ncols(lp1)-2*rad in width x nrows(lp1)-2*rad deep
 [BLANK]        - default if BORDER option used
 REFLECT        - fills boundary with algorithm based on image reflection at
                  boundary
 [ERODE]        - does a grey level erosion
 DILATE         - does a grey level dilation
 OPEN           - does a grey level opening - 2 passes through algorithm
 CLOSE          _ does a grey level closing - 2 passes through algorithm
$end
$add glmorph morphology glm
GLM (Grey Level Morphology) is used for grey level morphological operations 
such as ERODE, DILATE, OPEN, CLOSE on full grey level images. The basic
algorithm is described in Prod'homme et al. (1992)  Morphological Filtering 
and Glanulometric Analysis on Scanning Electron Micrographs.  Scanning 
Microscopy, Supplement 6 (1992) p 255 - 268.

Exx:
     GLM 4:1 to 4:8 REFLECT 
        This does a default grey level [ERODE] on image 4:1 and stores the
        result in 4:8.  Image reflection is used to fill boundary region.
        The mask is [7] pixels radius (default) and cicular.  
     GLM 4:1 to 4:2 CLOSE BORDER RADIUS 11 OCTAGON
        The performs a greylevel CLOSE on image 4:1 with an octagonal
        mask of radius 11.  Boundary values are not computed but output image
        is surrounded by a blank border to make output image same size as input.
     GLM 5:3 to 5:201 RADIUS 1 DIAMOND DILATE
        This performs a grey level dilation on image 5:3 with a diamond shaped
        mask of radius 1.  The output image will be 2*RADIUS smaller than input.

SEE also HELP GLM.SYNTAX. 
$
The original algorithm of Prod'Homme et al. only allowed for SQUARE masks.  
This algorithm also allows [CIRCULAR], DIAMOND, and OCTAGONAL masks.  Three
options are available to deal with boundary pixels.  1) output image is smaller
by 2*radius by default; 2) boundary is filled with blanks; 3) boundary pixels 
are computed using image reflection at boundaries.

An option LMEAN allows a local mean to be computed using the mask shape 
defined.  The algorithm works by replacing the value at the centre of the
mask in the output image by the [MINIMUM (in ERODE)] or MAXIMUM (for DILATE)
pixel value contained within the defined mask area of the input image.  

OPENing (or CLOSEing) involves two passes (ERODE followed by DILATE) or
(DILATE followed by ERODE) respectively.

Currently the maximum radius is 29 pixels and is a limitation of PC 
implementations of SEMPER.
$end
