$add intgrad gradient edges vectors
INTGRAD is used for determining the intensity gradient vectors according to
the formulation defined by TOVEY and SMART. This verb is a more flexible
version of the PS2 verb which runs slightly faster for the same data than
INTGRAD. INTGRAD can be used to select the configuration of the angles
histogram, breaking the histograms into vector magnitude divisions, or the
tangent and square of the angles and magnitude can be output if required.

Exx:
     INTGRAD 2 102 MAG 103 HIS 104 SECTOR 2 FTYPE 4,2
        The angles picture is produced in 102, magnitude picture in 103 and
        angles histogram in 104. The histogram is only of the aggregate (> or
        = CUTOFF), with an angular division of 2 degrees. The formula used in
        the analysis is the 4,2
     INT TO 102 HIS 104 CUT 5 DIV 8 SUM BELOW
        The  vector angle is output to picture 102.
        The angles histogram in picture 104 has 8 magnitude divisions + one
        for the vectors with magnitudes below the CUTOFF value of 5. One of
        these is the aggregate. The histograms contain the summed magnitudes,
        rather than the equal vector weighting assumed by default.
$
This is principally of use for quantifying the orientation of edges and other
intensity related textures in images, but it can also be used as an edge
detector much like the EDGE verb, where the magnitude output of INTGRAD
(or PS2) is comparible to the output of the EDGE or DIFFERENTIATE verbs. The
difference is that the INTGRAD verb you can select the number of points over
which the intensity gradient is determined, so leading to degree of smoothing
and averaging over a noisy image.

The selection of the formula type, FTYPE allows the use of either 3x3 or 5x5
arrays for determination of the intensity gradient (See Smart, P. & Tovey,
N.K., Theoretical aspects of intensity gradient analysis, Scanning
vol 10: 115-121). The formulas allowed are:
2,2;  3,2;  4,2; (8,2; 8,5); (12,2; 12,5); 12,9; (20,2; 20,5); (20,9; 20,14);
 (24,2; 24,5) or (24,9; 24,14), where those in brackets indicate the
formulation is identical. The first figure refers to the number of points used
in the calculation, and the second the accuracy of the formula approximation.
The 2,2 and 3,2 can be thought of as similar formulations to those used in the
verb EDGE.

The class of the picture of the histogram depends on the number of magnitude
divisions wanted, the default DIVISION of 1 produces a class histogram
picture, with the aggregated angular vectors. If DIVISION is greater than 1
or option BELOW is set a class plist picture is produced. The first row of
this plist contains the magnitude threshold setting for dividing the vector
magnitudes, the 2nd row the aggregate values and subsequent rows the intensity
divisions, in ascending order. If option BELOW is set the 3rd row is that of
vectors with magnitudes < or = CUTOFF. The verb ISTAT will interpret these
pictures, producing statistics or extracting particular intensity divisions as
class histogram pictures.

The angles are specified with reference to the +y axis (ie. up is 0 degrees).

The output pictures have the titles marked at position 100 onwards with the
type of data they contain:

Angles picture (default) :                      \ANG\
Angles picture with all angles stored:          \ANG\STORE\
A tangent angles picture:                       \TAN\
Magnitude picture (default):                    \MAG\
The square of the magnitude:                    \SQR\
An angles histogram (default):                  \HIS\
An angles histogram of the summed magnitudes:   \HIS\SUM\

The form used internally is integer for byte type images and fp for integer
or fp input pictures, unless overriden by the general options fp or integer.
The verb will not process the two row/cols of pixels near to the picture
border, because of the 5x5 arrays used in the analysis.

A sub-region of the picture can be analysed with the use of the nomal 2-d
subregion keys/options. Remembering that pixels within 2 pixel widths of the
border cannot have a value with this verb. Subregions away from the border
regions will have output pictures corresponding to the full size specified
with the sub-region keys/options.

The semper variables UAA, UAM and UAH are set indicating the angles, magnitude
and histogram pictures, which may be later used by processing commands.

                                        Restrictions: Pictures smaller than
                                                      10x10 are faulted
                                        Multi-layer pictures:  faulted
                                        Form used internally:  integer,fp

See also: Intgrad.syntax Istats ps2

$add domain top-contour von-mises rayleigh
DOMAIN maps patches of consistent directional (or intensity) features in an
image. It is assumed the input image is an 'angles' image produced from
either INTGRAD or PS2. There are two algorithms available, either top-
contouring, or a vector method involving a statistical test of the mean vector
(the Rayleigh test, Mardia; 1972). The masking region over which the algorithm
works can be either square or circular (default), and up to a width of 79
pixels.

Exx:  DOM 10 TO 900 RAD 11 CODED BORDER
       Top-contours coded image 10 with a circular mask of radius 11 placing
       a blank border around the image
      DOM 10 900 RAY SEGMENT 9 SQUARE
       Domain maps using the Rayleigh algorithm, with a radius of 7 with
       a square mask, producing a coded output picture with 0-9 grey levels
      DOM 10 COUT 901 segment 18
       Codes the picture 10, outputing result to 901, with 901 consisting of
       19 gray levels. The coding is performed by scaling the input
       grey levels 0-180 to 1 to 18, with 255 set to zero
$
The source image can be either a coded image (in which case the output image
has the same range as the input, and option CODED should be set) or an angles
image produced from INTGRAD or PS2. The range of the output picture can be
set by the key SEGMENT, so a value of 4 corresponds to an angular segment of
45 degrees (18 is a segment of 10 degrees). The value of segment is ignored
if the input image is coded.

The default processing algorithm is top contouring. With the option RAYLEIGH
the processing algorithm calculates the vector mean in the mask, and performs
the Rayleigh test of randomness (at 95% significance level).[see Mardia, 1972.
Statistics of directional data). If the mean resultant is not significantly
different from random the pixel is assigned a value of 0, otherwise it is
set to the coded value of the mean direction. Therefore with the Rayleigh
algorithm there is no undecided class

The coding in the output picture is such that a pixel value of 1 corresponds
to an angular segment CENTRED on zero degrees (upwards directed). Pixel values
2 upwards correspond with consequetive segments clockwise. A pixel value of 0
indicates an non-significant angle or not sufficient data. With the top
contouring algorithm an undecided class (seg+1, or 5 for default value of 4)
is also present.

The RADIUS key can have a value of 3 to 39, with only odd values in this
range, to preserve the symmetry of the mask. The mask diameter used is
RADIUS*2+1

Only byte and integer form pictures can be processed by this verb, and the
maximum allowed picture size is 512x512. The output picture size is
RADIUS*2 smaller in both dimensions than the input picture

                                      Multilayer-pictures :faulted

See also DOMAIN.SYNTAX



$end
$add istatistics stat
The purpose of ISTAT is to calculate statistical parameters for the histograms
generated by the verbs INTGRAD or PS2. This can use the class histogram or
plist pictures generated by these verbs. The statistics calculated are based
on i) the best-fitting ellipse and ii) the Von-Mises angular statistics
outlined in Mardia(1972). See ISTAT.PAR for a full list and explanation of the
parameters calculated. ISTAT will also produce class histogram pictures from
selected intensity classes stored in a class plist picture produced by
INTGRAD.

Exx:    ISTAT 20 TYPE
           Will produce a list of the intensity classes and number of points/
           magnitude stored in picture 20.
        ISTAT ALL DIVISION 1,4 HISTOGRAM 900
           Will produce a listing to the screen of all the statistics
           determined for intensity divisons 1 to 4, including the statistics
           for the aggregate class. Class histogram pictures will also be
           generated in pictures 900 to 904. Uses picture UAH as input.
        ISTAT TO 20 NOSTAT
           Produces a plist output file 20, containing all the statistical
           parameters calculated for all intensity divisions stored in 20.
           No statistics are printed to the screen.
$
By default the input picture is picked up from variable UAH which is set by
PS2 and INTGRAD. ISTAT with no key or option set will print selected
statistics to the screen of the aggregate intensity class only. The key
DIVISION indicates which OTHER intensity class statistics are also produced,
where the valid intensity divisions are those indicated by using the TYPE
option. Obviously this is only applicable if the input is a plist produced by
INTGRAD. A covenient way to use the verb is to use TYPE first to examine the
classes, and then use the other key/options to examine the statistics of the
intensity classes.

The number of class histogram pictures produced by HISTOGRAM n, are also
determined by the DIVISION key and AGGREGATE option. These go into pictures n,
n+1, n+2 etc. according to the number of intensity divisions pointed at by
the DIVISION and AGRREGATE key/option.

Specifying an output picture with the TO key will produce a class plist
containing all the statistics of all the intensity statistics stored in the
input picture. The number of layers in this plist will correspond to the
size of the number of rows contained in the input picture. The full list of
the parameters in the plist file is explained in ISTAT.PAR. And the format of
the class plist picture produced is explained in ISTAT.IPLIST

See also: istat.syntax istat.par istat.iplist intgrad ps2
$end
$add area percentage porosity proportion
AREA determines the percentage of pixels in a grey level range, from a
picture or subregion of a picture, printing the result to the screen
and returning the percentage in variable T.

Exx:  AREA 2 GREY 20
       determines the percentage of grey level 20 in picture 2
      AREA 100 GREY 200,255 NOTYPE TOTAL SIZE 40
       Determines the total number of pixels in a subregion of the picture
       40 square centered at origin, which have grey level between (>=200)
       200 and 255 (<=255). The result is not typed on the screen but only
       returned in variable T.

The full set of subregion/position keys/options are accepted

                                         Restrictions:  Only byte and integer
                                                        pictures supported
                                 Multi-layer pictures: supported

$end
