#line 1 "crdwrt.f"
/* crdwrt.f -- translated by f2c (version 19991025).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

#line 1 "crdwrt.f"
/* Common Block Declarations */

struct {
    real rbs[121980]	/* was [8132][15] */;
} sembuf_;

#define sembuf_1 sembuf_

struct {
    real rovvs[4], batch, displa, select, cframe, clut, rc, uif, trap, vmin, 
	    vmax, vmean, vme2, vsd, cd, fs, dollar, vvrest[980], locval[300];
    integer vnames[1000], nsemve, loclev, locvar[300];
    logical locuns[300];
} semtab_;

#define semtab_1 semtab_

struct {
    real gsmin[50], gsmax[50];
    integer flsiz[50], nextb[50], addr__[50], currb[50], medn[50], protn[50], 
	    dvtyp[50], drsiz[50], tpsid[50], dvscr[50], frsiz[50], frsi2[50], 
	    nfrs[50], monsiz[50], monsi2[50], gpsiz[50], chsiz[50], chsi2[50];
    logical ovlind[50];
    integer dvhan[50], dvwid[50];
    logical filflg[300]	/* was [50][6] */;
    integer currf[50], nextf[50], when[50], devn[50], picn[50], wstat[50], 
	    ncols[50], nrows[50], nlays[50], ccoln[50], crown[50], clayn[50], 
	    classn[50], formn[50], xleft[50], ytop[50], labfg[50], frame1[50],
	     frame2[50], pxsam[50];
} semcbs_;

#define semcbs_1 semcbs_

struct {
    integer wrkdvs, wrkdpd, wrklut, wrklin, wrkind;
    real forend[20], forstp[20];
    integer forcnt[20], forvar[20], fordev[20], forlin[20], foroff[20], 
	    forslt[20], forfsl[20], forlsl[20], forloc[20], forlev;
} semfrl_;

#define semfrl_1 semfrl_

struct {
    integer inpdev[21], inplin[21], inpslt[21], inplen[21], inpnxt[21], 
	    intrap[21], inpfor[21], inploc[21], inplev, input, ptdev, ptnumb, 
	    ptprio[50], dtbuff[7];
} sempri_;

#define sempri_1 sempri_

struct {
    integer lintxt, linbuf[2048], lindex[512], lindev, linslt, linnum, linlen,
	     linptr, comlim, obeyln[2048], obeypt;
    logical obeyme;
} sempli_;

#define sempli_1 sempli_

struct {
    integer i4ider, vrblev, verb, jmpdst, lastsc, nextsc, prevsc, npmodu, lp1,
	     lp2, lp3, error, iderr, iderr2, currwh, basewh, oplpn, bufrow, 
	    buflay, buflpn, buffrm, picsel;
    logical noercn, reqdcf, reqdts, reqfsf, reqdrr, lblinc, selopn, monit, 
	    mchanl[16];
} semcip_;

#define semcip_1 semcip_

struct {
    real smgr1, smgr2, smgr3, smgr4, smgr5, smgr6, dumfp;
    integer smgi1, smgi2, smgi3, smgi4, smgi5, smgi6, smgi7, smgi8, smgi9, 
	    smgi10, dumint;
    logical smgl1, smgl2, smgl3, dumlog;
} semarg_;

#define semarg_1 semarg_

struct {
    integer cbsize, cbnum, ndpds, nluts, lutlen, lutmax, lutsze;
} semcfg_;

#define semcfg_1 semcfg_

struct {
    real dpmin, dpmax, dplef, dprig, dpbot, dptop, dpma, dpmb, dpma2, dpmb2;
    integer dptyp, dpfra, dpfra2, dptlx, dptly, dpsiz, dpsi2, dpimo, dpsrc, 
	    dpdev, dpnum, dplut, dprest[1];
} sempdb_;

#define sempdb_1 sempdb_

struct {
    integer lutmod[100];
} semldb_;

#define semldb_1 semldb_

struct {
    real fsxsca, fsysca, fsxoff, fsyoff, fsxmin, fsxmax, fsymin, fsymax, 
	    fsblef, fsbrig, fsbbot, fsbtop, fsxpan, fsypan, fsioff;
    integer fsdev, fsfra, fsfra2, fslut, fspar, fsptyp, fsi1, fsi2, fsmmod, 
	    fsmsiz;
} semgdb_;

#define semgdb_1 semgdb_

struct {
    char record[200], terstr[68], trprec[200], errrec[200], idmess[78], 
	    worknm[80], ioname[255];
} semiob_;

#define semiob_1 semiob_

struct {
    real terasp;
    integer terwid, terlen, tercnt, terpln, terlbn, terqbn, terpbn;
    logical tercr, terlf, terxcr, terxlf, terpro, terwra, souflg[6], terqui, 
	    lserf, liovld, serflg[6];
} semiod_;

#define semiod_1 semiod_

struct {
    integer kstate, bstate, pstate, kbstep;
    logical levent;
} semeve_;

#define semeve_1 semeve_

struct {
    integer kbufn, kbufp;
} semkb1_;

#define semkb1_1 semkb1_

struct {
    char kbuff[3200];
} semkb2_;

#define semkb2_1 semkb2_

/* Table of constant values */

static integer c__22623 = 22623;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__76 = 76;
static integer c__2 = 2;
static integer c_n601 = -601;

/* Semper 6 processing module CRDWRT */

/* Subroutine */ int crdwrt_()
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3, i__4[2];
    static integer equiv_55[20];

    /* Builtin functions */
    /* Subroutine */ int s_copy(), s_cat();

    /* Local variables */
    integer i__, j, k, fd, nf;
#define nx ((shortint *)equiv_55 + 1)
#define ny ((shortint *)equiv_55 + 2)
#define nz ((shortint *)equiv_55 + 3)
#define rb1 ((real *)&sembuf_1 + 10)
#define rb2 ((real *)&sembuf_1 + 8142)
#define rb3 ((real *)&sembuf_1 + 16274)
#define rb4 ((real *)&sembuf_1 + 24406)
#define rb5 ((real *)&sembuf_1 + 32538)
#define rb6 ((real *)&sembuf_1 + 40670)
#define col ((shortint *)equiv_55 + 27)
#define dum ((shortint *)equiv_55 + 36)
    integer iop;
    extern logical opt_();
#define rb1lhs ((real *)&sembuf_1)
#define rb2lhs ((real *)&sembuf_1 + 8132)
#define rb3lhs ((real *)&sembuf_1 + 16264)
#define rb4lhs ((real *)&sembuf_1 + 24396)
#define rb5lhs ((real *)&sembuf_1 + 32528)
#define rb6lhs ((real *)&sembuf_1 + 40660)
#define rb1rhs ((real *)&sembuf_1 + 8122)
#define rb2rhs ((real *)&sembuf_1 + 16254)
#define rb3rhs ((real *)&sembuf_1 + 24386)
#define rb4rhs ((real *)&sembuf_1 + 32518)
#define rb5rhs ((real *)&sembuf_1 + 40650)
#define rb6rhs ((real *)&sembuf_1 + 48782)
#define echdia ((logical *)&semcbs_1 + 1151)
    extern logical fildcd_(), filmak_(), filsea_();
    extern integer lnblnk_(), ivalpn_(), semfrm_();
    extern logical semopn_(), filstr_(), semrow_();
#define echcon ((logical *)&semcbs_1 + 1150)
#define echlog ((logical *)&semcbs_1 + 1152)
#define echmon ((logical *)&semcbs_1 + 1153)
#define echcom ((logical *)&semcbs_1 + 1154)
#define echinp ((logical *)&semcbs_1 + 1155)
#define soucon ((logical *)&semiod_1 + 14)
#define soudia ((logical *)&semiod_1 + 15)
#define soulog ((logical *)&semiod_1 + 16)
#define soumon ((logical *)&semiod_1 + 17)
#define soucom ((logical *)&semiod_1 + 18)
#define souinp ((logical *)&semiod_1 + 19)
#define sercon ((logical *)&semiod_1 + 23)
#define serdia ((logical *)&semiod_1 + 24)
#define serlog ((logical *)&semiod_1 + 25)
#define sermon ((logical *)&semiod_1 + 26)
#define sercom ((logical *)&semiod_1 + 27)
#define serinp ((logical *)&semiod_1 + 28)
    integer ncol, nrow, nlay;
#define aligns ((logical1 *)equiv_55)
#define header ((logical1 *)equiv_55 + 2)
#define ramp0 ((shortint *)equiv_55 + 4)
#define rmp255 ((shortint *)equiv_55 + 5)
#define isbyte ((shortint *)equiv_55 + 8)
#define filno ((shortint *)equiv_55 + 9)
#define a1name ((shortint *)equiv_55 + 10)
    shortint a1temp[32];
#define ismerg ((shortint *)equiv_55 + 26)
#define fileid ((shortint *)equiv_55 + 28)
#define lamp0 ((shortint *)equiv_55 + 29)
#define lmp255 ((shortint *)equiv_55 + 30)
#define lcol ((shortint *)equiv_55 + 31)
#define isedit ((shortint *)equiv_55 + 32)
#define lens ((shortint *)equiv_55 + 33)
#define magfac (equiv_55 + 17)
#define noter (equiv_55 + 3)
    integer nchars;
    logical lwrite, exists;
    char pathnm[255], file[255], name__[255], prefix[255], exten[4];
    extern logical eikexi_(), eikdel_(), eikope_(), eikbya_(), eikclo_();
    extern /* Subroutine */ int semics_(), cform_(), headsw_(), irowsw_();


/* Provides verbs CGET, CPUT:  Reads/Writes Biorad confocal microscope */
/* images that have originated from a PC based system */



/* Standard COMMON block declarations */


/* RBS contains NNBUF row buffers of length LNBUF bytes, with LNEDGE */
/* extra real elements at both ends of each buffer to allow for edge */
/* processing.  The pre-existing row buffer declarations are maintained */
/* by means of equivalencing. */

/* Standard parameter definitions */

/* ****** CHANGE ****** */

/* Data representation */
/* ------------------- */
/* Lengths in bytes of INTEGER and FP pixel representations; latter */
/* must not be less than former */


/* Length in bytes of INTEGER*4 datatype */


/* Picture storage devices */
/* ----------------------- */
/* Size in bytes of logical disc blocks */
/* - must be a multiple of LNINT */


/* NOBACK indicates that MCTP61 cannot provide backward block and */
/* file skips (IOP=7 and 9) */


/* Maximum display look-up table length */


/* Input/output */
/* ----------------- */
/* Flag numbers for interactive input and output */
/* **** NOT FOR USE AS FORTRAN UNIT NUMBERS **** */


/* Fortran unit numbers for run files and error message file */


/* Fortran unit number for opening data files */
/*   (e.g. in READ/WRITE - see FRDWRT) */


/* Maximum length for fully specified file names */


/* Nesting levels */
/* -------------- */
/* Maximum input depth, FOR loop depth and maximum number of saved */
/* variables; no change needed at initial installation time unless */
/* to save array space */


/* Array sizes */
/* ----------- */
/* Maximum number of devices that may be assigned at once excluding the */
/* workdisc (size of half of the arrays in SEMCBS). If you are using a */
/* 16-bit integer model (LNINT=2) then NDVS should not exceed 31. */


/* Device number for work disc */


/* Maximum number of display partition descriptors that may be created */
/* at once.  Currently depends only on size of work disc (see SEMINI), */
/* but will depend on array size in COMMON when multiple display devices */
/* are to be supported. */


/* Maximum number of display look-up tables */


/* Number of row buffers declared in COMMON /SEMBUF/ */


/* Size in bytes of Semper's row buffers (single array RBS declares */
/* contiguous series of row buffers) */
/* Controls maximum row lengths of pictures processed - must be a */
/* multiple of LNBLK and at least 256*LNREAL */


/* Number of REALs allowed for edge processing at either end of each row */
/* buffer */


/* Size in bytes of entire row buffer space (including edge pixels) */


/* Maximum number of pictures that may be open at once (size of the PCB */
/* arrays) */


/* Maximum number of variables that can be set at once (size of the value */
/* and name arrays in common SEMTAB) */


/* Maximum length to which input command line may be continued (size of */
/* the array LINBUF), and size in blocks. */


/* Maximum length of the temporary work disc name */


/* ****** ****** ****** */

/* Maximum number of options, keys and open definitions supported by */
/* any one command */


/* Enumerated class numbers */



/* Enumerated form numbers */


/* Enumerated medium numbers */


/* Enumerated file device types */


/* Enumerated signal position numbers */


/* Picture label item parameters */



/* Items LBPLTY+1 (57) to 99 currently unused */


/* Derived parameters */

/* Number of bytes/blocks in a picture label */


/* Number of bytes/blocks in a DPD */


/* Number of integers need to pad DPD to next block boundary (1 added to */
/* ensure NDPPAD always greater than zero) */


/* Size of Procedure directory slots in bytes/blocks etc. */
/* Size of index area array in entries/blocks */

/* ****** WOS ****** */
/* increased from 256 to 512 - but is this safe? */
/* ****** *** ****** */

/* Maximum length of procedure name */


/* Enumerated type of Program types */


/* Enumerated type for logical output streams */


/* Diagnostic message severity values */


/* Size of input/output buffer for centralised I/O */


/* Number of monitor channel */


/* Length of COMPLEX pixels */


/* Last protected and last fixed variable in SEMTAB */


/* Number of global options, keys defined in SEMFVD */


/* Standard values for pi and 2*pi */


/* Remote Sensing Commands base error address */


/* User written Commands base error address */


/* Length of keyboard buffer array entry */


/* Maximum number of keyboard buffers */


/* Special error codes */



/* Copyright (C) 1988-1993 Synoptics Ltd */

/* Ascii character set parameters */


/* Function parameters, control codes etc. */

/* KBRET = return key */
/* KBTAB = TAB key */
/* KBESC = ESC key */
/* KBDEL = Delete character key */
/* KBKILL = Delete line key */
/* KBINS = Insert/Overstrike toggle key */
/* KBHOME,KBEND = Move to beginning/end of line */
/* KBREFL = Refresh line */
/* KBUP, KBDOWN, KBLEFT, KBRITE = Up, down, left and right arrow keys */
/* KBFUNC = Beginning of function key sequence */
/* KBFMAX = End of function key sequence (max. no. function keys = 256) */
/* KMBUT = passed back if mouse button close detected during keyboard */
/*         input via INKEY ONLY.  Passed back as KMBUT + button number */
/* KMBMAX = End of mouse button sequence (max. no. mouse buttons = 256) */
/* KBNONE = No key/button press, undefined input for no-wait input */
/*         -------------------- */








/* SEMTAB holds the command level variable table and */
/*        information relating to local variables */




/* SEMCBS holds info on devices & pictures currently open (reals first, */
/* then integers*4, then integers) */






/* SEMFRL holds information relating to the work disc and FOR loops */




/* SEMPRI holds information relating to Program device input, program */
/*        text management routines and the data for the TIME command */



/* SEMPLI holds information relating to buffered program command line */



/* SEMCIP holds information relating to command interpretation */





/* SEMARG used for miscellaneous arg passing between modules */
/*        it also contains dummy variables for ignored results */





/* SEMCFG contains installation configuration parameters that may be */
/* changed at run-time */



/* SEMPDB provides a display partition descriptor buffer */




/* SEMLDB contains the information pertaining to look-up tables */


/* SEMGDB contains all of the data required for the operation of the */
/* FSXXXX frame-store graphics routines */




/* SEMIOB contains text buffer for centralised I/O and any other */
/*        CHARACTER variables that need to be in COMMON */



/* SEMIOD contains rest of data for centralised I/O */




/* SEMEVE contains flags for event utility routines */


/* SEMKB1 contains information about keyboard buffer */


/* SEMKB2 contains the actual keyboard buffer array */


/* Declare external type conversion functions */


/* Copyright (C) 1986-1996 Synoptics Ltd */








/*     Packed names */


/*     'C' functions */


#line 52 "crdwrt.f"
    lwrite = semcip_1.verb == 5461;

/*     Fetch file name from key NAME, prompting if key absent */

#line 56 "crdwrt.f"
    if (filstr_(" ", file, &nf, &lwrite, (ftnlen)1, (ftnlen)255)) {
#line 56 "crdwrt.f"
	goto L130;
#line 56 "crdwrt.f"
    }
#line 57 "crdwrt.f"
    if (nf == 0) {
#line 57 "crdwrt.f"
	goto L130;
#line 57 "crdwrt.f"
    }

/*     Branch on whether reading or writing file */

#line 61 "crdwrt.f"
    if (lwrite) {

/*        Build full pathname, and see if file exists */

#line 65 "crdwrt.f"
	if (filmak_(file, ".pic", pathnm, (ftnlen)255, (ftnlen)4, (ftnlen)255)
		) {
#line 65 "crdwrt.f"
	    goto L130;
#line 65 "crdwrt.f"
	}
#line 66 "crdwrt.f"
	nf = lnblnk_(pathnm, (ftnlen)255);

#line 68 "crdwrt.f"
	if (eikexi_(pathnm, &exists, nf)) {
#line 68 "crdwrt.f"
	    goto L130;
#line 68 "crdwrt.f"
	}

/*        If file already exists, delete it if NEW requested, otherwise */
/*        raise an error. */

#line 73 "crdwrt.f"
	if (exists) {
#line 74 "crdwrt.f"
	    if (opt_(&c__22623)) {
#line 75 "crdwrt.f"
		if (eikdel_(pathnm, nf)) {
#line 75 "crdwrt.f"
		    goto L70;
#line 75 "crdwrt.f"
		}
#line 76 "crdwrt.f"
		exists = FALSE_;
#line 77 "crdwrt.f"
	    } else {
#line 78 "crdwrt.f"
		semcip_1.error = 135;
#line 79 "crdwrt.f"
		s_copy(semiob_1.idmess, pathnm, (ftnlen)78, nf);
#line 80 "crdwrt.f"
		goto L130;
#line 81 "crdwrt.f"
	    }
#line 82 "crdwrt.f"
	}
#line 83 "crdwrt.f"
	iop = 2;
#line 84 "crdwrt.f"
    } else {

/*        See if file exists on the path if reading */

#line 88 "crdwrt.f"
	if (filsea_(file, ".pic", pathnm, &exists, nf, (ftnlen)4, (ftnlen)255)
		) {
#line 88 "crdwrt.f"
	    goto L130;
#line 88 "crdwrt.f"
	}
#line 90 "crdwrt.f"
	if (exists) {
#line 91 "crdwrt.f"
	    nf = lnblnk_(pathnm, (ftnlen)255);
#line 92 "crdwrt.f"
	    iop = 1;
#line 93 "crdwrt.f"
	} else {

/*           Error - non-existent file requested for read */

#line 97 "crdwrt.f"
	    semcip_1.error = 130;
#line 98 "crdwrt.f"
	    s_copy(semiob_1.idmess, file, (ftnlen)78, nf);
#line 99 "crdwrt.f"
	    goto L130;
#line 100 "crdwrt.f"
	}
#line 101 "crdwrt.f"
    }

/*     Try to open the file */

#line 105 "crdwrt.f"
    if (eikope_(&iop, &fd, pathnm, nf)) {
#line 105 "crdwrt.f"
	goto L80;
#line 105 "crdwrt.f"
    }

/*     Switch code on whether reading or writing file header info. */

#line 109 "crdwrt.f"
    if (lwrite) {

/*        Code for CPUT */
/*        ------------- */
/*        Fetch dimensions, etc. from picture */

#line 115 "crdwrt.f"
	*nx = (shortint) semcbs_1.ncols[semcip_1.lp1 - 1];
#line 116 "crdwrt.f"
	ncol = *nx;
#line 117 "crdwrt.f"
	*ny = (shortint) semcbs_1.nrows[semcip_1.lp1 - 1];
#line 118 "crdwrt.f"
	nrow = *ny;
#line 119 "crdwrt.f"
	*nz = (shortint) semcbs_1.nlays[semcip_1.lp1 - 1];
#line 120 "crdwrt.f"
	nlay = *nz;
#line 121 "crdwrt.f"
	*ramp0 = 0;
#line 122 "crdwrt.f"
	*rmp255 = 255;
#line 123 "crdwrt.f"
	*noter = 0;
#line 124 "crdwrt.f"
	*isbyte = 1;
#line 125 "crdwrt.f"
	*filno = 0;
#line 126 "crdwrt.f"
	*ismerg = 0;
#line 127 "crdwrt.f"
	*col = 7;
#line 128 "crdwrt.f"
	*fileid = 12345;
#line 129 "crdwrt.f"
	*lamp0 = 0;
#line 130 "crdwrt.f"
	*lmp255 = 255;
#line 131 "crdwrt.f"
	*lcol = 0;
#line 132 "crdwrt.f"
	*isedit = 0;
#line 133 "crdwrt.f"
	*lens = 10;

/* MAGFAC Should be 1.0 ! */

#line 137 "crdwrt.f"
	*magfac = 1065353216;
#line 138 "crdwrt.f"
	dum[0] = 0;
#line 139 "crdwrt.f"
	dum[1] = 0;
#line 140 "crdwrt.f"
	dum[2] = 0;

#line 142 "crdwrt.f"
	if (fildcd_(file, prefix, name__, exten, (ftnlen)255, (ftnlen)255, (
		ftnlen)255, (ftnlen)4)) {
#line 142 "crdwrt.f"
	    goto L120;
#line 142 "crdwrt.f"
	}

/* Build name in A1NAME */

#line 146 "crdwrt.f"
	k = lnblnk_(name__, (ftnlen)255);
#line 147 "crdwrt.f"
	if (k > 32) {
#line 147 "crdwrt.f"
	    k = 32;
#line 147 "crdwrt.f"
	}
#line 148 "crdwrt.f"
	for (j = k + 1; j <= 32; ++j) {
#line 149 "crdwrt.f"
	    a1temp[i__ - 1] = 0;
#line 150 "crdwrt.f"
/* L10: */
#line 150 "crdwrt.f"
	}

#line 152 "crdwrt.f"
	semics_(name__, a1temp, &k, nchars);
#line 153 "crdwrt.f"
	nchars = k;
#line 154 "crdwrt.f"
	cform_(a1temp, a1name, &c__1, &c__0, &nchars);

/*        Output header line */

#line 158 "crdwrt.f"
	headsw_(header);
#line 159 "crdwrt.f"
	if (eikbya_(&iop, &fd, header, &c__76)) {
#line 159 "crdwrt.f"
	    goto L90;
#line 159 "crdwrt.f"
	}

#line 161 "crdwrt.f"
	i__1 = nlay;
#line 161 "crdwrt.f"
	for (j = 1; j <= i__1; ++j) {
#line 162 "crdwrt.f"
	    i__2 = nrow;
#line 162 "crdwrt.f"
	    for (i__ = 1; i__ <= i__2; ++i__) {
#line 163 "crdwrt.f"
		if (semrow_(&c__1, rb1, &c__0, &i__, &j, &semcip_1.lp1)) {
#line 163 "crdwrt.f"
		    goto L120;
#line 163 "crdwrt.f"
		}
#line 164 "crdwrt.f"
		if (eikbya_(&iop, &fd, rb1, &ncol)) {
#line 164 "crdwrt.f"
		    goto L90;
#line 164 "crdwrt.f"
		}
#line 165 "crdwrt.f"
/* L20: */
#line 165 "crdwrt.f"
	    }
#line 166 "crdwrt.f"
/* L30: */
#line 166 "crdwrt.f"
	}
#line 167 "crdwrt.f"
    } else {

/*        Code for CGET */
/*        ------------- */
/*        Read header line */

#line 173 "crdwrt.f"
	if (eikbya_(&iop, &fd, header, &c__76)) {
#line 173 "crdwrt.f"
	    goto L90;
#line 173 "crdwrt.f"
	}
#line 174 "crdwrt.f"
	headsw_(header);
#line 175 "crdwrt.f"
	if (*fileid != 12345) {
#line 176 "crdwrt.f"
	    semcip_1.error = 77;
#line 177 "crdwrt.f"
	    s_copy(semiob_1.idmess, "File is not a valid confocal picture fi\
le", (ftnlen)78, (ftnlen)41);
#line 178 "crdwrt.f"
	    goto L120;
#line 179 "crdwrt.f"
	}

/*        Open Semper picture */

#line 183 "crdwrt.f"
	semcip_1.lp1 = 0;
#line 184 "crdwrt.f"
	ncol = *nx;
#line 185 "crdwrt.f"
	nrow = *ny;
#line 186 "crdwrt.f"
	nlay = *nz;
#line 187 "crdwrt.f"
	i__1 = ivalpn_(&c_n601);
#line 187 "crdwrt.f"
	i__2 = semfrm_(&c__0);
#line 187 "crdwrt.f"
	if (semopn_(&c__2, &i__1, &ncol, &nrow, &nlay, &c__1, &i__2, &
		semcip_1.lp1)) {
#line 187 "crdwrt.f"
	    goto L120;
#line 187 "crdwrt.f"
	}
#line 189 "crdwrt.f"
	i__1 = nlay;
#line 189 "crdwrt.f"
	for (j = 1; j <= i__1; ++j) {
#line 190 "crdwrt.f"
	    if (*isbyte != 0) {
#line 191 "crdwrt.f"
		i__2 = nrow;
#line 191 "crdwrt.f"
		for (i__ = 1; i__ <= i__2; ++i__) {
#line 192 "crdwrt.f"
		    if (eikbya_(&iop, &fd, rb1, &ncol)) {
#line 192 "crdwrt.f"
			goto L90;
#line 192 "crdwrt.f"
		    }
#line 193 "crdwrt.f"
		    if (semrow_(&c__2, rb1, &c__0, &i__, &j, &semcip_1.lp1)) {
#line 193 "crdwrt.f"
			goto L120;
#line 193 "crdwrt.f"
		    }
#line 194 "crdwrt.f"
/* L40: */
#line 194 "crdwrt.f"
		}
#line 195 "crdwrt.f"
	    } else {
#line 196 "crdwrt.f"
		i__2 = nrow;
#line 196 "crdwrt.f"
		for (i__ = 1; i__ <= i__2; ++i__) {
#line 197 "crdwrt.f"
		    i__3 = ncol + ncol;
#line 197 "crdwrt.f"
		    if (eikbya_(&iop, &fd, rb1, &i__3)) {
#line 197 "crdwrt.f"
			goto L90;
#line 197 "crdwrt.f"
		    }
#line 198 "crdwrt.f"
		    irowsw_(rb1, rb3, &ncol);
#line 199 "crdwrt.f"
		    if (semrow_(&c__2, rb3, &c__1, &i__, &j, &semcip_1.lp1)) {
#line 199 "crdwrt.f"
			goto L120;
#line 199 "crdwrt.f"
		    }
#line 200 "crdwrt.f"
/* L50: */
#line 200 "crdwrt.f"
		}
#line 201 "crdwrt.f"
	    }
#line 202 "crdwrt.f"
/* L60: */
#line 202 "crdwrt.f"
	}
#line 203 "crdwrt.f"
    }

#line 205 "crdwrt.f"
    if (eikclo_(&fd)) {
#line 205 "crdwrt.f"
	goto L100;
#line 205 "crdwrt.f"
    }
#line 206 "crdwrt.f"
    goto L130;

/*     Deal with UNIX I/O errors */

#line 210 "crdwrt.f"
L70:
/* Writing concatenation */
#line 210 "crdwrt.f"
    i__4[0] = 29, a__1[0] = "Error deleting existing file ";
#line 210 "crdwrt.f"
    i__4[1] = nf, a__1[1] = pathnm;
#line 210 "crdwrt.f"
    s_cat(semiob_1.idmess, a__1, i__4, &c__2, (ftnlen)78);
#line 211 "crdwrt.f"
    goto L110;

#line 213 "crdwrt.f"
L80:
/* Writing concatenation */
#line 213 "crdwrt.f"
    i__4[0] = 19, a__1[0] = "Error opening file ";
#line 213 "crdwrt.f"
    i__4[1] = nf, a__1[1] = pathnm;
#line 213 "crdwrt.f"
    s_cat(semiob_1.idmess, a__1, i__4, &c__2, (ftnlen)78);
#line 214 "crdwrt.f"
    goto L110;

#line 216 "crdwrt.f"
L90:
#line 216 "crdwrt.f"
    if (iop == 1) {
/* Writing concatenation */
#line 217 "crdwrt.f"
	i__4[0] = 19, a__1[0] = "Error reading file ";
#line 217 "crdwrt.f"
	i__4[1] = nf, a__1[1] = pathnm;
#line 217 "crdwrt.f"
	s_cat(semiob_1.idmess, a__1, i__4, &c__2, (ftnlen)78);
#line 218 "crdwrt.f"
    } else {
/* Writing concatenation */
#line 219 "crdwrt.f"
	i__4[0] = 19, a__1[0] = "Error writing file ";
#line 219 "crdwrt.f"
	i__4[1] = nf, a__1[1] = pathnm;
#line 219 "crdwrt.f"
	s_cat(semiob_1.idmess, a__1, i__4, &c__2, (ftnlen)78);
#line 220 "crdwrt.f"
    }
#line 221 "crdwrt.f"
    semcip_1.error = 77;
#line 222 "crdwrt.f"
    goto L120;

#line 224 "crdwrt.f"
L100:
/* Writing concatenation */
#line 224 "crdwrt.f"
    i__4[0] = 19, a__1[0] = "Error closing file ";
#line 224 "crdwrt.f"
    i__4[1] = nf, a__1[1] = pathnm;
#line 224 "crdwrt.f"
    s_cat(semiob_1.idmess, a__1, i__4, &c__2, (ftnlen)78);

#line 226 "crdwrt.f"
L110:
#line 226 "crdwrt.f"
    semcip_1.error = 77;
#line 227 "crdwrt.f"
    goto L130;

/*     Closing a file after an error (ignore any error on closing file) */

#line 231 "crdwrt.f"
L120:
#line 231 "crdwrt.f"
    if (eikclo_(&fd)) {
#line 231 "crdwrt.f"
	goto L130;
#line 231 "crdwrt.f"
    }

/*     All done */

#line 235 "crdwrt.f"
L130:
#line 235 "crdwrt.f"
    return 0;

/* Copyright (C) 1987-1992:  Synoptics Ltd,  All Rights Reserved */

} /* crdwrt_ */

#undef noter
#undef magfac
#undef lens
#undef isedit
#undef lcol
#undef lmp255
#undef lamp0
#undef fileid
#undef ismerg
#undef a1name
#undef filno
#undef isbyte
#undef rmp255
#undef ramp0
#undef header
#undef aligns
#undef serinp
#undef sercom
#undef sermon
#undef serlog
#undef serdia
#undef sercon
#undef souinp
#undef soucom
#undef soumon
#undef soulog
#undef soudia
#undef soucon
#undef echinp
#undef echcom
#undef echmon
#undef echlog
#undef echcon
#undef echdia
#undef rb6rhs
#undef rb5rhs
#undef rb4rhs
#undef rb3rhs
#undef rb2rhs
#undef rb1rhs
#undef rb6lhs
#undef rb5lhs
#undef rb4lhs
#undef rb3lhs
#undef rb2lhs
#undef rb1lhs
#undef dum
#undef col
#undef rb6
#undef rb5
#undef rb4
#undef rb3
#undef rb2
#undef rb1
#undef nz
#undef ny
#undef nx


/* Subroutine HEADSW - swap integer packing order in header */

/* Subroutine */ int headsw_(header)
logical1 *header;
{
    integer i__;
    logical1 hold;




#line 248 "crdwrt.f"
    /* Parameter adjustments */
#line 248 "crdwrt.f"
    --header;
#line 248 "crdwrt.f"

#line 248 "crdwrt.f"
    /* Function Body */
#line 248 "crdwrt.f"
    for (i__ = 1; i__ <= 9; i__ += 2) {
#line 249 "crdwrt.f"
	hold = header[i__];
#line 250 "crdwrt.f"
	header[i__] = header[i__ + 1];
#line 251 "crdwrt.f"
	header[i__ + 1] = hold;
#line 252 "crdwrt.f"
/* L10: */
#line 252 "crdwrt.f"
    }

#line 254 "crdwrt.f"
    hold = header[11];
#line 255 "crdwrt.f"
    header[11] = header[14];
#line 256 "crdwrt.f"
    header[14] = hold;

#line 258 "crdwrt.f"
    hold = header[12];
#line 259 "crdwrt.f"
    header[12] = header[13];
#line 260 "crdwrt.f"
    header[13] = hold;

#line 262 "crdwrt.f"
    for (i__ = 15; i__ <= 65; i__ += 2) {
#line 263 "crdwrt.f"
	hold = header[i__];
#line 264 "crdwrt.f"
	header[i__] = header[i__ + 1];
#line 265 "crdwrt.f"
	header[i__ + 1] = hold;
#line 266 "crdwrt.f"
/* L20: */
#line 266 "crdwrt.f"
    }

#line 268 "crdwrt.f"
    hold = header[67];
#line 269 "crdwrt.f"
    header[67] = header[70];
#line 270 "crdwrt.f"
    header[70] = hold;

#line 272 "crdwrt.f"
    hold = header[68];
#line 273 "crdwrt.f"
    header[68] = header[69];
#line 274 "crdwrt.f"
    header[69] = hold;

#line 276 "crdwrt.f"
    for (i__ = 71; i__ <= 75; i__ += 2) {
#line 277 "crdwrt.f"
	hold = header[i__];
#line 278 "crdwrt.f"
	header[i__] = header[i__ + 1];
#line 279 "crdwrt.f"
	header[i__ + 1] = hold;
#line 280 "crdwrt.f"
/* L30: */
#line 280 "crdwrt.f"
    }

#line 282 "crdwrt.f"
    return 0;

/* Copyright (C) 1989:  Synoptics Ltd,  All Rights Reserved */

} /* headsw_ */

