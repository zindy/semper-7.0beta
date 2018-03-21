#  Makefile for Semper 6.4 plus
#  ----------------------------
#
#  The #STARTKEEP and #ENDKEEP directives in this file are there to
#  enable the production system to produce a cut-down makefile for
#  user-extendable systems: they are treated as comments by make, but
#  should be considered carefully when this file is modified.
#
#  First define the general compiler options
#
.f.o: ; @echo $*.f
	@f77 -woff 2569 -w66 -i2 -c -O2 -OPT:Olimit=2048 $*.f
# WOS what IS -w66?  suppress warnings about incompatibilities with f66?
# WOS -woff 2569 suppresses warning #2569 (integer constant > i2)
# WOS -N1250 removed from above Feb 01 when compiler said no longer needed
# WOS -Olimit 1024 rephrased as -OPT:Olimit=2048 (suppresses optimisation
#        of routines larger than this - but what units? - for speed
# WOS -u removed from above to suppress IMPLICIT NONE for ll.f etc
# WOS when useful, debug support wants -g2 following -O2

.c.o: ; @echo $*.c
	@cc -DANSI_C -O2 -OPT:Olimit=2048 -c $*.c
# WOS -DANSI_C to force definition of ANSI_C, which switches declarations
#   in dcache,cdisc,mcdc61,cunix (probably existed in previous releases?)
#   FIRMLY experimental basis at present
# WOS -Olimit 1024 rephrased as -OPT:Olimit=2048

LIBS= user.a command.a middle.a kernel.a
# WOS user.a removed from .PRECIOUS, since corrupted if ar interrupted
.PRECIOUS: command.a middle.a kernel.a

#
#  Set up the macros for the host libraries and the link options
#  -------------------------------------------------------------
#
SEMGEN=semgen
LINK= f77
HOST= -lX11 -limage
# WOS add -g to HOST for debug support
OPTS=
DISPLAY= XWindows
#
#  SET UP THE BUILD TARGETS.  Set up Semper first, so it gets built
#  ----------------------------------------------------------------
#  by default
#  ----------
#
dsemper: semper.o $(LIBS)
	@echo -n Linking Semper 6 for $(DISPLAY) as dsemper...
	@$(LINK) -o dsemper $(OPTS) semper.o $(LIBS) $(HOST)
	@echo "dsemper rebuilt - mv dsemper semper if OK"

semper:
	@echo Make dsemper instead, please

love:
	@echo not war?

#
#  Define the headers which systems depend on... first general headers...
#

USHDR= COMMON PARAMS ICSET EVENTS

semper.o: semper.f $(USHDR)
#WOS emfds line inserted
emfds.o: emfdcl.f $(USHDR) EMFDCL
semper.f: $(SEMGEN) semper.syn
	@./semgen

HDR= $(USHDR) EQXCOM EQCOM
CHDR= events.h ftypes.h icset.h params.h dcache.h
#
#  ... UIF ...
#
UHDR= $(HDR) UIFCOM UIXCOM UIFERR STORECOM LOGINDEX
#
#  ... system specific headers ...
#
#
#  Now define the actual 'Sub systems'...
#
#  ... graphics routines...
#
FS0= fsampl.o fsarc.o fsarro.o fsband.o fsbord.o fscirc.o fsclip.o
FS1= fsctyp.o fscurs.o fscurv.o fsdrag.o fsedge.o fselec.o fseras.o
FS2= fsflus.o fsinit.o fsline.o fslist.o fslurw.o fsmark.o fsoptn.o
FS3= fsovrw.o fsqbor.o fsqcha.o fsqlim.o fsqmon.o fsqtra.o fsregn.o
FS4= fsrow.o fstext.o fsview.o fsxwir.o
#
#  ... program management...
#
PM1= prucas.o prucop.o pruent.o prufls.o pruind.o pruinf.o prulab.o prupri.o
PM2= prusho.o pruslt.o prusor.o pruadd.o prucom.o prudir.o prutin.o
#
#  ... C sources (kept separate for header file dependencies)...
#
C1= stdio.o xrect.o
C2= a1conv.o bitfld.o bmap33.o ccache.o cdisc.o dcache.o fs.o usleep.o mctime.o
C3= imgio.o
C4=
C5=
C6= gethid.o
C7= cunix.o
CS= $(C1) $(C2) $(C3) $(C4) $(C5) $(C6) $(C7)
#
$(CS):  $(CHDR)
#
#  Group these together sensibly
#
SUBS= $(FS0) $(FS1) $(FS2) $(FS4) $(FS3) $(PM1) $(PM2)
#
#  Now set up the macros for general Semper
#  ----------------------------------------
#
#  Local commands
#
COM00= crdwrt.o erdwrt.o funix.o iptget.o srdwrt.o zrdwrt.o
#
#  Start with the command procedures and essential routines
#
COM01= acf.o analys.o analy2.o ask.o assig.o bakpro.o basfit.o bdiler.o
COM02= bhmt.o blkmea.o blkvar.o bmlut.o bmmap.o bmproc.o bopclo.o box.o
COM03= beep.o bprint.o cache.o calc.o chull.o closed.o cls.o colour.o
COM04= copyp.o corect.o covar.o ctf.o ctf1.o ctf2.o cut.o deass.o
COM05= destrp.o ddiler.o differ.o disp.o dlabel.o dlabl3.o dlabl5.o
COM06= dmanu.o dopclo.o drag.o dskiz.o dt.o dzone.o echo.o edge.o editor.o
COM07= edstub.o erase.o eventc.o exampd.o expand.o extime.o extrac.o
COM08= extra2.o extra3.o extra4.o extrcb.o extrct.o extrnn.o featur.o
COM09= fft.o find.o firfil.o fir33.o fir55.o fitsbd.o fitlc.o flood.o
COM10= flushd.o frdwrt.o frdwrf.o frdwru.o gau.o halful.o hilbrt.o histog.o
COM11= hfill.o hplj.o image.o inputf.o jump.o label.o labsub.o lattic.o
COM12= letter.o lmanu.o lpdet.o locals.o lutadj.o lutset.o macro.o magsr.o
COM13= mappix.o mark.o mask.o mask1.o mask2.o mask3.o maxlik.o mindis.o
COM14= monitm.o morph.o motif.o mrtran.o noise.o ocf.o outptf.o overly.o
COM15= ovread.o ovwrit.o page.o partit.o paste.o pca.o pcalc.o pcurve.o
COM16= pdraw.o peaks.o peaksd.o peaksr.o pedit.o pextr.o pferet.o picdat.o
COM17= pfiltr.o phist.o phrand.o pictur.o pid.o pmanu.o pmark.o porder.o pmomen.o
COM18= prdwrt.o printc.o projec.o ps.o pset.o pshow.o psout.o psout0.o
COM19= psout1.o psout2.o psout3.o psout4.o psout5.o psout6.o pssubs.o
COM20= ptype.o pxcon.o pxsca.o radx50.o ramps.o rank.o rcf.o reinit.o
COM21= report.o retur.o rewin.o rfiltr.o rgb.o saver.o sectn.o semend.o
COM22= semfor.o semlib.o semloo.o semnxt.o sempix.o semrun.o semsig.o
COM23= semslg.o semsyn.o separt.o sheet.o show.o show0.o show1.o show2.o
COM24= show3.o show4.o show5.o show6.o show7.o show8.o show9.o showlf.o
COM25= shownl.o showno.o showsy.o sketch.o solid.o stack.o survey.o
COM26= thresh.o train.o turn.o type.o view.o vgcon.o waitu.o walsh.o
COM27= warp.o warp2.o warp3.o weight.o window.o xcf.o xwires.o ymod.o
COM0A= $(COM00) $(COM01) $(COM02) $(COM03) $(COM04) $(COM05) $(COM06)
COM0B= $(COM07) $(COM08) $(COM09) $(COM10) $(COM11) $(COM12) $(COM13)
COM0C= $(COM14) $(COM15) $(COM16) $(COM17) $(COM18) $(COM19) $(COM20)
COM0D= $(COM21) $(COM22) $(COM23) $(COM24) $(COM25) $(COM26) $(COM27)
COMMS= $(COM0A) $(COM0B) $(COM0C) $(COM0D)
#
$(COMMS): $(HDR)
#
# MID00 contains the C source versions
#
MID00= cform.o eikio.o mcdc61.o mctp61.o semhan.o
$(MID00): $(CHDR)
#
# MID01 to MID03 contains top/middle level UIF routines
#
MID01= compm.o devices.o initm.o uifini.o uifinp.o uixinp.o uixipe.o
MID02= uvcell.o uvdevi.o uvele1.o uvele2.o uvexec.o uvjust.o uvmenu.o
MID03= uvmous.o uvpane.o uvtext.o uvuif.o
MID0A= $(MID01) $(MID02) $(MID03)
$(MID0A): $(UHDR)
#
# MID04 onwards contain the middle level Semper routines
#
MID04= abandn.o assdis.o asstap.o assfil.o assscr.o assnew.o assold.o
MID05= assmem.o asssiz.o assnam.o asshdr.o
MID06= colft.o conopt.o bmmdsk.o bmmmem.o bmpdsk.o bmpmem.o datstr.o dftm.o
MID07= dftm2.o dftsub.o dspsub.o dthres.o dtrans.o ecdec.o eqflus.o eqinit.o
MID08= eqnext.o eqnqre.o eqread.o eqsets.o eqterm.o filcio.o filcls.o fildcd.o
MID09= fildir.o filelm.o filexi.o filmak.o filopn.o filpos.o filsea.o findcm.o
MID10= ft1d.o ft2d.o gamfn.o genhst.o getpat.o gettmp.o graph.o inpbmp.o inpdef.o
MID11= inpnem.o inpras.o inpraw.o inptif.o inpunf.o invdet.o invft2.o iosmsg.o
MID12= iosubs.o ipack.o ival.o ivalpn.o kline.o lmanus.o lutsub.o magsub.o
MID13= marset.o meansd.o mrkreg.o opt.o optno.o outbmp.o outdef.o outnew.o
MID14= outras.o outraw.o outtif.o outunf.o psort.o range.o rsum1d.o sembee.o
MID15= semcen.o semcls.o semcp2.o semcpw.o semcom.o semcon.o semctx.o semdcr.o
MID16= semdel.o semdia.o semdpd.o semdpn.o semech.o semenv.o semeol.o semerr.o
MID17= semexp.o semfac.o semfil.o semfrm.o semfvd.o semhel.o seminx.o semioe.o
MID18= semini.o semin2.o semjmp.o semktx.o semlab.o semlin.o semlnf.o
MID19= semlog.o semlu.o  semlut.o semmac.o semmed.o semmor.o semopn.o
MID20= sempag.o semppn.o semprg.o semret.o semrng.o semrow.o semser.o semsol.o
MID21= semsop.o semsou.o semszz.o semtbk.o semtfc.o semtps.o semtyp.o semunw.o
MID22= semwai.o semxpl.o semzzz.o setppr.o tape.o textu1.o textu2.o textu3.o
MID23= timstr.o tstsrg.o unpack.o val.o varset.o yesno.o
#
MID0B= $(MID04) $(MID05) $(MID06) $(MID07) $(MID08) $(MID09) $(MID10)
MID0C= $(MID11) $(MID12) $(MID13) $(MID14) $(MID15) $(MID16) $(MID17)
MID0D= $(MID18) $(MID19) $(MID20) $(MID21) $(MID22) $(MID23)
$(MID0B): $(HDR)
$(MID0C): $(HDR)
$(MID0D): $(HDR)
#
MIDDLE= $(MID00) $(MID0A) $(MID0B) $(MID0C) $(MID0D)
#
# The kernel stuff - UIF + routines called by user only or limited places
#
KER01= $(SUBS) convert.o
KER02= disc.o eqbsup.o eqgetd.o eqksup.o eqpsup.o eqsetd.o eqssup.o
KER03= evclos.o evflus.o evopen.o evqenq.o filpat.o fortde.o fortrd.o fortwr.o
KER04= getcl.o getfrm.o getrg1.o getrg2.o getrng.o inkey.o kpress.o kread.o
KER05= mkchar.o mkicha.o nblank.o obeycl.o pdelta.o psigma.o readln.o semask.o
KER06= sembrk.o semchs.o semics.o seminp.o semint.o semmon.o sempro.o semsel.o
KER07= semtit.o semxa1.o setvar.o unsetv.o varint.o
KER08= fssubs.o waits.o
KER09= fsoq61.o
KERN0A= $(KER01) $(KER02) $(KER03) $(KER04) $(KER05)
KERN0B= $(KER06) $(KER07) $(KER08) $(KER09)
KERNEL= $(KERN0A) $(KERN0B)
#
$(KERNEL): $(HDR)
#
# Demo objects
#
DEMO= assig_d.o assold_d.o asshdr_d.o semini_d.o
#
#  Now set up the macros for the user library (kept separate from Semper)
#  ----------------------------------------------------------------------
#New modules go here after user.o
USER= user.o hrems.o emfds.o ll.o pl.o npsout.o roll.o iirfil.o
#
#  Now set up the macros for UIF
#  -----------------------------
#
UIF01= allocm.o allocx.o appact.o applic.o freem.o mspace.o pindex.o
UIF02= strings.o uifape.o uifcad.o uifcbo.o uifccc.o uifcco.o uifccy.o
UIF03= uifcdo.o uifcdr.o uifcfc.o uifchi.o uifcic.o uifcin.o uifcof.o
UIF04= uifcro.o uifctc.o uifcte.o uifdac.o uifdlo.o uifdqu.o uifdre.o
UIF05= uifdrf.o uifdsa.o uifeac.o uifeck.o uifeco.o uifecr.o uifede.o
UIF06= uifedr.o uifena.o uifepo.o uifesi.o uifiaf.o uifiba.o uifibe.o
UIF07= uifigo.o uifiju.o uifite.o uifmac.o uifmda.o uifmdr.o uifmob.o
UIF08= uifmoe.o uifmop.o uifmoq.o uifmst.o uifmty.o uifpau.o uifpco.o
UIF09= uifpcr.o uifpde.o uifphi.o uifpma.o uifpna.o uifppo.o uifpsh.o
UIF10= uifpsi.o uifptr.o uifrea.o uifsac.o uifsad.o uifsas.o uifsav.o
UIF12= uifsaw.o uifsaz.o uifsta.o uiftap.o uifscs.o uifssu.o uiftas.o
UIF11= uiftco.o uiftcp.o uiftde.o uiftdr.o uiftex.o uiftin.o uiftle.o
UIF13= uiftnu.o uiftou.o uiftse.o uiftta.o uiftva.o uiftwp.o uixcdh.o
UIF15= uixcmp.o uixcpo.o uixcpu.o uixcsi.o uixdre.o uixein.o uixepo.o
UIF14= uixfil.o uixgcl.o uixica.o uixicm.o uixint.o uixipb.o uixipk.o
UIF16= uixipp.o uixisd.o uixise.o uixmsi.o uixobs.o uixoin.o uixpdr.o
UIF17= uixpin.o uixque.o uixsad.o uixscl.o uixsin.o uixsof.o uixtsi.o
UIF18= uixwin.o winbox.o winclr.o wincna.o wincol.o wincre.o windes.o
UIF19= winhid.o winpos.o winsho.o wintex.o wdxbox.o wdxclr.o wdxcol.o
UIF20= wdxcre.o wdxdes.o wdxhid.o wdxpos.o wdxsho.o wdxtex.o wfsbox.o
UIF21= wfsclr.o wfscna.o wfscol.o wfscre.o wfsdes.o wfshid.o wfspos.o
UIF22= wfssho.o wfstex.o whobox.o whoclr.o whocna.o whocol.o whocre.o
UIF23= whodes.o whohid.o whopos.o whosho.o whotex.o
UIF00= dfsdummy.o dx11.o fsxs61.o fsxw61.o
$(UIF00): $(UHDR)
#
UIF0A= $(UIF01) $(UIF02) $(UIF03) $(UIF04) $(UIF05) $(UIF06) $(UIF07) $(UIF08)
UIF0B= $(UIF09) $(UIF10) $(UIF11) $(UIF12) $(UIF13) $(UIF14) $(UIF15) $(UIF16)
UIF0C= $(UIF17) $(UIF18) $(UIF19) $(UIF20) $(UIF21) $(UIF22) $(UIF23)
UIFALL= $(UIF0A) $(UIF0B) $(UIF0C)
$(UIFALL): $(UHDR)
UIF= $(UIF00) $(UIFALL)
#
#

demo_semper: semdem.o $(DEMO) $(LIBS)
	@echo -n Linking Demo Semper 6 for $(DISPLAY)...
	@$(LINK) -o demo_semper $(OPTS) semdem.o $(DEMO) $(LIBS) $(HOST)
	@echo " done"

semdem.o: semdem.f

semdem.f: $(SEMGEN) $(HDR) semper_d.syn
	semgen semper_d.syn
	mv semper.f semdem.f

GENSUB= a1conv.o getcl.o

semgen: semgen.o $(GENSUB) $(UHDR) $(CHDR)
	@echo -n Linking Semper Generator...
	@$(LINK) -o semgen $(OPTS) semgen.o $(GENSUB) $(HOST)
	@strip semgen
	@echo " done"

semgen.o: $(HDR) semgen.f
#
#  Next the help manager
#  ---------------------
#
helpman: helpman.o $(LIBS)
	@echo -n Linking Semper Help Manager...
	@$(LINK) -o helpman $(OPTS) helpman.o $(LIBS) $(LIBS) $(LIBS) $(HOST)
	@strip helpman
	@echo " done"

helpman.o: helpman.f


#
#  Next the library build sequence.  Set up the header dependencies...
#  -------------------------------------------------------------------
#  ... first for the general Semper code...
#  ----------------------------------------
#
$(DEMO): $(HDR)
#
# ... and accumulate into the Semper library macro name
#
SEMALL= $(CS) $(KERNEL)
#
# ... and accumulate into the Semper library macro name
#
SEMP02= $(SEMALL)
SEMPER= $(UIF) $(SEMP02)
#
# NOW BUILD THE SEMPER LIBRARY
# ----------------------------
#

command.a: $(COMMS)
	@ar vru command.a $(COMMS)

middle.a: $(MIDDLE)
	@ar vru middle.a $(MIDDLE)

kernel.a: $(SEMPER)
	@ar vru kernel.a $(SEMPER)

#
# NOW BUILD THE USER LIBRARY
# --------------------------
#
user.a: $(USER)
	@ar vru user.a $(USER)
$(USER): $(USHDR)
#
# Set up a target to build all systems
# ------------------------------------
#
all: semper demo_semper semper.hlb tutor.hlb

semper.hlb: semper.shl helpman
	@echo ""
	@echo "Run helpman on semper.shl (use $ INIT)..."
	@echo ""

tutor.hlb: tutor.shl helpman
	@echo ""
	@echo "Run helpman on tutor.shl (use $ INIT 30 2)..."
	@echo ""
#
#  Finally define the targets to copy the include files to matching names
#  ----------------------------------------------------------------------
#  of different case
#  -----------------
#
COMMON: common.f
	cp common.f COMMON

PARAMS: params.f
	cp params.f PARAMS

ICSET: icset.f
	cp icset.f ICSET

EVENTS: events.f
	cp events.f EVENTS

# WOS insert
EMFDCL: emfdcl.f
	cp emfdcl.f EMFDCL

EQCOM: eqcom.f
	cp eqcom.f EQCOM

EQXCOM: eqxcom.f
	cp eqxcom.f EQXCOM

UIFCOM: uifcom.f
	cp uifcom.f UIFCOM

UIXCOM: uixcom.f
	cp uixcom.f UIXCOM

UIFERR: uiferr.f
	cp uiferr.f UIFERR

STORECOM: storecom.f
	cp storecom.f STORECOM

LOGINDEX: logindex.f
	cp logindex.f LOGINDEX
#
#  Copyright (c) 1989-1993: Synoptics Ltd., All rights reserved
#
