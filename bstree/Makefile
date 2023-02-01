######
#
# Note that
# certain installations require the following changes:
#
# atscc -> patscc
# atsopt -> patsopt
# ATSHOME -> PATSHOME
#
######
#
CCOPT=gcc -std=c99 -D_GNU_SOURCE
PATSCC=$(PATSHOME)/bin/patscc
PATSOPT=$(PATSHOME)/bin/patsopt

########

INCLUDES=-I$(PATSHOME) -I$(PATSHOME)/ccomp/runtime

########

all:: bstree bstreeg

#
######
#
#
# HX: Please uncomment the one you want, or skip it entirely
#
#ATSCCFLAGS=
CFLAGS=
ATSCCFLAGS=
#
# '-flto' enables link-time optimization such as inlining lib functions
#
# ATSCCFLAGS=-O2 -flto
#
#
######
#
cleanall::
#
######
#
# Please uncomment the following three lines and replace the name [foo]
# with the name of the file you want to compile
#
bstree: bstree_dats.c ; \
  $(CCOPT) -DATS_MEMALLOC_LIBC $(INCLUDES) $(CFLAGS) -o "$@.bin" $< || echo $@ ": ERROR!!!" 
regress:: bstree; ./bstree
cleanall:: ; $(RMF) bstree


bstreeg: bstreeg_dats.c ; \
  $(CCOPT) -DATS_MEMALLOC_LIBC $(INCLUDES) $(CFLAGS) -o "$@.bin" $< || echo $@ ": ERROR!!!" 
regress:: bstreeg; ./bstreeg
cleanall:: ; $(RMF) bstreeg
#
######
#
# You may find these rules useful
#
# %_sats.o: %.sats ; $(ATSCC) $(ATSCCFLAGS) -c $< || echo $@ ": ERROR!!!"
# %_dats.o: %.dats ; $(ATSCC) $(ATSCCFLAGS) -c $< || echo $@ ": ERROR!!!"
#
######
%_dats.c: %.dats ; \
	$(PATSOPT) $(ATSCFLAGS) -o $@ --dynamic $< || echo $@ ": ERROR!!!"
######
#
RMF=rm -f
#
######
#
clean:: ; $(RMF) *~
clean:: ; $(RMF) *_?ats.o
clean:: ; $(RMF) *_?ats.c
clean:: ; $(RMF) *.bin
#
cleanall:: clean
#
###### end of [Makefile] ######
