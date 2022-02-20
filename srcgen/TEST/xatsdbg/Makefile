######
#
# For testing xinterp
#
######
ECHO=echo
######
XINTERP=\
"./../../../bin/xinterp"
######
all:: test
######
test:: ; $(XINTERP)
######
test:: \
fact01
fact01: ; \
$(XINTERP) -d ./fact01.dats
######

######
#
# HX: for cleaning up
#
######
clean:: ; rm -f *~
######
cleanall:: clean
cleanall:: ; rm -f output
######
# export XATSHOME=$(XANADU)
# XANADU=$(shell pwd)/../../xanadu
######
###### end of [Makefile_output] ######
