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
test:: \
fact02
fact02: ; \
$(XINTERP) -d ./fact02.dats
######
test:: \
fibo01
fibo01: ; \
$(XINTERP) -d ./fibo01.dats
test:: \
fibo02
fibo02: ; \
$(XINTERP) -d ./fibo02.dats
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
