#  File     : Makefile
#  RCS      : $Id: Makefile,v 1.23 2004/04/02 14:04:36 schachte Exp $
#  Author   : Peter Schachte
#  Origin   : 1995
#  Purpose  : build ROBDD manipulation code to support groundness analyzer


include Makefile.SWI

SOLIBS=-L $(SWIDIR)/lib/x86_64-darwin16.4.0 -lswipl

COMMONOS=bryant.o
SOOS=$(COMMONOS) swi_robdd.o swi_bryantPrint.o
OS=$(COMMONOS) bryantPrint.o 
SO=swi_robdd.$(SHAREDEXT)
RELFILES=bryant.c bryant.h bryantPrint.c var.h Makefile
GENFILES=$(TESTOS) $(OS) $(SOOS)

RM=rm -f
MV=mv
MKDIR=mkdir -p
DEBUG=-g -DDEBUGALL
OPTIMIZE=-O3 -DNDEBUG -funroll-loops
CC=gcc
BASECFLAGS     = -Wall -Wshadow -Wcast-qual -Wcast-align -Wpointer-arith \
                 -Wstrict-prototypes -Wmissing-declarations -Winline \
                 -pedantic
LIBS=
SOOPTS=-fpic -DSWI


# General options

CHOSENOPTIM=$(OPTIMIZE)
#CHOSENOPTIM=$(DEBUG)

OPTS=-DCLEAR_CACHES -DCOMPUTED_TABLE -DEQUAL_TEST


# Main makefile

CFLAGS=$(BASECFLAGS) $(CHOSENOPTIM) $(OPTS) $(RSET) $(MSGS) $(EXTRAOPTS)

all: $(OS) $(SO)

so:	$(SO)

bryantPrint.o: bryantPrint.c bryant.h

bryant.o: bryant.c bryant.h

swi_robdd.o:	swi_robdd.c bryant.h
	$(CC) -c -I $(SWIDIR)/include $(SOOPTS) -o $@ swi_robdd.c

swi_bryantPrint.o:	bryantPrint.c bryant.h
	$(CC) -c -I $(SWIDIR)/include $(SOOPTS) -o $@ bryantPrint.c

Makefile.SWI:
	swipl -q -g 'file_search_path(swi,Dir), \
		     current_prolog_flag(shared_object_extension,So), \
		     format("SWIDIR=~w~n", [Dir]), \
		     format("SHAREDEXT=~w~n", [So]), \
		     halt.' > $@

$(SO): $(SOOS)
	gcc -shared -o $@ $(CFLAGS) $(SOOPTS) $(SOLIBS) $(SOOS)

$(SOOS):	 bryant.h

clean:
	- $(RM) $(GENFILES) Makefile.SWI

tidy:	clean
	- $(RM) $(TESTS) $(SO)
	- $(RM) *~
