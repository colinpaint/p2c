# Makefile for "p2c", the Pascal to C translator.
CC = gcc
OPT = # -O
DEB = -g
CFLAGS = $(OPT) $(DEB)
LFLAGS =

OBJ = main.o scan.o analysis.o traverse.o code.o list.o common.o
SRC = main.c scan.c analysis.c traverse.c code.c list.c common.c dumpToken.c #dumpInter.c dumpPseudo.c

DEP = common.def token.def inter.def pseudo.def \
      debug.def list.def main.def scan.def analysis.def traverse.def code.def

p68: $(OBJ) $(DEP) $(SRC)
	$(CC) $(LFLAGS) $(OBJ) ../p2clib.a -o p68

pmt: $(PMT_OBJ)
	$(CC) $(LFLAGS) pasmat.o ../p2clib.a -o pmt

dumpToken: dumpToken.o $(DEP)
	$(CC) $(LFLAGS) dumpToken.o common.o ../p2clib.a -o dumpToken

dumpInter: dumpInter.o $(DEP)
	$(CC) $(LFLAGS) dumpInter.o common.o ../p2clib.a -o dumpInter

dumpPseudo: dumpPseudo.o $(DEP)
	$(CC) $(LFLAGS) dumpPseudo.o common.o ../p2clib.a -o dumpPseudo

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.c: %.pas $(DEP)
	./../p2c -q $<

all: p68 dumpToken pmt

run: p68
	./p68 test.pas

.PHONY: clean
clean:
	rm -f -v *.~*
	rm -f -v *.*~
	rm -f -v *~
	rm -f -v *.o
	rm -f -v *.c
	rm -f -v *.sa
	rm -f -v *.ro
	rm -f -v p68
	rm -f -v dumpToken
	rm -f -v dumpInter
	rm -f -v dumpPseudo
	rm -f -v pmt
	rm -f -v *.log
	rm -f -v *.tmp
	rm -f -v formatted.pas
#
