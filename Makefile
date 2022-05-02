# Makefile for "p2c", the Pascal to C translator.
SHELL = /bin/sh

# Compiler options
CC = gcc
OPT = # -O
DEB = # -g
DEFS =  -DTEST_MALLOC
CFLAGS = $(OPT) $(DEB) -DTEST_MALLOC
LFLAGS =

# File names
OBJS = main.o stuff.o out.o comment.o lex.o parse.o decl.o expr.o pexpr.o funcs.o dir.o

# all
all: p2c p2clib.a

# p2c - app
p2c: $(OBJS)
	$(CC) $(LFLAGS) $(OBJS) -o p2c

dir.o: dir.c main.h
	$(CC) -c $(CFLAGS) dir.c

main.o: main.c main.h
	$(CC) -c $(CFLAGS) -DHASDUMPS main.c

# libp2c - runtime library
p2clib.a: p2clib.o
	ar r p2clib.a p2clib.o

p2clib.o: p2clib.c
	$(CC) -c $(CFLAGS) p2clib.c

clean:
	rm -f -v p2c 
	rm -f -v *.sa
	rm -f -v *.a
	rm -f -v *.o
	rm -f -v *.~*
	rm -f -v *.*~
