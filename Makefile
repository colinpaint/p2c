# Makefile for "p2c", the Pascal to C translator.
SHELL = /bin/sh

# Compiler options
CC = gcc
OPT = # -O
DEB = -g
CFLAGS = $(OPT) $(DEB)

# File names
OBJS = main.o stuff.o out.o comment.o lex.o parse.o decl.o expr.o pexpr.o funcs.o dir.o

all: p2c p2clib.a

rebuild:
	make clean
	make all -j8

# p2c - app
p2c: $(OBJS)
	$(CC) $(LFLAGS) $(OBJS) -o p2c

# libp2c - runtime library
p2clib.a: p2clib.o
	ar r p2clib.a p2clib.o

%.o: %.c main.h p2c.h
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f -v p2c
	rm -f -v *.sa
	rm -f -v *.a
	rm -f -v *.o
	rm -f -v *.~*
	rm -f -v *.*~
	rm -f -v *~
	rm -f -v .~*
