CC = gcc
OPT = # -O
DEB = -g
CFLAGS = $(OPT) $(DEB)
LFLAGS =

OBJ = main.o
SRC = main.c
DEP =

main: $(OBJ) $(DEP) $(SRC)
	$(CC) $(LFLAGS) $(OBJ) ../p2clib.a -o main

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.c: %.pas $(DEP)
	./../p2c $<

all: main
	./main commandLineTextAll

run: main
	./main helloColin commandLineTextRun

.PHONY: clean
clean:
	rm -f -v *.~*
	rm -f -v *.*~
	rm -f -v *~
	rm -f -v *.o
	rm -f -v *.c
	rm -f -v main
