CC=gcc
CFLAGS=-g
SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)
EXEC=exo2

all: $(EXEC)

$(EXEC): $(OBJ)
	$(CC) -o $@ $(OBJ) $(CFLAGS)

%.o: %.c %.h
	$(CC) -o $@ $(CFLAGS) -c $<

clean:
	rm *.o; rm $(EXEC)
