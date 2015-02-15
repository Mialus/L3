CC=gcc
CFLAGS=-g
SRC=$(wildcard *.cpp)
OBJ=$(SRC:.cpp=.o)
EXEC=main

all: $(EXEC)

$(EXEC): $(OBJ)
	$(CC) -o $@ $(OBJ) $(CFLAGS)

%.o: %.cpp %.h
	$(CC) -o $@ $(CFLAGS) -c $<

clean:
	rm *.o; rm $(EXEC)
