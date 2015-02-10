all: scrolling

scrolling: main.o  CBigCharacterPainter.o \
		   CBigCharacterPainterDefault.o

	g++ main.o CBigCharacterPainter.o \
		   CBigCharacterPainterDefault.o \
		   -o scrolling

main.o: main.cpp
	g++ -c main.cpp

CBigCharacterPainter.o: CBigCharacterPainter.cpp
	g++ -c CBigCharacterPainter.cpp

CBigCharacterPainterDefault.o: CBigCharacterPainterDefault.cpp
	g++ -c CBigCharacterPainterDefault.cpp

clean:
	rm -rf *.o scrolling

