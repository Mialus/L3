all: main

main: main.o Cportes.o Cfenetre.o Couverture.o Cmaison.o Cfacade.o Crectangle.o Ctriangle.o Cpignon.o
	g++ -c main.o Cportes.o Cfenetre.o Couverture.o Cmaison.o Cfacade.o Crectangle.o Ctriangle.o Cpignon.o

Crectangle.o: Crectangle.cpp 
	g++ -g -c Crectangle.cpp -o Crectangle.o

Ctriangle.o: Ctriangle.cpp 
	g++ -g -c Ctriangle.cpp -o Ctriangle.o

Cfenetre.o: Cfenetre.cpp 
	g++ -g -c Cfenetre.cpp -o Cfenetre.o

Cportes.o: Cportes.cpp 
	g++ -g -c Cportes.cpp -o Cportes.o

Couverture.o: Couverture.cpp 
	g++ -g -c Couverture.cpp -o Couverture.o

Cmaison.o: Cmaison.cpp 
	g++ -g -c Cmaison.cpp -o Cmaison.o

main.o: main.cpp 
	g++ -g -c main.cpp -o main.o

Cfacade.o: Cfacade.cpp 
	g++ -g -c Cfacade.cpp -o Cfacade.o

Cpignon.o: Cpignon.cpp 
	g++ -g -c Cpignon.cpp -o Cpignon.o

clean:
	rm *.o

mrproper: clean
	rm main
