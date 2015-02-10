#include <stdio.h>
#include <stdlib.h>
#include <time.h>

//bubble sort
void bubbleSort(int tab[20])
{
int i,j,aux;

for(i=0;i<20;i++)
{
	for(j=0;j<20;j++)
	{
		if(tab[j]>tab[j+1])
		{
		aux=tab[j];
		tab[j]=tab[j+1];
		tab[j+1]=aux;
		}
	}

}
}

insertionSort(int tab[20])
{
}


void sort(int tab[20])
{
char choix;
do
{
	printf("\nVeuillez choisir un algorithme :\n-Pour le Bubble sort, taper b\n-Pour l'insertion sort, taper i\n");
	scanf("%c",&choix);
	getchar();

	switch(choix){

		case 'b':
		bubbleSort(tab);
		break;

		case 'i':
		insertionSort(tab);
		break;

		default:
		printf("\nChoix non valide\n");
		break;
	}
}
while((choix!='b')&&(choix!='i'));
}

int main()
{
	int tab [20];
	int i;
	srand(time(NULL));
	for(i = 0; i<20; i++)
	{
		tab[i]= rand();
		printf("%d\n", tab[i]);
	}

	sort(tab);
	printf("\nApres sort !\n");

	for(i = 0; i<20; i++)
	{
		printf("%d\n", tab[i]);
	}

	return 0;
}
