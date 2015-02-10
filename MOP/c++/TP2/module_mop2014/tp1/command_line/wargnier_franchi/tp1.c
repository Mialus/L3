#include <stdio.h>
#include <stdlib.h>
#include <time.h>

//bubble sort
void sort(int tab[20])
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
