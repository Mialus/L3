#include <stdio.h>
#include <stdlib.h>
#include <time.h>


void sort()
{


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
		
	sort();
	printf("\nAprès sort !\n");
	for(i = 0; i<20; i++)
	{
		tab[i]= rand();
		printf("%d\n", tab[i]);
	}

	return 0;
}
