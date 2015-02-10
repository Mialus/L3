#include <stdio.h>
#include <stdlib.h>
#define TRUE 1;
#define FALSE 0;


void triBulle(int t[], int const taille)
{
int j   = 0;
int tmp = 0;


int pasrange = TRUE;
	while (pasrange)
	{

		pasrange = FALSE;

		for (j = 0; j < taille-1; j++)
		{
			if(t[j] > t[j+1])
			{
 				tmp = t[j+1];
 				t[j+1] = t[j];
 				t[j] = tmp;
				pasrange = TRUE;
 			}
		}
	}
}
