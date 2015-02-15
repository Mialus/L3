#include<stdio.h> 
#include<stdlib.h>

int main()
{

	int i,j;
	
	for(i=0;i<4; i++)
	{
		for(j=0; j<1000000000; j++)
		{
			if(j==0)
			{
				printf("processus1\n");

			}
		}
	}
}
