#include<stdio.h> 
#include<stdlib.h>

int main()
{
	int i,j;
int n=fork();

if(n==0)
{

	
	for(i=0;i<4; i++)
	{
		for(j=0; j<1000000000; j++)
		{
			if(j==0)
			{
				
			printf("processus num %d\n",getpid());
			system("ps");
			printf("mon pere a le num %d\n",getppid());

			}
		}
	}
}
else
{

	
	for(i=0;i<4; i++)
	{
		for(j=0; j<1000000000; j++)
		{
			if(j==0)
			{
			printf("chez le pere\n");
			printf("mon numero est %d\n",getpid());

			}
		}
	}
}
wait();
}
