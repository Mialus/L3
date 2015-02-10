#include <stdlib.h>
#include <stdio.h>
#include <string.h>


char* concatAvecStringH(char* ch1,char* ch2)
{
	char* ch3 = malloc(24*sizeof(char));
	if(strlen(ch1)<strlen(ch2))
	{
	strcat(ch3,ch1);
	strcat(ch3,ch2);
	}else
	{	
	strcat(ch3,ch2);
	strcat(ch3,ch1);
	}
return ch3;
}

char* concatSansStringH(char* ch1,char* ch2)
{
	int i1=0;
	int i2=0;
	char* debutch = malloc(12*sizeof(char));
	char* debutch2 = malloc(12*sizeof(char));
	char* ch3 = malloc(24*sizeof(char));
	
	debutch=ch1;
	debutch2=ch2;
	while (*ch1 !='\0')
	{
	i1++;
	ch1++;
	}
		while (*ch2 !='\0')
		{
		i2++;
		ch2++;
		}
			if(i1<i2)
			{
			ch1=debutch;
			ch2=debutch2;
			i1=0;
				while (*ch1 !='\0')
				{
				ch3[i1]=*ch1;
				ch1++;
				i1++;
				}
					while (*ch2 !='\0')
					{
					ch3[i1]=*ch2;
					ch2++;
					i1++;
					}
			}		else
					{
					i2=0;
						ch1=debutch;
						ch2=debutch2;
						while (*ch2 !='\0')
						{
						ch3[i2]=*ch2;
						ch2++;
						i2++;
						}
							while (*ch1 !='\0')
							{
							ch3[i2]=*ch1;
							ch1++;
							i2++;
							}
					}
return ch3;
}

int main()
{
	char* ch = malloc(12*sizeof(char));
	char* ch2 = malloc(12*sizeof(char));
	char* ch3 = malloc(24*sizeof(char));
	printf("Veuillez entrer la premiére chaine de caractere\n");
	scanf("%s",ch);
	printf("Veuillez entrer la deuxieme chaine de caractere\n");
	scanf("%s",ch2);

	ch3=concatAvecStringH(ch,ch2);
	printf("Avec concatAvecStringH : %s\n",ch3);
	ch3=concatSansStringH(ch,ch2);
	printf("Avec concatSansStringH : %s\n",ch3);

}
