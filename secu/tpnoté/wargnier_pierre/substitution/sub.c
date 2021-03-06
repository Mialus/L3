#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define CHIFFREMENT 1
#define DECHIFFREMENT 2
#define DECRYPTAGE 3
#define QUIT 0
#define NBR 2
#define TAILLE 256


void del_char(char* str, char c) //Enleve tous les c de str sauf le premier
{
   int id_read, id_write, id_nbr;
   id_read = 0;
   id_write = 0;
   id_nbr = 0;

   while (str[id_read] != '\0')
   {
      if (str[id_read] != c)
      {
          str[id_write] = str[id_read];
          id_write++;
      }
      if ((str[id_read] == c)&&(id_nbr==0))
      {
          str[id_write] = str[id_read];
          id_write++;
	  id_nbr=1;
      }
      id_read++;
    }
    str[id_write] = '\0';
}

//fonction qui réalise le chiffrement
void Chiffre(char nomFichier[NBR][TAILLE], char* motclef){
char i;
int k,j,l,n,val;

j=strlen(motclef);
for(l=0;l<j;l++){
	for(k=0;k<j;k++){
		if((k!=l)&&(motclef[l]==motclef[k])){
		del_char(motclef,motclef[k]);    
		}
	}
}
j=strlen(motclef);
int clef[26];
for(l=0;l<j;l++){
clef[l]=(motclef[l]-'A');
}
for(l=0;l<(26-j);l++){
val=0;
	for(n=0;n<j;n++){
	if(l==(motclef[n]-'A')){
	val=1;
	}
	}
	if(val==0){
	clef[l]=l;
	}
}
l=0;
FILE *f=fopen(nomFichier[0], "r");
FILE *fsortie=fopen(nomFichier[1], "w");
while((i=fgetc(f))!=EOF) 
{
	if(i>='A'&&i<='Z') 
	{
	i= (i + motclef[k]-'A')%26+'A';
	i = (i%26 + 26)%26+'A';  
		if(k==25) 
		{
		k=0;
		}
		else 
		{
		k++;
		}
	}
fputc(i,fsortie);
}
}

//fonction qui réalise le Déchiffrement
void DeChiffre(char nomFichier[NBR][TAILLE], char* motclef){
char i;
int k,j,l,n,val;
j=strlen(motclef);
for(l=0;l<j;l++){
	for(k=0;k<j;k++){
		if((k!=l)&&(motclef[l]==motclef[k])){
		del_char(motclef,motclef[k]);    
		}
	}
}
j=strlen(motclef);
int clef[26];
for(l=0;l<j;l++){
clef[l]=(motclef[l]-'A');
}
for(l=0;l<(26-j);l++){
val=0;
	for(n=0;n<j;n++){
	if(l==(motclef[n]-'A')){
	val=1;
	}
	}
	if(val==0){
	clef[l]=l;
	}
}
l=0;
FILE *f=fopen(nomFichier[0], "r");
FILE *fsortie=fopen(nomFichier[1], "w");
while((i=fgetc(f))!=EOF) 
{
	if(i>='A'&&i<='Z') 
	{
	i= (i - motclef[k]-'A')%26+'A';
	i = (i%26 + 26)%26+'A';  
		if(k==25) 
		{
		k=0;
		}
		else 
		{
		k++;
		}
	}
fputc(i,fsortie);
}
}

//fonction qui réalise le Décryptage
void DeCrypte(char nomFichier[NBR][TAILLE]){
FILE *f=fopen(nomFichier[0],"r");
char motclef[50],let,tab3[26];
int i,j,k,dec,pos,nbr,inde,indc,tab2[26];
float ref[50],refl,tab[50][26];
float refmoy;

refmoy=0;
dec=1;
pos=0;
nbr=0;
inde=0;
for(i =0;i<50;i++) 
{
	for(j= 0;j<26;j++) 
	{
	tab[i][j]=0;    
	}
}
  
for(i=0;i<50;i++) 
{
ref[i]=0;
}

while((let=fgetc(f))!=EOF)
{
	if(let>='A'&&let<='Z')
	{
	nbr++;
	}
}
  
i = 0;
rewind(f);
refmoy= 0;
pos=0;

while(i <50 &&(refmoy<0.075 || refmoy>0.085)) 
{
	refmoy=0;
	for(k=0;k<dec;k++)
	{
		while((let=fgetc(f))!=EOF)
		{
			if((let>='A')&&(let<= 'Z'))
			{
				if(dec != 1)
				{
					if((pos-k)%(dec)-1 == 0 && pos>=k)
					{
					tab[i][let-'A']++;
					}
				}
				else
				{
					if(pos%dec== 0)
					{
					tab[i][let-'A']++;
					}
				}
				pos++;
			}
		}
		for(j=0;j<26;j++)
		{
		refl=((tab[i][j]*(tab[i][j]-1))/((nbr/dec)*((nbr/dec)-1)));
		ref[k]=ref[k]+refl;
		}
	pos=0;
	}
	for(j=0;j<dec;j++)
	{
	refmoy=refmoy+ref[j];
	}
refmoy=refmoy/ (float)dec;
	for(j=0;j<50;j++) 
	{
	ref[j]=0;
	}
dec++;
i++;
rewind(f);
}


indc=16;
printf("La clef est : ");
int tabClef[indc];
for(i=0;i<26;i++) 
{
tab2[i] = 0;    
}
	
for(j=0;j<indc;j++)
{
tabClef[j] = 0;
}
	

for(k=0;k<indc;k++)
{
pos=0;
	while((let=fgetc(f)) != EOF)
	{
		if(let>='A' && let<='Z')
		{
			if((pos-k)%(indc)== 0 && pos>=k)
			{
			tab2[let-'A']+= 1;
			}
		pos++;
		}
	}
	for(j=0;j<26;j++)
	{
		if(inde<tab2[j])
		{
		inde=tab2[j];
		}
	}
	for(j=0;j<26;j++)
	{
		if(tab2[j]==inde)
		{
		inde=j;
		}	
	}
	for(j=0;j<26;j++)
	{
	tab2[j]=0;
	}
tabClef[k]=inde;
inde=0;
rewind(f);
}
	
char Clef[indc];
	for(k=0;k<indc;k++){
	Clef[k]=((tabClef[k]+'A'-'E')+'A');
	}
	for(k=0;k<indc;k++)
	{
	printf("%c",Clef[k]);
	}

printf("\nVeuillez donner le mot-clef : \n");
scanf("%s",&motclef);
DeChiffre(nomFichier,motclef);

}


int main(int argc, char *argv[]) {

char nomFichier[NBR][TAILLE];
int choix;
char motclef[50];


 
do {
printf("pour chiffrer un texte, tapez 1 \n");
printf("pour déchiffrer un texte, tapez 2 \n");
printf("pour décrypter un texte, tapez 3 \n");
printf("sinon, tapez 0 \n");
scanf("%d",&choix);

switch(choix){

	case CHIFFREMENT:

	printf("Veuillez donner le nom du fichier à chiffrer : \n");
	scanf("%s",&nomFichier[0]);
	printf("Veuillez donner le mot-clef : \n");
	scanf("%s",&motclef);
	printf("Veuillez donner le nom du fichier de sortie : \n");
	scanf("%s",&nomFichier[1]);

	Chiffre(nomFichier,motclef);

	break;

	case DECHIFFREMENT:
	printf("Veuillez donner le nom du fichier à déchiffrer : \n");
	scanf("%s",&nomFichier[0]);
	printf("Veuillez donner le mot-clef : \n");
	scanf("%s",&motclef);
	printf("Veuillez donner le nom du fichier de sortie : \n");
	scanf("%s",&nomFichier[1]);

	DeChiffre(nomFichier,motclef);

	break;

	case DECRYPTAGE:
	printf("Veuillez donner le nom du fichier à décrypter : \n");
	scanf("%s",&nomFichier[0]);
	printf("Veuillez donner le nom du fichier de sortie : \n");
	scanf("%s",&nomFichier[1]);

	DeCrypte(nomFichier);

	break;
	
	case QUIT:
	return 0;

}
}while(QUIT==0);
}
