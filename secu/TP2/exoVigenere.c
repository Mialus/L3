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
int k,j,l;
j=strlen(motclef);
for(l=0;l<j;l++){
	for(k=0;k<j;k++){
		if((k!=l)&&(motclef[l]==motclef[k])){
		del_char(motclef,motclef[k]);    
		}
	}
}
j=strlen(motclef);
printf("%s\n",motclef);
int clef[j];
for(l=0;l<j;l++){
clef[l]=(motclef[l]-'A');
}

l=0;
FILE *f=fopen(nomFichier[0], "r");
FILE *fsortie=fopen(nomFichier[1], "w");
while ((i = fgetc(f)) != EOF) {
	if(l>(j-1)){
	l=0;
	}
	if((i>=65)&&(i<91))
	{
	i=((i-65)+clef[l])%26+65;
	}
	
	fputc(i,fsortie);
}
fclose(f);
fclose(fsortie);
}

//fonction qui réalise le Déchiffrement
void DeChiffre(char nomFichier[NBR][TAILLE], char* motclef)
{
char i;
int k,j,l;
j=strlen(motclef);
for(l=0;l<j;l++){
	for(k=0;k<j;k++){
		if((k!=l)&&(motclef[l]==motclef[k])){
		del_char(motclef,motclef[k]);    
		}
	}
}
j=strlen(motclef);
printf("%s\n",motclef);
int clef[j];
for(l=0;l<j;l++){
clef[l]=(motclef[l]-'A');
}

l=0;
FILE *f=fopen(nomFichier[0], "r");
FILE *fsortie=fopen(nomFichier[1], "w");
while ((i = fgetc(f)) != EOF) {
	if(l>(j-1)){
	l=0;
	}
	if((i>=65)&&(i<91))
	{
	i=((i-'A')+(26-clef[l]))%26+'A';
	}
	
	fputc(i,fsortie);
}
fclose(f);
fclose(fsortie);
}

//fonction qui réalise le Décryptage
void DeCrypte(char nomFichier[NBR][TAILLE])
{
char motclef[50];

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
