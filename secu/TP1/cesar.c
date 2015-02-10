#include <stdio.h>
#include <stdlib.h>
#define CHIFFREMENT 1
#define DECHIFFREMENT 2
#define DECRYPTAGE 3
#define QUIT 0
#define NBR 2
#define TAILLE 256


//fonction qui réalise le chiffrement par décalage
void Chiffre(char nomFichier[NBR][TAILLE], int indice){
char i;

FILE *f=fopen(nomFichier[0], "r");
FILE *fsortie=fopen(nomFichier[1], "w");
while ((i = fgetc(f)) != EOF) {
	if((i>='A')&&(i<='Z'))
	{
	i=((i-'A')+indice)%26+'A';
	}
	
	fputc(i,fsortie);
}
fclose(f);
fclose(fsortie);
}

//fonction qui réalise le déchiffrement par décalage
void DeChiffre(char nomFichier[NBR][TAILLE], int indice){
char i;

FILE *f=fopen(nomFichier[0], "r");
FILE *fsortie=fopen(nomFichier[1], "w");
while ((i = fgetc(f)) != EOF) {
	if((i>='A')&&(i<='Z'))
	{
	i=((i-'A')+(26-indice))%26+'A';
	}
	
	fputc(i,fsortie);
}
fclose(f);
fclose(fsortie);
}

char readChar()
{
	char out;
	scanf("%c", &out);
	char c = 'a';
	while(c != '\n' && c!= EOF)
	{
		c = getchar();
	}
	return out;
}

//fonction qui réalise le décryptement
void DeCrypte(char nomFichier[NBR][TAILLE]){
char i,j,k,test2,indice;
int test;
fpos_t ma_pos;

FILE *f=fopen(nomFichier[0], "r");
FILE *fsortie=fopen(nomFichier[1], "w");
fgetpos(f,&ma_pos);//sauvegarde la position du fichier
test2=0;
indice=0;
while(test2==0)
{
test=0;

	while (((i = fgetc(f)) != EOF)&&(test<30)) {

		if((i>='A')&&(i<='Z'))
		{
		i=((i-'A')+(26-indice))%26+'A';
		}
	
		printf("%c",i);
		test++;
	
	}
printf("\nIndice : %d\nCela vous convient-il ?(y/n)\n",indice);
k=readChar();
	if(k=='y')
	{
	test2=1;
	}
indice++;
fsetpos(f,&ma_pos);//remet le fichier au début
}
indice--;
while ((i = fgetc(f)) != EOF) {
	if((i>='A')&&(i<='Z'))
	{
	i=((i-'A')+(26-indice))%26+'A';
	}
	
	fputc(i,fsortie);
}
fclose(f);
fclose(fsortie);
}


int main(int argc, char *argv[]) {

char nomFichier[NBR][TAILLE];
int indice,choix;


 
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
	printf("Veuillez donner l'indice de décalage : \n");
	scanf("%d",&indice);
	printf("Veuillez donner le nom du fichier de sortie : \n");
	scanf("%s",&nomFichier[1]);

	Chiffre(nomFichier,indice);

	break;

	case DECHIFFREMENT:
	printf("Veuillez donner le nom du fichier à déchiffrer : \n");
	scanf("%s",&nomFichier[0]);
	printf("Veuillez donner l'indice de décalage : \n");
	scanf("%d",&indice);
	printf("Veuillez donner le nom du fichier de sortie : \n");
	scanf("%s",&nomFichier[1]);

	DeChiffre(nomFichier,indice);

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
