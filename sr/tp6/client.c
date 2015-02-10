#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#define NBR 2
#define TAILLE 1024

void LireChaine(char chaine[], int size) {
    fgets(chaine, size, stdin);
    chaine[strlen(chaine) - 1] = '\0';
}

int main()
{
 struct sockaddr_in serveur;
 struct hostent *hp;

 char mess_recu[BUFSIZ], mess_envoi[BUFSIZ],ip[16],mess_envoi2[TAILLE];
 char nomFichier[NBR][TAILLE];
 int sock,errno,i,j,n,l,cc,d_sock,iFile,tailleFile,port;
 int longe,service;

 printf("Veuillez Entrer l'ip du serveur : \n");
 LireChaine(ip, 16);

 hp=gethostbyname(ip);
 memcpy(&serveur.sin_addr,hp->h_addr,hp->h_length);
 serveur.sin_port=7658;
 serveur.sin_family=AF_INET;

 sock=socket(AF_INET,SOCK_STREAM,0);
 l=connect(sock,(struct sockaddr *)&serveur,sizeof(serveur));

 printf("Veuillez donner le nom du fichier local à transférer : \n");
 scanf("%s",&nomFichier[0]);

 printf("Veuillez donner le nom du fichier distant qui contiendra le fichier : \n");
 scanf("%s",&mess_envoi);
 cc=write(sock,mess_envoi,sizeof(mess_envoi));
 
 FILE *f=fopen(nomFichier[0], "r");
 tailleFile=0;
 while ((iFile = fgetc(f)) != EOF) {
	if((iFile>='A')&&(iFile<='Z'))
	{
	i=((iFile-'A')+5)%26+'A';// l'indice est de 5
	}
	
	mess_envoi[tailleFile]=i;
 tailleFile++;
 }
 fclose(f);

 tailleFile--;
 mess_envoi[tailleFile]='\0';

 printf("le taille du fichier est de : %d\n", tailleFile);

 while(n>0){
 cc=write(sock,mess_envoi,sizeof(mess_envoi));
 
 if(cc != sizeof(mess_envoi)){
 perror("Erreur !");
 }
 printf("le client à envoyer au serveur : %s\n",mess_envoi);
 close(sock);
}
