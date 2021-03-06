#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#define NBR 2

//fonction qui permet d'écrire dans une chaine à partir d'une entrée
void LireChaine(char chaine[], int size) {
    fgets(chaine, size, stdin);
    chaine[strlen(chaine) - 1] = '\0';
}

int main()
{
 struct sockaddr_in serveur;
 struct hostent *hp;

 char mess_recu[BUFSIZ], mess_envoi[BUFSIZ],ip[16],buffer[1024];
 char nomFichier[NBR][BUFSIZ];
 int sock,errno,i,port,j,n,l,cc,d_sock,iFile,tailleFile;
 int longe,service;

//demande d'information et connection au serveur
 printf("Veuillez Entrer l'ip du serveur : \n");
 LireChaine(ip, 16);

 hp=gethostbyname(ip);
 memcpy(&serveur.sin_addr,hp->h_addr,hp->h_length);
 printf("Veuillez Entrer le port : \n");
 scanf("%d",&port);
 serveur.sin_port=port;
 serveur.sin_family=AF_INET;

 sock=socket(AF_INET,SOCK_STREAM,0);
 l=connect(sock,(struct sockaddr *)&serveur,sizeof(serveur));

//demande du nom du fichier
 printf("Veuillez donner le nom du fichier local à transférer : \n");
 scanf("%s",&nomFichier[0]);
//demande du nom sous lequel le fichier sera écris chez le serveur
 printf("Veuillez donner le nom du fichier distant qui contiendra le fichier : \n");
 scanf("%s",&mess_envoi);
//envoi du nom
 cc=write(sock,mess_envoi,sizeof(mess_envoi));
 
//ouverture et chiffrage du fichier
 FILE *f=fopen(nomFichier[0], "r");
 tailleFile=0;
 while ((iFile = fgetc(f)) != EOF) {
	i=iFile;
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
 j=0;
//on met la chaine dans le buffer
 while(mess_envoi[j]!='\0'){
 for(i=0;i<1024;i++){
 buffer[i]=mess_envoi[j];
 j++;
 }
//on envoi le buffer
 cc=write(sock,buffer,sizeof(buffer));
 if(cc != sizeof(buffer)){
 perror("Erreur !");
 }
}
 buffer[0]='\0';
//on envoi le caractère de fin de chaine
 cc=write(sock,buffer,sizeof(buffer));
 if(cc != sizeof(buffer)){
 perror("Erreur !");
 }
 printf("le client à envoyer au serveur : %s\n",mess_envoi);
 close(sock);
}
