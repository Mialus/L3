#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int main()
{

 char buffer[1024],mess_recu[BUFSIZ],mess_recu2[BUFSIZ],mess_envoi[BUFSIZ],envoi[BUFSIZ];
 int pp,d_sock,port,cc,l;
 int longe,service,j,k,n,tailleFile;
 char i;

  struct sockaddr_in serveur = {AF_INET};
  struct sockaddr client;

//préparation du serveur 
 printf("Veuillez Entrer le port : \n");
 scanf("%d",&port);
 serveur.sin_port=port;
 serveur.sin_family=AF_INET;
 serveur.sin_addr.s_addr=INADDR_ANY;
 d_sock=socket(AF_INET,SOCK_STREAM,0);

 pp=bind(d_sock,(struct sockaddr*)&serveur,sizeof(serveur));
 printf("serveur pret\n");

 l=listen(d_sock,1);
 longe=sizeof(struct sockaddr_in);
 service=accept(d_sock,(struct sockaddr *)&client,&longe);
 printf("accept= %d\n",service);

//attente du nom du fichier du client

 read(service,mess_recu2,sizeof(mess_recu2));
 printf("le serveur à recu : %s\n",mess_recu2);

//création et ouverture du fichier
 FILE *fsortie=fopen(mess_recu2, "w");
k=0;

//reception, déchiffrage et écriture du fichier reçu sur le disque
 do{
 n=read(service,buffer,sizeof(buffer));
 strcat(mess_recu,buffer);
 
 
}while(n!=0);

 printf("le serveur à recu : %s\n",mess_recu);

 for(tailleFile=0;tailleFile<strlen(mess_recu);tailleFile++){
	i=mess_recu[tailleFile];
	if((mess_recu[tailleFile]>='A')&&(mess_recu[tailleFile]<='Z'))
	{
	i=((mess_recu[tailleFile]-'A')+(26-5))%26+'A';
	}
	fputc(i,fsortie);
 }
 close(d_sock);
 fclose(fsortie);
}
