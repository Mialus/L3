#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int main()
{

 char mess_recu[BUFSIZ],mess_envoi[BUFSIZ];
 int pp,d_sock,cc,l;
 int longe,service,tailleFile;
 char i;

  struct sockaddr_in serveur = {AF_INET};
  struct sockaddr client;

 serveur.sin_port=7658;
 serveur.sin_family=AF_INET;
 serveur.sin_addr.s_addr=INADDR_ANY;
 d_sock=socket(AF_INET,SOCK_STREAM,0);

 pp=bind(d_sock,(struct sockaddr*)&serveur,sizeof(serveur));
 printf("serveur pret\n");

 l=listen(d_sock,1);
 longe=sizeof(struct sockaddr_in);
 service=accept(d_sock,(struct sockaddr *)&client,&longe);
 printf("accept= %d\n",service);

 read(service,mess_recu,sizeof(mess_recu));
 printf("le serveur à recu : %s\n",mess_recu);

 FILE *fsortie=fopen(mess_recu, "w");

 read(service,mess_recu,sizeof(mess_recu));
 printf("le serveur à recu : %s\n",mess_recu);

 for(tailleFile=0;tailleFile<strlen(mess_recu);tailleFile++){
	if((mess_recu[tailleFile]>='A')&&(mess_recu[tailleFile]<='Z'))
	{
	i=((mess_recu[tailleFile]-'A')+(26-5))%26+'A';
	}
	fputc(i,fsortie);
 }
 close(d_sock);
 fclose(fsortie);
}
