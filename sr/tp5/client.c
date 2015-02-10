#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

void LireChaine(char chaine[], int size) {
    fgets(chaine, size, stdin);
    chaine[strlen(chaine) - 1] = '\0';
}

main()
{
struct sockaddr_in serveur,from;
struct hostent * hp;

char mess_recu[128], mess_envoi[128],mess_envoi2[128];
int sock,cc,cc3,cc2;
unsigned int from_size;

from_size=sizeof(from);
	hp=gethostbyname("172.20.128.186");
	
	memcpy((char*)&serveur.sin_addr,hp->h_addr,hp->h_length);

	sock=socket(AF_INET,SOCK_DGRAM,0);
	serveur.sin_family=AF_INET;
		serveur.sin_port=5825;
printf("Entrer le message\n");
while(1){
LireChaine(mess_envoi, 128);


cc=sendto(sock,mess_envoi,sizeof(mess_envoi),0,(struct sockaddr*)&serveur,sizeof(serveur));
cc3=recvfrom(sock,mess_recu,sizeof(mess_recu), 0, (struct sockaddr*)&from, &from_size);
printf("Lilian : %s\n",mess_recu);
}
}
