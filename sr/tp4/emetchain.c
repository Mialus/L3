#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>

extern int errno;

#define NAME "/tmp/socket_254"
#define CHAINE "Demain, Secu !"

main()
{
	int sock, err;

	struct sockaddr_un nom;

	sock=socket(AF_UNIX, SOCK_DGRAM, 0);
	if (sock< 0)
	{
	perror("Erreur a la création de socket");
	exit(0);
	}

	nom.sun_family = AF_UNIX;
	strcpy(nom.sun_path, NAME);

	err = sendto(sock, CHAINE, (strlen(CHAINE)+1), 0, (struct sockaddr *)&nom, sizeof(nom));
	if (err != strlen(CHAINE)+1)
	{
	perror("Erreur sur le sendto");
	exit(0);
	}
	
	close(sock);
}
