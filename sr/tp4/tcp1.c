#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <netdb.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>

#define NAME "/tmp/socket_254"

main(){
int sock, msgsock, rval;
	struct sockadir_un server;
	char buf[1024];
	sock=socket(AF_UNIX, SOCK_STREAM, 0);
	
	if(sock<0){
	perror("opening stream socket");
	exit(1);
	}

	server.sun_family = AF_UNIX;
	strcpy(server.sun_path, NAME);
	if(bind(sock, (struct sockaddr *) &server, sizeof(struct sockaddr_un))) {
	perror("binding stream socket");
	exit(1);
	}

	printf("Socket has name %s\n", server.sun_path);
	listen(sock, 5);
	for(;;){
	msgsock = accept(sock, 0, 0);
	do {
	bzero(buf, sizeof(buf));
	rval = read(msgsock, buf, 1024);
	if(rval==0) printf("ending connection\n");
	else printf("-->%s\n", buf);
	}while(rval>0);
	close(msgsock);
	}
close(sock);
unlink(NAME);
}
