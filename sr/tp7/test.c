#include <stdio.h>
#include <sys/ipc.h>
#include <sys/shm.h>

main()
{
	char* pint;
	int shmid, i;

	shmid=shmget(205,4,0777|IPC_CREAT);
	pint = (char*)shmat(shmid,0,0);
	for(i=0;i<4;i++){
	printf("%c\n", *pint++);
	}
}
