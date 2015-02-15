#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

int main(int argc, char** argv){
int v;
if(fork()!=0)
{
	wait();
	if(fork()!=0){
	wait();	
	}
	else{
	v=execlp("ls", "ls", "-li", NULL);
		if(v==-1)
		{
		exit(0);		
		}
	}
}
else
{
	v=execlp("pwd", "pwd", NULL);
		if(v==-1)
		{
		exit(0);		
		}
}
return 0;
}
