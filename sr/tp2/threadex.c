#include<stdio.h> 
#include<stdlib.h>
#include<pthread.h>


void *threadTest(void * arg)
{
int i=0;
printf("Thread %s, Mon TID est : %d le PID du processus est : %d la valeur de ma variable est : %d\n ", (char*)arg, pthread_self(), getppid(), i);



}




int main()
{
pthread_t th1,th2,th3;
pthread_create(&th1, NULL, threadTest,"1");
pthread_create(&th2, NULL, threadTest,"2");
pthread_create(&th3, NULL, threadTest,"3");
(void)pthread_join(th1, NULL);
(void)pthread_join(th2, NULL);
(void)pthread_join(th3, NULL);
}
