#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <string.h>
#include <pthread.h>


using namespace std;

int n = 3;//nombre de taches lancer
pthread_mutex_t monVerrou = PTHREAD_MUTEX_INITIALIZER;//creation+initialisation verrou
pthread_cond_t varCond = PTHREAD_COND_INITIALIZER;//creation+initialisation variable conditionnelle


void info_thread(){
	cout<<"je suis le thread "<<pthread_self()<<endl;
}


void *f1(void *p){
	info_thread();
	//sleep(1);
	pthread_mutex_lock(&monVerrou);//prend le verrou
	n--;
	if (n==0) {
		//cas ou c'est la derniere tache
		//libere tous les threads bloqués
		pthread_cond_broadcast(&varCond);
	}else{
		//attend la derniere tache 
		//+ libere verrou en attendant 
		//+ recupere le verrou a la fin après wait
		pthread_cond_wait(&varCond,&monVerrou);
	}
	pthread_mutex_unlock(&monVerrou);//libere le verrou
	pthread_exit(NULL);

	//ajouter msg a la fin pour voir que les thread se reveille en meme temps
}



int main(){
	
	//creation des n taches
	for (int i=0; i<n; i++){
		pthread_t t;	
		pthread_create(&t,NULL,f1,NULL);
	}
	
	//attend la fin des taches
	//boucle qui initialise join
	sleep(10);
	return 0;
}