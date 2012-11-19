#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <string.h>


#define nbProcessus 2



using namespace std;


void info_process(void)
{
    printf("Je suis le processus %d\n", (int) getpid());
}

//Structure pour initialiser un
//enssemble de semaphore
union semun{
	int val;
	struct semid_ds *bf;
	unsigned short *m_array;
	struct seminfo *__buf;
};

//Declare operation sur le semaphore
struct sembuf op[] = {
  {(u_short) 0, (short)-1, 0},//operations P(S1)
  {(u_short) 0, (short)0, 0} //operations Z(S)
};

int main(){

	//calcul de la cles
	key_t cles = ftok("../fichier",40);
	if (cles < 0)
	{
		fprintf(stderr, "Erreur recuperation de la cles.\n");
		exit(1);
	}	

	//creation semaphore ou verifie l'existance du semaphore associé à la cles
	int sem_id = semget(cles,2,0666 | IPC_CREAT | IPC_EXCL );
	if (sem_id < 0)
	{
		printf("Semaphore deja creer\n");

		sem_id = semget(cles,2,0666);
		if(sem_id < 0){
			fprintf(stderr, "Erreur récupération du Semaphore.\n");
			exit(1);
		}
	}else{
		printf("Creation Semaphore\n");
		//intialise le semaphore
		short unsigned int sarray[1]={nbProcessus};
		semun init;
		init.m_array = sarray;
		
		semctl(sem_id, 0, SETALL, init);
	}

	info_process();
	semop(sem_id,op,1);//P(S1)
	semop(sem_id,op+1,1);//Z(S)

	//detruit le semaphore
	int res = semctl(sem_id, 0, IPC_RMID);
	if (res >= 0)
		printf("Suppression sémaphore\n");
	
	
	return 0;

}