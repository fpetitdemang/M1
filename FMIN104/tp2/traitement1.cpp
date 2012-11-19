#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <string.h>
#include <time.h>


using namespace std;

//Structure pour initialiser un
//enssemble de semaphore
union semun{
	int val;
	struct semid_ds *bf;
	unsigned short *m_array;
	struct seminfo *__buf;
};

//Declare operation sur le semaphore
struct sembuf opP[] = {
  {(u_short) 0, (short)-1, 0},//operations P1(S1)
  {(u_short) 1, (short)-1, 0},//operations P1(S2)
  {(u_short) 2, (short)-1, 0},//operations P1(S3)
  {(u_short) 3, (short)-1, 0} //operations P1(S4)
};

struct sembuf opV[] = {
  {(u_short) 0, (short)+2, 0},//operations V2(S1)
  {(u_short) 1, (short)+2, 0},//operations V2(S2)
  {(u_short) 2, (short)+2, 0},//operations V2(S3)
  {(u_short) 3, (short)+2, 0} //operations V2(S4)
};

void info_process(void)
{
	printf("Je suis le processus %d\n", (int) getpid());
	sleep(rand()%5);
}

void traitement(int i, int sem_id)
{
	printf("J'effectue le traitement %d\n",i);
	//Recuperation de la valeur de l'enssemble des semaphores
	semun sem_val;
	sem_val.m_array = (unsigned short*)malloc(sizeof(unsigned short)*4);
	semctl(sem_id, 0, GETALL, sem_val);
	cout<<sem_val.m_array[0];
}

int main(){

	//calcul de la cles
	key_t cles = ftok("../fichier",51);
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
		//On cree autant de semaphore que de traitement -> ici 4
		short unsigned int sarray[4]={1, 1, 1, 1};
		semun init;
		init.m_array = sarray;
		
		semctl(sem_id, 0, SETALL, init);
	}

	info_process();
	for (int i=0; i < 4; i++)
	{
		semop(sem_id,opP+i,1);//P1(Si)
		traitement(i, sem_id);
		semop(sem_id,opV+i,1);//V2(Si)
	}



	return 0;

}