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


void info_process(void)
{
    printf("Je suis le processus %d\n", (int) getpid());
}

void traitement(int i)
{
	printf("J'effectue le traitement %d\n",i);
	sleep(rand()%5);
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
struct sembuf opP[] = {
  {(u_short) 0, (short)-2, 0},//operations P2(S1)
  {(u_short) 1, (short)-2, 0},//operations P2(S2)
  {(u_short) 2, (short)-2, 0},//operations P2(S3)
  {(u_short) 3, (short)-2, 0} //operations P2(S4)
};

struct sembuf opV[] = {
  {(u_short) 0, (short)+3, 0},//operations V3(S1)
  {(u_short) 1, (short)+3, 0},//operations V3(S2)
  {(u_short) 2, (short)+3, 0},//operations V3(S3)
  {(u_short) 3, (short)+3, 0} //operations V3(S4)
};

int main(){

	//calcul de la cles
	key_t cles = ftok("../fichier",51);
	if (cles < 0)
	{
		fprintf(stderr, "Erreur recuperation de la cles.\n");
		exit(1);
	}	

	//Boucle tant que l'enssemble de Semaphores pas créés
	int sem_id;
	do{
	sem_id = semget(cles,2,0666);//Attention aux droit -> traitement2 qui detruit semaphore
	}while(sem_id < 0);

	info_process();

	for (int i=0; i < 4; i++)
	{
		semop(sem_id,opP+i,1);//P2(Si)
		traitement(i);
		semop(sem_id,opV+i,1);//V3(Si)
	}	

	//Destruction du semaphore
	int res = semctl(sem_id, 0, IPC_RMID);
	if (res >= 0)
	{
		printf("Suppression sémaphore\n");
	}else{
		printf("Erreur suppression sémaphore");
	}
	
	return 0;

}