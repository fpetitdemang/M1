#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <string.h>


using namespace std;

//Declare operation sur les semaphores
struct sembuf op[] = {
  {(u_short) 0, (short)+1, 0},//operations V(S1)
  {(u_short) 0, (short)-1, 0} //operations P(S1)
};

struct sembuf op2[] = {
  {(u_short) 1, (short)0, 0},// rdv S2	
  {(u_short) 1, (short)+2, 0},// operation V(S2)
};

union semun{
	int val;
	struct semid_ds *bf;
	unsigned short *array;
	struct seminfo *__buf;
};


int main(){
  
	int nbProcessusEmploye = 2;

	//calcul de la cles
	key_t cles = ftok("../fichier",10);
	if (cles < 0)
	{
		fprintf(stderr, "Erreur recuperation de la cles.\n");
		exit(1);
	}	
	
	//creation et identification d'un segment
	int sh_id;
	sh_id = shmget(cles,size_t(256*sizeof(char)),IPC_CREAT|0666);

	if (sh_id < 0)
	{
		fprintf(stderr, "Erreur creation du segment.\n");
		exit(1);
	}	

	//demande d'attachement 
	char * att;	
	if(!(att = (char *)shmat(sh_id, NULL, 0)))
	{
		fprintf(stderr, "Erreur demande d'attachement.\n");
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
		//intialise les semaphores
		int sarray[2]={0,nbProcessusEmploye};
		semctl(sem_id, 0, SETALL, sarray);
	}
	
	char msg[255];
	do{
	printf("Saisir message : \n");
	scanf("%s", msg);

	if( (strcpy(att,msg)) > 0)//ecriture sur le segment
	  cout<<"j'ai ecrit : "<<msg<<endl;

	semop(sem_id,op,1);//V(S1)
	semop(sem_id,op2,2);//Z(S2) + V(S2)
	semop(sem_id,op+1,1);//P(S1)
	
	}while(1);
	
	
	return 0;
}