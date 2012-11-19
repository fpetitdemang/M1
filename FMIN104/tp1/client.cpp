#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>



using namespace std;

struct msgR{//structure d'une requete
	long etiq;
	double op1;
	double op2;
	char operation;
	int pid;
};

struct msgRep{//structure d'une reponse
	long etiq;
	double rep;
};

/*void afficherMsgR(msgR msg){
	cout<<"Reponse : "<<msg.etiq<<endl;
}*/

int main(){

	key_t cles = ftok("../fichier",10);//calcul de la cles de la file
	if (cles < 0)
	{
		fprintf(stderr, "Erreur creation cles.\n");
		exit(1);
	}	
	
	int f_id;
	do{
		f_id = msgget(cles,0666);
	}
	while(f_id < 0);//tant que la file n'est pas cree, boucle

	msgR requete;
	msgRep reponse;
	int pid = getpid();
	
	//teste bloquage file
	/*requete.op1=1;
	requete.op2=4;
	requete.etiq = '-';
	requete.pid=pid;
	int compt = 0;*/

	while(1){
		//construction requete
		cout<<"operande 1?"<<endl;
		cin>>requete.op1;
		cout<<"operande 2?"<<endl;
		cin>>requete.op2;
		requete.etiq = '-';
		requete.pid = pid;

		//depose requete dans la file
		cout<<"Le processus "<<getpid()<<" depose la requete"<<endl;
		msgsnd(f_id,&requete,sizeof(msgR) - sizeof(long),0);
		
		//teste bloquage de la file
		/*compt++;
		cout<<compt<<endl;*/

		//extraction reponse de la file
		cout<<"Attente reponse"<<endl;
		msgrcv(f_id,&reponse,sizeof(msgRep) - sizeof(long), (long)pid, 0);

		cout<<"Resultat : "<<reponse.rep<<endl;*/
	}
	
}
