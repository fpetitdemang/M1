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

int n = 4;//nombre de taches lancer
int nbZ = 4;//nombre de zone
int tabD[4];//initialisation tableau traitement zone par thread

pthread_mutex_t monVerrou = PTHREAD_MUTEX_INITIALIZER;//creation+initialisation verrou pour acces I/O tabD

//creattion variable conditionnnel
pthread_cond_t varCond[4]; 



void info_thread(){
  cout<<"je suis le thread "<<pthread_self()<<endl;
}

void traitement(int zone){
  info_thread();
  cout<<"je travaille sur la zone "<<zone<<endl;
}


void *f1(void *p){

  for (int i=0; i<nbZ; i++){
    //prend le verrou
    pthread_mutex_lock(&monVerrou);
    
    tabD[i]++;
    //signal que l'on modifie le tableau
    pthread_cond_signal(&varCond[i]);
    
    if (i >= tabD[i-1]) pthread_cond_wait(&varCond[i-1],&monVerrou);
    
    //rend verrou
    pthread_mutex_unlock(&monVerrou);

    traitement(i);
      }
}






	




int main(){

  int tabIdThread[n];


  //initialise les variables conditionnelles
  for(int i=0; i<n; i++){
    varCond[i]=PTHREAD_COND_INITIALIZER;
  }

  //intialise
  for (int i=0; i<n; i++){
    tabD[i]=0;
  }


  //creation des n thread
  for (int i=0; i<n; i++){
    pthread_t t;	
    tabIdThread[i]=t;
    pthread_create(&t,NULL,f1,NULL);
  }
  
  //attend la fin des taches
  for(int i=0; i<n; i++){
    pthread join(tabIdThread[i],NULL);
  }
  
  return 0;
}
