//============================================================================
// Name        : Serveur Tcp
// Author      : Petitdemange-Franck
// Version     : finale
// Copyright   : 
// Description : Programmation Serveur TCP de type itératif
//============================================================================

#include <iostream>
#include <errno.h>
#include <sys/socket.h>
#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <strings.h>
#include <string.h>
#include <pthread.h>

#include "../Sock/sock.h"
#include "../Sock/sockdist.h"

#include <stdlib.h>

#define PORT 38519	//num de la BR publique


using namespace std;

void *f1(void *pBr){
  
  do{
   
    char msgR[256];
    int reception = recv((int)pBr[0], msgR, 256, 0);
    if (reception < 0){
      perror("Send");
    }else{
      perror("Send");
    }

    int envoie = send((int)pBr[1], msgR, 256, 0);
    if (envoie < 0){
      perror("Send");
    }else{
      perror("Send");
    }

  }while(1);
}




int main(){

  cout<<"\n"<<endl;
  cout<<"Serveur en route"<<endl;
  cout<<"================\n"<<endl;
  
  
  //Creation BR publique
  Sock brPub(SOCK_STREAM,(short)PORT,0);
  int descBrPub;
  if(brPub.good()){
    descBrPub=brPub.getsDesc();
    perror("Creation BR publique");
  }else{
    perror("Creation BR publique");
    exit(1);
  }
  
  
  if (listen(descBrPub,1) < 0){
    perror("Creation liste attente");
    exit(1);
  }else{
    perror("Creation liste attente");
  }

  struct sockaddr_in brCv;
  socklen_t lgbrCv = sizeof(struct sockaddr_in);
  

  int nbClientCo = 0;
  //accepte 2 premiers clients
  while(nbClientCo < 2){
    cout<<"Attent demande connexion"<<endl;
    int descBrCv = accept(descBrPub,(struct sockaddr*)&brCv,&lgbrCv);
    if (descBrCv < 0){
      perror("Traitement demande connexion");
      exit(1);
    }else{
      perror("Traitement demande connexion");
    }
    
    TdescBrCv[nbClientCo];
    nbClientCo++;

  }

  int tabIdThread[nbClientCo];

  //creation thread
  pthread_t t1;	
  tabIdThread[0]=t1;
  pthread_create(&t1,NULL,f1,TdescBrCv);
		 
  int TdescBrCv_tmp = TdescBrCv[0];
  TdescBrCv[0] =   TdescBrCv[1];
  TdescBrCv[1] =   TdescBrCv_tmp;

  
  pthread_t t2;	
  tabIdThread[1]=t2;
  pthread_create(&t2,NULL,f1,TdescBrCv);
		   
    

  


  return 0;
}
