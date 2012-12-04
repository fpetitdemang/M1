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

#define PORT 38521


using namespace std;

void *thread_reception(void *p){
  
  int* pBr = (int*)p;
  do{
   
    char msgR[256];
    int reception = recv(pBr[0], msgR, 256, 0);
    if (reception < 0){
      perror("recv");
      close(pBr[0]);
      close(pBr[1]);
      cout<<"Fermeture serveur"<<endl;
      exit(1);
    }

    if(reception == 0){
      perror("send");
      close(pBr[0]);
      close(pBr[1]);
      cout<<"Fermeture serveur"<<endl;
      exit(1);
    }

    if(reception > 0) {
      cout<<"rcv : Success"<<endl;
      cout<<msgR<<endl;
    }
    

    int envoie = send(pBr[1], msgR, 256, 0);
    if (envoie < 0){
      close(pBr[0]);
      close(pBr[1]);
      perror("send");
    }
    if (envoie == 0){
      close(pBr[0]);
      close(pBr[1]);
      perror("send");
    }

    if(envoie > 0){
      cout<<"send : Success"<<endl;
      cout<<msgR<<endl;
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
    cout<<"Creation BR publique : Ok"<<endl;
  }else{
    perror("Creation BR publique");
    exit(1);
  }
  
  
  if (listen(descBrPub,1) < 0){
    perror("Creation liste attente");
    exit(1);
  }else{
    cout<<"Creation liste attente : Ok"<<endl;
  }

  struct sockaddr_in brCv;
  socklen_t lgbrCv = sizeof(struct sockaddr_in);
  

  //Traite les deux premieres demande de connexion
  int nbClientCo = 0;
  int TdescBrA[nbClientCo];
  int TdescBrB[nbClientCo];

  while(nbClientCo < 2){
    cout<<"Attend demande connexion"<<endl;
    int descBrCv = accept(descBrPub,(struct sockaddr*)&brCv,&lgbrCv);
    if (descBrCv < 0){
      perror("Traitement demande connexion");
      exit(1);
    }else{
      cout<<"Traitement demande connexion n°"<<nbClientCo + 1<<endl;
    }
    
    /* TdescBrA[brA:brB]
       TdescBrB[brB:brA]
    */
    if (nbClientCo == 0){
      TdescBrA[0] = descBrCv;
      TdescBrB[1] = descBrCv;
      
    }else{
      TdescBrA[1] = descBrCv;
      TdescBrB[0] = descBrCv;
      }
    
    nbClientCo = nbClientCo + 1;
    cout<<nbClientCo<<endl;
  }

  
  int tabIdThread[nbClientCo];

  //Créer thread de reception
  for (int i=0; i<nbClientCo; i++){
    pthread_t t1;	
    tabIdThread[i]=t1;

    if ( i == 0){
      pthread_create(&t1,NULL,thread_reception,TdescBrA);		   
    }else{
      pthread_create(&t1,NULL,thread_reception,TdescBrB);	
    }
  }


  
  //attend la fin des taches
  for(int i=0; i<nbClientCo; i++){
    pthread_join(tabIdThread[i],NULL);
  }
		
  return 0;
}
