#include <iostream>
#include <errno.h>
#include <sys/socket.h>
#include <stdio.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <strings.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>




#include "../Sock/sock.h"
#include "../Sock/sockdist.h"

#include <stdlib.h>

#define PORTS 38521

void *thread_reception(void *p){
  
  int pBrCli = (int )p;
  
  do{
    char msgR[256];
    int reception = recv(pBrCli, msgR, 256, 0);
    if (reception < 0){
      perror("recv");
      close(pBrCli);
      close(pBrCli);
      cout<<"Fermeture serveur"<<endl;
      exit(1);
    }
    
    if(reception == 0){
      perror("send");
      close(pBrCli);
      close(pBrCli);
      cout<<"Fermeture serveur"<<endl;
      exit(1);
    }
    
    if(reception > 0) cout<<"rcv : Success"<<endl;
    
    cout<<msgR<<endl;
  }while(1);
  
}

using namespace std;

int main(int argc, char *argv[]){
  
  int PORTC;
  printf("BR client : \n");
  cin>>PORTC;

  cout << "Client en route" << endl;
  cout << "===============\n" << endl;
  string nomHote = "localhost";
  char buffer[256]="salut";
  char buffer_rec[256];
  
  
  //Demande Boite Reseau Privée
  Sock brCli(SOCK_STREAM,PORTC,0);
  int descBrCli;
  if(brCli.good()){
    descBrCli = brCli.getsDesc();
    perror("Creation BR privee");  
  }else{
    perror("Creation BR privee");  
    exit(1);
  }
  
  //BR distante
  SockDist brPub(nomHote.c_str(), (short)PORTS);
  struct sockaddr_in *adrBrPub= brPub.getAdrDist();
  int lgAdrBrPub=sizeof(struct sockaddr_in);

  
  //Demande de connexion
  if (connect(descBrCli,(struct sockaddr *)adrBrPub, lgAdrBrPub) < 0){
    perror("Demande connexion");
    exit(1);
  }else{
    perror("Demande connexion");
  }
  
  /*thread reception*/
  pthread_t t1;
  pthread_create(&t1,NULL,thread_reception,(void*)descBrCli);		   
	
  
  do{

    char msgE[256];
    printf("Saisir message : \n");
    scanf("%s", msgE);
    
    int envoie = send(descBrCli, msgE, 256, 0);
    if (envoie < 0){
      close(descBrCli);
      close(descBrCli);
      perror("send");
    }
    if (envoie == 0){
      close(descBrCli);
      close(descBrCli);
      perror("send");
    }

    if(envoie > 0) cout<<"send : Success"<<endl;

    /***************/
    

  }while(1);
  
  
  return 0;
}

  
