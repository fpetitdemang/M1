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



#include "../Sock/sock.h"
#include "../Sock/sockdist.h"

#include <stdlib.h>

#define PORTC 38523
#define PORTS 38519

using namespace std;

int main(int argc, char *argv[]){

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
  

  
  do{

    char msgE[256];
    printf("Saisir message : \n");
    scanf("%s", msgE);
    
    int envoie = send(descBrCli, msgE, strlen(msgE)+1, 0);
    if (envoie < 0){
      perror("Send");
    }else{
      perror("Send");
    }
    
    /*char msgR[256];
    int reception = recv(descBrCli, msgR, 256, 0);
    if (reception < 0){
      perror("Send");
    }else{
      perror("Send");
      }*/
    
  }while(1);
  
  
  return 0;
}

  
