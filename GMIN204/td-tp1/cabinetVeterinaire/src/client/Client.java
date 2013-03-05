package client;

import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import client.*;
import Interface.*;

public class Client {
	private Client() {
	}

	public static void main(String[] args) {

		String host = (args.length < 1) ? null : args[0];
		try {
			System.setSecurityManager(null);
			Registry registry = LocateRegistry.getRegistry(host);
			

			/*version-1*/
			//demande stub auprès de rmiregistry
			IAnimal animal_recup = (IAnimal) registry.lookup("chien");

			//recupere stub 
			IDossier dossier_recup = animal_recup.getDossier();
			


			// modifie le dossier sur le client
			dossier_recup.setObservation("tout vas bien");

			//recupere espece par copie//
			Espece espece_recup = animal_recup.getEspece();
			espece_recup.nom = "chat";
		
			System.out.println("espece (copie) : "+espece_recup.getEspeceType());
			System.out.println("espece (distribue) : "+animal_recup.getEspece().nom);

			// affiche l'objet modifier
			//animal_recup.afficherDossier();
			
			
			/*version-2*/
			ICabinet cabinet1 = (ICabinet) registry.lookup("cabinet-1");
			cabinet1.ajoutAnimal("tintin", "milou", new Espece("Chien", 10));
			
			IAnimal monAnimal = cabinet1.rechecheAnimal("milou");
			System.out.println(monAnimal.getNom());
			
			//distribution objet alerte du client
			//pour recevoir msg du serveur


			Alerte monAlerte = new Alerte();
			registry.bind("mon-alerte", monAlerte);
			
			
			cabinet1.inscriptionAlerte(host);
			
			for (int i = 0; i < 200; i++) {
				cabinet1.ajoutAnimal("tintin", "rintintin", new Espece("Chien", 10));
			}

			

		} catch (Exception e) {
			System.err.println("Client exception: " + e.toString());
			e.printStackTrace();
		}
	}
}