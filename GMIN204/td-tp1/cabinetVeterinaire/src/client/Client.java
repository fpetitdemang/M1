package client;

import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import Interface.*;

public class Client {
	private Client() {
	}

	public static void main(String[] args) {

		String host = (args.length < 1) ? null : args[0];
		try {
			System.setSecurityManager(null);
			Registry registry = LocateRegistry.getRegistry(host);

			/*version-1
			//demande stub auprès de rmiregistry
			IAnimal animal_recup = (IAnimal) registry.lookup("chien");

			//recupere stub 
			IDossier dossier_recup = animal_recup.getDossier();
			


			// modifie le dossier sur le client

			dossier_recup.setObservation("tout vas bien");

			//recupere espece par copie//
			Espece espece_recup = animal_recup.getEspece();
		
			System.out.println("mon espece : "+espece_recup.getEspeceType());
			

			// affiche l'objet modifier
			animal_recup.afficherDossier();*/
			
			
			/*version-2*/
			ICabinet cabinet1 = (ICabinet) registry.lookup("cabinet-1");
			cabinet1.ajoutAnimal("tintin", "milou", new Espece("Chien", 10));
			
			IAnimal monAnimal = cabinet1.rechecheAnimal("milou");
			System.out.println(monAnimal.getNom());
			
			

		} catch (Exception e) {
			System.err.println("Client exception: " + e.toString());
			e.printStackTrace();
		}
	}
}