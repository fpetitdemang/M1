package client;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import serveur.Dossier;
import serveur.IAnimal;
import serveur.IDossier;

public class Client {
	private Client() {}

	public static void main(String[] args) {

		String host = (args.length < 1) ? null : args[0];
		try {
			System.setSecurityManager(null);
			Registry registry = LocateRegistry.getRegistry(host);
			
			
			
			
			IAnimal stub = (IAnimal) registry.lookup("chien");
			
			//recupere dossier sur le serveur
			IDossier dossier_recup = stub.getDossier();
			dossier_recup.afficher();
			
			//modifie le dossier sur le client
			dossier_recup.setObservation("tout vas bien");
			
			//renvoie l'objet modifier sur le serveur
			//stub.setDossier(dossier_recup);
			
			//affiche l'objet modifier
			stub.afficherDossier();
			

		} catch (Exception e) {
			System.err.println("Client exception: " + e.toString());
			e.printStackTrace();
		}
	}
}