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

			//demande stub auprès de rmiregistry
			IAnimal stub = (IAnimal) registry.lookup("chien");

			//recupere stub 
			IDossier dossier_recup = stub.getDossier();
			


			// modifie le dossier sur le client
			System.out.println("/* ajoute observation dans le dossier */");
			dossier_recup.setObservation("tout vas bien");

			// renvoie l'objet modifier sur le serveur
			// stub.setDossier(dossier_recup);

			// affiche l'objet modifier
			stub.afficherDossier();

		} catch (Exception e) {
			System.err.println("Client exception: " + e.toString());
			e.printStackTrace();
		}
	}
}