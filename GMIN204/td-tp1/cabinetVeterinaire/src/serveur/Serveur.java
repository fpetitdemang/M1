package serveur;

import java.rmi.Remote;
import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;

public class Serveur {

	public Serveur() {}


	public static void main(String args[]) {

		try {
			System.setSecurityManager(null);
			
			//instancie objet a distribuer
			Animal chien = new Animal("tintin","milou");
			//Registry registry = LocateRegistry.createRegistry(1099);
			Registry registry = LocateRegistry.getRegistry();
			if (registry==null){
				System.err.println("RmiRegistry not found");
			}else{
				//enregistre indexe les objets à distribuer dans le registre
				registry.bind("chien", chien);
				System.err.println("Server ready");
			}
		} catch (Exception e) {
			System.err.println("Server exception: " + e.toString());
			e.printStackTrace();
		}
	}
}