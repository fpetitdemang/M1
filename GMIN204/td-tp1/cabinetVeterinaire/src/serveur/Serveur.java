 package serveur;

import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;

import Interface.Espece;

public class Serveur {

	public Serveur() {}


	public static void main(String args[]) {

		
		
		try {
			System.setSecurityManager(null);
			/*version-1*/
			//instancie objet a distribuer
			Animal chien = new Animal("tintin","milou",new Espece("chien", 15));
			
			
			
			
			//Registry registry = LocateRegistry.createRegistry(1098);
			Registry registry = LocateRegistry.getRegistry();
			
			
			/*version-2*/
			Cabinet cabinet1 = new Cabinet("cabinet-1");
			
			
			/*version-3*/
			System.setProperty("java.rmi.server.codebase","file:///auto_home/fpetitdemang/M1/GMIN204/Veterinaire/scr");
			

			
			
			
			if (registry==null){
				System.err.println("RmiRegistry not found");
			}else{
				/*vesion-1*/
				//enregistre indexe les objets à distribuer dans le registre
				registry.bind("chien", chien);
				
				/*version-2*/
				registry.bind("cabinet-1", cabinet1);
				
				
				System.out.println("Server ready");
			}
		} catch (Exception e) {
			System.err.println("Server exception: " + e.toString());
			e.printStackTrace();
		}
	}
}