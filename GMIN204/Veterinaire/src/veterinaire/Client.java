package veterinaire;


import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import Interface.ICabinet;


public class Client {
	private Client() {
	}

	public static void main(String[] args) {
		System.setProperty("java.security.policy","file:///home/franck/M1/GMIN204/td-tp1/cabinetVeterinaire/src/serveur/server.txt");
		String host = (args.length < 1) ? null : args[0];
		try {
			System.setSecurityManager(null);
			
			Registry registry = LocateRegistry.getRegistry(host);

			ICabinet cabinet1 = (ICabinet) registry.lookup("cabinet-1");
			
			System.out.println(cabinet1.infoEspece(new EspeceProtegee("chien", 10)));
			
			

		} catch (Exception e) {
			System.err.println("Client exception: " + e.toString());
			e.printStackTrace();
		}
	}
}