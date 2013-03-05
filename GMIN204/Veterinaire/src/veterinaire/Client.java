package veterinaire;


import java.rmi.RMISecurityManager;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

import Interface.Espece;
import Interface.ICabinet;


public class Client {
	private Client() {
	}

	public static void main(String[] args) {
		System.setProperty("java.security.policy", "/home/franck/M1/GMIN204/Veterinaire/src/veterinaire/securite");
		System.setSecurityManager(new RMISecurityManager());
		
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