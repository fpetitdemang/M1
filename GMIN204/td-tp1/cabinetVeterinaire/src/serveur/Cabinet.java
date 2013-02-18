package serveur;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.Vector;

import Interface.Espece;
import Interface.IAlerte;
import Interface.IAnimal;
import Interface.ICabinet;

public class Cabinet extends UnicastRemoteObject implements ICabinet{
	String nom;
	Vector<IAlerte>lAlerte = new Vector<IAlerte>();
	Vector<Animal> lAnimal = new Vector<Animal>();
	int nbPAtient = 0;
	
	public Cabinet (String m_nom) throws RemoteException{
		this.nom = m_nom;
	}

	@Override
	public boolean ajoutAnimal(String maitre, String nom, Espece type) throws RemoteException{
		nbPAtient++;
		
		for (int i = 0; i < lAlerte.size(); i++) {
			lAlerte.get(i).EnvoyerAlerte("ajout d'un animal");
		}
		
		if ((nbPAtient % 100) == 0)
		for (int i = 0; i < lAlerte.size(); i++) {
			lAlerte.get(i).EnvoyerAlerte("il y'a "+nbPAtient+" patients");
		}
		

		
		if (lAnimal.add(new Animal(maitre, nom, type)))
				return true;
		return false;
	}

	@Override
	public IAnimal rechecheAnimal(String mNom) throws RemoteException{		
		for (int i = 0; i < lAnimal.size(); i++) {
			if (lAnimal.get(i).nom == mNom){
				//return (IAnimal)lAnimal.get(i);
				}	
			}	
		return (IAnimal)lAnimal.get(0);
		//return null;
	}

	@Override
	public String infoEspece(Espece m_espece) throws RemoteException {
		return m_espece.Info();
	}

	@Override
	public void inscriptionAlerte(String host) throws RemoteException {
		Registry registry = LocateRegistry.getRegistry(host);
		try {
			IAlerte monAlerte = (IAlerte) registry.lookup("mon-alerte");
			lAlerte.add(monAlerte);
			monAlerte.EnvoyerAlerte("inscription alerte : done");
		} catch (NotBoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

	
}
