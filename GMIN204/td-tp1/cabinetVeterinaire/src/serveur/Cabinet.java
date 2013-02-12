package serveur;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Vector;

import Interface.Espece;
import Interface.IAnimal;
import Interface.ICabinet;

public class Cabinet extends UnicastRemoteObject implements ICabinet{
	String nom;
	Vector<Animal> lAnimal = new Vector<Animal>();
	
	public Cabinet (String m_nom) throws RemoteException{
		this.nom = m_nom;
	}

	@Override
	public boolean ajoutAnimal(String maitre, String nom, Espece type) throws RemoteException{
		if (lAnimal.add(new Animal(maitre, nom, type)))
				return true;
		return false;
	}

	@Override
	public IAnimal rechecheAnimal(String mNom) throws RemoteException{		
		for (int i = 0; i < lAnimal.size(); i++) {
			if (lAnimal.get(i).getNom() == mNom)
				return (IAnimal)lAnimal.get(i);
			}	
		return (IAnimal)lAnimal.get(0);
		//return null;
	}

	@Override
	public String infoEspece(Espece m_espece) throws RemoteException {
		return m_espece.Info();
	}

	
}
