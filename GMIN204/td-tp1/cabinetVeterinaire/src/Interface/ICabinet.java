package Interface;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ICabinet extends Remote{
	
	public boolean ajoutAnimal(String maitre, String nom, Espece type) throws RemoteException;
	public IAnimal rechecheAnimal(String m_nom)throws RemoteException;
	
	
}
