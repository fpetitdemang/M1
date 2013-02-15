package Interface;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ICabinet extends Remote{
	
	public boolean ajoutAnimal(String maitre, String nom, Espece type) throws RemoteException;
	public IAnimal rechecheAnimal(String m_nom)throws RemoteException;
	/**
	 * Retourne informations sur l'espece passe en parametre
	 * @param m_espece
	 * @return
	 * @throws RemoteException
	 */
	public String infoEspece(Espece m_espece)throws RemoteException;
	public void inscriptionAlerte(int port)throws RemoteException;
}
