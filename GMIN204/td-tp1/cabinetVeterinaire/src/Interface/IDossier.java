package Interface;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface IDossier extends Remote{
	public void afficher()throws RemoteException;
	public void setObservation(String m_obser)throws RemoteException;

}
