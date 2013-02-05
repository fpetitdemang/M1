package Interface;

import java.rmi.Remote;
import java.rmi.RemoteException;


public interface IAnimal extends Remote{

	public String identite()throws RemoteException;
	public IDossier getDossier()throws RemoteException;
	public void setDossier(IDossier m_dossier)throws RemoteException;
	public void afficherDossier() throws RemoteException;

}


