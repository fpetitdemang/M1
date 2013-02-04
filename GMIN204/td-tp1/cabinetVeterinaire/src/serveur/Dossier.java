package serveur;


import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Dossier extends UnicastRemoteObject implements IDossier{
	String observation;
	
	public  Dossier()throws RemoteException{
		observation = "";
	}
	
	public void afficher()throws RemoteException{
		System.out.println("Observation : " + observation);
	}
	
	public void setObservation(String m_obser)throws RemoteException{
		observation = m_obser;
	}
}
