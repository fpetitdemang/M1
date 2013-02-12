package Interface;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;


public class Espece implements Serializable, IEspece{
	public String nom;
	public int DureeVieMoyenne;
	
	public Espece(String m_espece, int m_dureevie) throws RemoteException{
		nom = m_espece;
		DureeVieMoyenne = m_dureevie;
	}

	@Override
	public int getDureeMoyenne() {
		return DureeVieMoyenne;
	}

	@Override
	public String getEspeceType() {
		return nom;
	}

	public String Info() {
		String chaine = "Type : "+nom+"\n" +
				"Duree de vie moyenne : "+DureeVieMoyenne;
		return chaine;
	}
	
	
}
