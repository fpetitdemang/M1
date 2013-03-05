package client;

import java.rmi.RemoteException;

import Interface.Espece;



public class EspeceProtegee extends Espece{

	public EspeceProtegee(String m_espece, int m_dureevie)
			throws RemoteException {
		super(m_espece, m_dureevie);
		// TODO Auto-generated constructor stub
	}
	
	public String Info() {
		String chaine = "Attention espece protegee!!!!\n" +
				this.Info();
		return chaine;
	} 

}
