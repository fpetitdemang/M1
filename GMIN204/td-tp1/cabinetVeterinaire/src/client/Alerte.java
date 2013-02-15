package client;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Vector;

import Interface.*;

public class Alerte extends UnicastRemoteObject implements IAlerte {
	
	boolean newAlerte = false;
	Vector<String> lAlerte = new Vector<String>();
	

	protected Alerte() throws RemoteException {
		super();
		// TODO Auto-generated constructor stub
	}

	@Override
	public void EnvoyerAlerte(String msg) throws RemoteException {
		lAlerte.add(msg);
		newAlerte = true;
		System.out.println(msg);
	}
	
}
