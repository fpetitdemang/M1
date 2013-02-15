package Interface;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface IAlerte extends Remote{
	public void EnvoyerAlerte(String msg)throws RemoteException;
}
