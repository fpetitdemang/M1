package Interface;

import java.io.Serializable;
import java.rmi.RemoteException;

public interface IEspece extends Serializable {
	
	public String getEspeceType()throws RemoteException;
	public int getDureeMoyenne()throws RemoteException;

}
