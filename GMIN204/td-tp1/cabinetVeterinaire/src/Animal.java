import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Animal extends UnicastRemoteObject implements IAnimal{
String nom;
String maitre;

public Animal(String maitre, String nom) throws RemoteException{
	this.nom = nom;
	this.maitre = maitre;
}

@Override
public String identite() {
		String chaine = "nom : "+nom+"\n" +
				"maintre :"+maitre+"\n";
	return chaine;
}

}
