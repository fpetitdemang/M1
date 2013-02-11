package serveur;


import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import Interface.*;

public class Animal extends UnicastRemoteObject implements IAnimal{
String nom;
String maitre;
Dossier monDossier;
Espece monespece;

public Animal(String maitre, String nom, Espece m_espece) throws RemoteException{
	this.nom = nom;
	this.maitre = maitre;
	this.monDossier = new Dossier();
	this.monespece = new Espece("chien", 15);
}

@Override
public String identite() throws RemoteException{
		String chaine = "nom : "+nom+"\n" +
				"maitre :"+maitre+"\n";
	return chaine;
}

@Override
public IDossier getDossier() throws RemoteException{
	return (IDossier) monDossier;
}

@Override
public void setDossier(IDossier m_Dossier) throws RemoteException{
	monDossier = (Dossier) m_Dossier;
	
}

@Override
public void afficherDossier() throws RemoteException{
	monDossier.afficher();	
}

@Override
public Espece getEspece() throws RemoteException{
	return monespece;
}

@Override
public String getNom() throws RemoteException {
	return nom;
	
}

}
