package serveur;


import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Animal extends UnicastRemoteObject implements IAnimal{
String nom;
String maitre;
Dossier monDossier;

public Animal(String maitre, String nom) throws RemoteException{
	this.nom = nom;
	this.maitre = maitre;
	this.monDossier = new Dossier();
}

@Override
public String identite() throws RemoteException{
		String chaine = "nom : "+nom+"\n" +
				"maitre :"+maitre+"\n";
	return chaine;
}

@Override
public IDossier getDossier() {
	return monDossier;
}

@Override
public void setDossier(IDossier m_Dossier) {
	monDossier = (Dossier) m_Dossier;
	
}

@Override
public void afficherDossier() throws RemoteException{
	monDossier.afficher();	
}

}
