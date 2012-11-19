import java.util.LinkedList;
import java.util.TimerTask;
import java.util.List;
import java.util.ListIterator;

public class CycleSeminaires {
	public String titre;
	public String resume;
	public int nbInscrit;
	public int capacite;
	public int creneaux;
	public List<Adherent> lAdherent;
	
	private AbstractEtat EtatCourant;
	private AbstractEtat[] etats = new AbstractEtat[12];
	
	
			
	public CycleSeminaires(){
		lAdherent = new LinkedList();
		
		etats[0] = new Plannification(this);
		etats[1] = new Annulable(this);
		etats[2] = new ReservationOuverte(this);
		etats[3] = new PlaceDisponible(this);
		etats[4] = new Complet(this);
		etats[5] = new EnCours(this);
		etats[6] = new Fini(this);
		etats[7] = new Terminer(this);
		etats[8] = new Abondonner(this);
		etats[9] = new Annuler(this);
		etats[10] = new Archive(this);
		etats[11] = new PreparationSeminaire(this);
		
		EtatCourant = etats[0];
		
	}
	public void ajoutListeAttente(Adherent ad) throws Exception{
		EtatCourant.ajoutListeAttente(ad);
		}

	public Adherent popListAttente()throws Exception{
		return EtatCourant.popListAttente();
	}
	
	public void ajoutInscrit()throws Exception{
		EtatCourant.ajoutInscrit();
	}
	
	public void retirerInscrit()throws Exception{
		EtatCourant.retirerInscrit();
	}
	
	public void inscription(Adherent ad)throws Exception{
		EtatCourant.inscription(ad);
		}
	
	public void desistement(Adherent ad)throws Exception{
		EtatCourant.desistement(ad);
	}
	
	public void archiver()throws Exception{
		EtatCourant.archiver();
	}
	
	public void abandon(Adherent ad)throws Exception{
		EtatCourant.abandon(ad);
	}
	
	public void ouvertureReservation()throws Exception{
		EtatCourant.ouvertureReservation();
	}
	
	public void clotureReservation()throws Exception{
		EtatCourant.clotureReservation();
	}
	
	public void annuler()throws Exception{
		EtatCourant.annuler();
	}
	
	public void ajoutDocument(Document doc)throws Exception{
		EtatCourant.ajoutDocument(doc);
	}
	
	public void set_capacite(int val)throws Exception{
		EtatCourant.set_capacite(val);
	}
	
	public void set_titre(String val)throws Exception{
		EtatCourant.set_titre(val);
	}
	public void set_resume(String val)throws Exception{
		EtatCourant.set_resume(val);
	}
	public void set_creneaux(int val)throws Exception{
		EtatCourant.set_creneaux(val);
	}
	
	public void set_etatCourant(int val){
		EtatCourant = etats[val];
	}
	
	
	public void affiche() {
		System.out.println("Contexte : \n");
		System.out.println("\tTitre : " +titre+ "\n");
		System.out.println("\tResume : " +resume+ "\n");
		System.out.println("\tListe Adherents :\n");
		for (int i = 0; i < lAdherent.size(); i++) {
			System.out.println("\t\t"+lAdherent.get(i).nom);
		}
		System.out.println("\n");
		EtatCourant.affiche();		
	} 
	
	    
	
	
}
