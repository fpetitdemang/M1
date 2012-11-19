public abstract class AbstractEtat {

	CycleSeminaires seminaire;
	
	public AbstractEtat(CycleSeminaires s){seminaire = s;}
	
	public  void ajoutListeAttente(Adherent ad) throws Exception{
		throw new Exception("Pas le droit");
		}

	public Adherent popListAttente()throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void ajoutInscrit()throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void retirerInscrit()throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void inscription(Adherent ad)throws Exception{
		
		throw new Exception("Pas le droit");}
	
	public void desistement(Adherent ad)throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void archiver()throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void abandon(Adherent ad)throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void ouvertureReservation()throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void clotureReservation()throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void annuler()throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void ajoutDocument(Document doc)throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void set_capacite(int val)throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public void set_titre(String val)throws Exception{
		throw new Exception("Pas le droit");
	}
	public void set_resume(String val)throws Exception{
		throw new Exception("Pas le droit");
	}
	public void set_creneaux(int val)throws Exception{
		throw new Exception("Pas le droit");
	}
	
	public abstract void affiche();
	

	
}
