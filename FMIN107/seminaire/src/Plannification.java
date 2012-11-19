
public class Plannification extends AbstractEtat{

	public Plannification(CycleSeminaires s) {
		super(s);
		// TODO Auto-generated constructor stub
	}
	
	public void set_capacite(int val)throws Exception{
		seminaire.capacite = val;
		seminaire.set_etatCourant(0);
	}
	
	public void set_titre(String val)throws Exception{
		seminaire.titre = val;
		seminaire.set_etatCourant(0);
	}
	public void set_resume(String val)throws Exception{
		seminaire.resume = val;
		seminaire.set_etatCourant(0);
	}
	public void set_creneaux(int val)throws Exception{
		seminaire.creneaux = val;
		seminaire.set_etatCourant(0);
	}
	
	public void ouvertureReservation()throws Exception{
		//place disponible
		seminaire.set_etatCourant(3);
	}

	@Override
	public void affiche() {
		System.out.println("\n\tEtat : Plannification\n");		
	} 
	
	

}
