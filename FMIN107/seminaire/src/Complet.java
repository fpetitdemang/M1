
public class Complet extends ReservationOuverte{

	public Complet(CycleSeminaires s) {
		super(s);
		// TODO Auto-generated constructor stub
	}
	
	public void affiche() {
		System.out.println("\n\tEtat : Complet");		
	} 

	public void inscription(Adherent ad)throws Exception{
		System.out.println("Le seminaire est complet");
		//complet
		seminaire.set_etatCourant(4);
	}
}
