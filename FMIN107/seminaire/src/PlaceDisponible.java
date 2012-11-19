
public class PlaceDisponible extends ReservationOuverte {

	public PlaceDisponible(CycleSeminaires s) {
		super(s);
		// TODO Auto-generated constructor stub
	}

	public void inscription(Adherent ad)throws Exception{
		System.out.println("j'ai ajouté un adherent");
		
		seminaire.lAdherent.add(ad);
		
		if (seminaire.lAdherent.size() < seminaire.capacite - 1)
		{	
			//placeDisponible
			seminaire.set_etatCourant(3);	
		}else{
			//complet
			seminaire.set_etatCourant(4);}
	}
	
	public void affiche() {
		System.out.println("\n\tEtat : PlaceDisponible\n");		
	} 

}
