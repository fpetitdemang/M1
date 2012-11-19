
public class main {

	/**
	 * @param args
	 * @throws Exception 
	 */
	public static void main(String[] args) throws Exception {
		CycleSeminaires c1 = new CycleSeminaires();
		c1.set_titre("mon premier seminaire");
		c1.set_resume("Seminaire resume");
		c1.set_capacite(5);
		c1.ouvertureReservation();
		c1.inscription(new Adherent("bob"));
		c1.inscription(new Adherent("bob"));
		c1.inscription(new Adherent("bob"));
		c1.inscription(new Adherent("bob"));
		c1.inscription(new Adherent("bob"));
		c1.inscription(new Adherent("bob"));
		c1.inscription(new Adherent("bob"));
		
		c1.affiche();
		

	}

}
