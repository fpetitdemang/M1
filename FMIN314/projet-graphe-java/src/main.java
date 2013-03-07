import Graphe.*;

public class main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
	
		Graphe graphe1 = new Graphe("g");
		
		Sommet smt1 = new Sommet("1");
		Sommet smt2 = new Sommet("2");
		Sommet smt3 = new Sommet("3");
		Sommet smt4 = new Sommet("4");
		Sommet smt5 = new Sommet("5");
		
		
		graphe1.ajouterArete(new Arete(smt1, smt1,"a"));
		graphe1.ajouterArete(new Arete(smt1, smt3,"b"));
		graphe1.ajouterArete(new Arete(smt2, smt3,"c"));
		graphe1.ajouterArete(new Arete(smt1, smt4,"d"));
		graphe1.ajouterArete(new Arete(smt3, smt4,"e"));
		graphe1.ajouterSommet(smt5);
		graphe1.ajouterArete(new Arete(smt3, smt5,"f"));
		
		
		//System.out.println(smt1.getAreteGreffer().toString());
		graphe1.supprimerSommet(smt1);
		graphe1.supprimerSommet(smt5);
		//graphe1.ajouterArete(new Arete(smt1, smt2,"a"));
		
		System.out.println(graphe1.toString());

	}

}
