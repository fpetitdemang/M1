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
		
		
		graphe1.grefferArete(new Arete(smt1, smt2,"a"));
		graphe1.grefferArete(new Arete(smt1, smt3,"b"));
		graphe1.grefferArete(new Arete(smt2, smt3,"c"));
		graphe1.grefferArete(new Arete(smt1, smt4,"d"));
		graphe1.grefferArete(new Arete(smt3, smt4,"e"));
		
		//System.out.println(smt1.getAreteGreffer().toString());
		smt1.detacherSommet();
		graphe1.grefferArete(new Arete(smt1, smt2,"a"));
		
		System.out.println(graphe1.toString());

	}

}
