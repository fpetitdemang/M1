package Graphe;

public class Arete {
	String nom;
	Graphe monGraphe;
	Sommet sommet1;
	Sommet sommet2;
	
	
	public Arete (Sommet smt1, Sommet smt2, String m_nom){
		sommet1 = smt1;
		sommet2 = smt2;
		nom = m_nom;
		smt1.grefferSommet(this);
		smt2.grefferSommet(this);
	}
	
	public Arete(Sommet smt1, Sommet smt2, Graphe m_graphe, String m_nom){
		this(smt1, smt2, m_nom);
		
		try {
			//invariant sommet doivent faire parti du meme graphe
			if (smt1.getGraphe() != smt2.getGraphe()) throw new Exception();
			monGraphe = m_graphe;

			
		} catch (Exception e) {
			// TODO: handle exception
		}
	}
	
	public void grefferAreteGraphe(Graphe m_graphe){
		monGraphe = m_graphe;
	}
	
	
	public String toString(){
		return nom+"("+sommet1.toString()+","+sommet2.toString()+")";
	}

	public void setGraphe(Graphe graphe) {
		monGraphe = graphe;
		sommet1.setGraphe(graphe);
		sommet2.setGraphe(graphe);		
	}

	public void supprimerArete() {
		monGraphe.supprimerArete(this);
		
	}

}
