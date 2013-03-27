package Graphe;

public class Arete {
	int id;
	Graphe monGraphe;
	Sommet sommet1;
	Sommet sommet2;
	
	
	public Arete (Sommet smt1, Sommet smt2){
		
		try {
			//invariant -> sommet doivent faire parti du meme graphe
			if (smt1.getGraphe() != smt2.getGraphe()) throw new Exception();
			
			sommet1 = smt1;
			sommet2 = smt2;
			monGraphe = smt1.getGraphe();
			id = monGraphe.getNbArete() + 1;
			
			//passage de reference
			//invariant -> association bidirectionnelle
			monGraphe.ajouter(this);
			smt1.ajouter(this);
			smt2.ajouter(this);

			} catch (Exception e) {
			// TODO: handle exception
			}	
		
		
	}
	
	
	public void supprimer(Sommet m_sommet) {
		//invariant pas d'arete pendante
		this.supprimer();
	}
	
	/*public void supprimer(Graphe m_graphe) {
	}*/



	public void supprimer() {
		//invariant -> supprimer arete => supprimer association en 2 sommets, mais supprime pas les sommets
		monGraphe.supprimer(this);
		sommet1.supprimer(this);
		sommet2.supprimer(this);
	}




}
