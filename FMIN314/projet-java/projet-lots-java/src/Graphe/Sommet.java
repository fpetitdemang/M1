package Graphe;

import java.util.Set;

public class Sommet {
	Graphe monGraphe;
	Set<Arete> laretes;

	public Sommet(Graphe m_graphe){
		this.monGraphe = m_graphe;
	}
	
	
	//manipulation association
	public void ajouter(Arete m_arete){
		laretes.add(m_arete);
	}
	
	
	
}
