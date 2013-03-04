package Graphe;

import java.util.Vector;

public class Graphe {
	String nom;
	Vector<Sommet> lSommet = new Vector<>();
	Vector<Arete> lArete = new Vector<>();
	
	public Graphe(String nom){
		this.nom = nom;
	}

	public void grefferArete(Arete m_arete){
		lArete.add(m_arete);
		m_arete.setGraphe(this);
	}
	
	public void grefferArete(Vector<Arete> l_arete){
		for (int i = 0; i < l_arete.size(); i++) {
			grefferArete(l_arete.get(i));
		}
	}
	
	
	
	
	public String toString(){
		String listeArete = "[\n";
		
		for (int i = 0; i < lArete.size(); i++) {
			listeArete += lArete.get(i).toString()+"\n";
		}
		
		return listeArete+"]";
	}



	public void supprimerArete(Arete a) {
		for (int i = 0; i < lArete.size(); i++) {
			if (lArete.get(i) == a){
				lArete.remove(i);
			}
		}
		
	}
}
