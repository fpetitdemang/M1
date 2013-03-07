package Graphe;

import java.util.Vector;

public class Graphe {
	String nom;
	Vector<Sommet> lSommet = new Vector<>();//association doivent etre caché du monde exterieur + jms retourné
	Vector<Arete> lArete = new Vector<>();
	
	public Graphe(String nom){
		this.nom = nom;
	}
	
	public void ajouterSommet(Sommet m_sommet){
		for (int i = 0; i < lSommet.size(); i++) {
			if (m_sommet == lSommet.get(i)){
				return;
			}
		}
		lSommet.add(m_sommet);
		m_sommet.ajouterGraphe(this);
	}

	public void ajouterArete(Arete m_arete){
		lArete.add(m_arete);
		ajouterSommet(m_arete.sommet1);
		ajouterSommet(m_arete.sommet2);
		m_arete.setGraphe(this);
	}
	
	public void supprimerSommet(Sommet m_sommet){
		lSommet.remove(m_sommet);
		m_sommet.supprimer();
		//invariant pas d'arete pendante -> supprime arete
		
		
	}
	
	
	/*public void grefferArete(Vector<Arete> l_arete){
		for (int i = 0; i < l_arete.size(); i++) {
			grefferArete(l_arete.get(i));
		}
	}*/
	
	
	
	
	public String toString(){
		String listeArete = "[";
		String listeSommet = "(";
		
		for (int i = 0; i < lArete.size(); i++) {
			listeArete += lArete.get(i).toString()+",";
		}
		
		for (int i = 0; i < lSommet.size(); i++) {
			listeSommet += lSommet.get(i).toString()+",";
		}
				
		return listeArete+"]\n"+listeSommet+")";
	}

	public void supprimerArete(Arete arete) {
		lArete.remove(arete);
		
	}

}
