package Graphe;

import java.util.Vector;

public class Graphe {
	String nom;
	Vector<Sommet> lSommet = new Vector<>();//association doivent etre caché du monde exterieur + jms retourné
	Vector<Arete> lArete = new Vector<>();
	
	public Graphe(String nom){
		this.nom = nom;
	}
	
	//Methode referencement -sommets, aretes-
	
	/**
	 * ajoute un sommet dans la collection de sommet du graphe
	 * rmq : un sommet appartient toujours à un graphe
	 *  + methode appeler depuis une instance de sommet
	 * @param m_sommet
	 */
	public void ajouter(Sommet m_sommet){
		
		//teste si sommet n'est pas deja dans le graphe
		for (int i = 0; i < lSommet.size(); i++) {
			if (m_sommet == lSommet.get(i)){
				return;
			}
		}
		
		lSommet.add(m_sommet);//ajoute sommet aux vecteur
	}

	 
	/**
	 * ajoute une arete dans la collection d'arete du graphe
	 * rmq : un sommet appartient toujours à un graphe
	 *  + methode apeller depuis une instance de arete
	 * @param m_arete
	 */
	public void ajouter(Arete m_arete){
		lArete.add(m_arete);
	}
	
	
	//Methode de dereferencement -sommets, aretes-
	
	public void supprimer(Sommet m_sommet){
		m_sommet.supprimer();
	}
	
	public void supprimer(Arete arete) {
		lArete.remove(arete);
	}
	
	
	
	//FABRIQUE
	
	/**
	 * Fabrique d'arete
	 * @param m_sommet
	 * @param m_sommet
	 * @return reference à l'arete construite
	 */
	public Arete ajouter(Sommet m_sommet1, Sommet m_sommet2){
		return new Arete(m_sommet1, m_sommet2);
	}
	

	
	
	/*public void grefferArete(Vector<Arete> l_arete){
		for (int i = 0; i < l_arete.size(); i++) {
			grefferArete(l_arete.get(i));
		}
	}*/
	
	
	public int getNbSommet(){
		return lSommet.size();
	}
	
	public int getNbArete(){
		return lArete.size();
	}
	
	
	
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



}
