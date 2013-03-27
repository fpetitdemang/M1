package Graphe;

import java.util.Set;
import java.util.Vector;

public class Sommet {
	
	int id;
	Graphe monGraphe;
	Vector<Arete> laretes = new Vector<>();
	
	

	public Sommet(Graphe m_graphe){
		//invariant : un sommet appartient toujours a un graphe
		this.id = m_graphe.getNbSommet() + 1;
		ajout(m_graphe);
		
		//passage de reference
		//invariant -> association bidirectionnelle
		monGraphe.ajouter(this);
	}
	
	
	//Methode referencement -graphe, aretes-
	public void ajouter(Arete a){
		laretes.add(a);
	}
	
	public void ajout(Graphe m_graphe){
		this.monGraphe = m_graphe;
	}
	

	
	//Methode dereferencement -graphe, aretes-
	
	public void supprimer(Arete m_arete){
		laretes.remove(m_arete);
	}
	
	
	
	
	
	
	
	public void supprimer(){
		
		//dereference aretes
		for (int i = 0; i < laretes.size(); i++) {
			laretes.get(i).supprimer(this);
		}
		//dereference graphe
		monGraphe.supprimer(this);
	}
	

	
	/*
	public void ajouterGraphe(Graphe m_Graphe){
		this.monGraphe = m_Graphe;
	}

		

	public void setGraphe(Graphe graphe) {
		monGraphe = graphe;		
	}
	
	public Vector<Arete> getAreteGreffer(){
		return laretes;
	}*/
	
	
	
	
	/*************/
	
	
	public Graphe getGraphe(){
		return this.monGraphe;
	}
	
	
	public String toString(){
		return identifiant;
	}
	
}
