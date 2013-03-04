package Graphe;

import java.util.Set;
import java.util.Vector;

public class Sommet {
	
	String identifiant;
	Graphe monGraphe;
	Vector<Arete> laretes = new Vector<>();
	
	public Sommet(String id){
		identifiant = id;
	}
	

	
	public void detacherSommet(){
		for (int i = 0; i < laretes.size(); i++) {
			laretes.get(i).supprimerArete();
		}
	}
	
	
	public void grefferSommet(Arete a){
		laretes.add(a);
	}

		
	public Graphe getGraphe(){
		return monGraphe;
	}

	public void setGraphe(Graphe graphe) {
		monGraphe = graphe;		
	}
	
	public Vector<Arete> getAreteGreffer(){
		return laretes;
	}
	
	
	
	
	/*************/
	
	
	public String toString(){
		return identifiant;
	}
	
}
