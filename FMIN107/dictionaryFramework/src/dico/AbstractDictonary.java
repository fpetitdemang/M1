package dico;

public abstract class AbstractDictonary implements IDictionary {

	protected Object conteneur1[], conteneur2[];
	
	
	/**
	 * rend l'index auquel est rangé le nom key dans le dictionnaire receveur,
	 * si key pas dans le receveur, renvoie -1
	 */
	public abstract int indexOf(Object key);
	/**
	 * calcule l'index auquel pourra avoir lieu l'insertion
	 * @param key
	 * @return
	 */
	public abstract int newIndexOf(Object key);

	public abstract String toString();

	public Object get(Object key){
		int index = indexOf(key);
		if( index >= 0 )
			return conteneur2[index];
		return -1;		
	}
	
	public Object put(Object key, Object value){
		int index;
		index = indexOf(key);
		//teste si la cles existe deja 
		if (index < 0)
			index = newIndexOf(key);
		
		conteneur1[index]=key;
		conteneur2[index]=value;
			
		return this;
			
	}
		
	public boolean constaintsKey(Object key){
		if( indexOf(key) < 0)
			return false;
		return true;
	}
	
	public boolean isEmpty(){
		for (int i = 0; i < conteneur1.length; i++) {
			if (conteneur1[i] != null)
				return false;
		}
		return true;
	}
	
	/**
	 * rend le nombre de couple nom/valeur effectivement contenu dans le dictionnaire
	 */
	public int size(){
		int compteur = 0;
		for (int i = 0; i < conteneur1.length; i++) {
			if (conteneur1[i] != (Object)0)
				compteur++;
		}
		return compteur;
	}
	
	
}
