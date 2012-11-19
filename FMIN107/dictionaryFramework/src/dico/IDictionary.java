package dico;

public interface IDictionary {
	
	/**
	 * Rend la valeur associée à key dans le conteneur
	 * @param key cles de la definition dans le dictionnaire 
	 * @return retourne la definition associé à la cles
	 */
	Object get(Object key);
	/**
	 * entre un nouveau couple cles/valeur dans le receveur
	 * @param key cles definition
	 * @param value valeur defintion
	 * @return receveur, this
	 */
	Object put(Object key, Object value);
	/**
	 * rend vrai si le receveur est vide, faux sinon
	 * @return vrai si le receveur est vide, faux sinon
	 */
	boolean isEmpty();
	/**
	 * rend vrai si la clef est contenu dans le receveur, faux sinon
	 * @param key
	 * @return
	 */
	boolean constaintsKey(Object key);
	int indexOf(Object key);
	int newIndexOf(Object key);
	int size();
	String toString();
		
}
