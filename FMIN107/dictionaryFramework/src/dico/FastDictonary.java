package dico;
import java.math.MathContext;
public class FastDictonary extends AbstractDictonary {
	
	public FastDictonary(){
		conteneur1 = new Object[10];
		conteneur2 = new Object[10];
		
		for (int i = 0; i < conteneur1.length; i++) {
			conteneur1[i] = 0;
		}		
	}

	@Override
	public int indexOf(Object key) {
		//calcule le hashcode associé à la cles
		int hashCode = key.hashCode() % conteneur1.length;
		
		
		//teste si il y'a conflit
		if (conteneur1[hashCode] != null && conteneur1[hashCode] == key)
			return hashCode;
		
		//effectue recherche sequentielle en cas de conflit
		return recherche_sequentielle(hashCode, key);
	}

	/**
	 * effectue une recherche sequentielle dans le conteneur 1
	 * @param indice
	 * @param key
	 * @return
	 */
	private int recherche_sequentielle(int indice, Object key) {
		for (int i = indice; i < conteneur1.length; i++) {
			if (conteneur1[indice] == key)
				return i;
		}
		return -1;
	}

	@Override
	public int newIndexOf(Object key) {
		int index;

		//test si index deja dans le conteneur, ecrase ancienne valeur
		/*index = indexOf(key);
		if ( index > 0 )
			return index;*/
		
		//test si conteneur doit être grandi
		if (mustGrow())
			grow();
		
		//calcul de l'index avec fonction de hachage
		index = key.hashCode()%conteneur1.length;
		

		//verifie qu'il y a pas de conflit
		if (conteneur1[index] == (Object)0){
			return index;
		}else{
		//sinon recherche emplacement vide
		return recherche_sequentielle(index, 0);
		}

	}


	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return null;
	}
	
	/**
	 * renvoie vraie si les tableaux sont au 3/4 pleins
	 * @return boolean
	 */
	public boolean mustGrow(){
		if (this.size() > (conteneur1.length / 4 * 3))
			return true;
		return false;
	}
	
	/**
	 * Fait grandir les conteneurs de 1/3
	 */
	public void grow(){
		//calcul nouvelle taille
		double nouvelle_taille = Math.ceil((this.size() * 1.3));
		
		//cree tableau plus grand
		Object tmp_conteneur1[] = new Object[(int) nouvelle_taille];
		Object tmp_conteneur2[] = new Object[(int) nouvelle_taille];
		
		//recopie valeur dans tableau plus grand
		for (int i = 0; i < conteneur1.length; i++) {
			tmp_conteneur1[i]=conteneur1[i];
			tmp_conteneur2[i]=conteneur2[i];
		}
		
		//switch les pointeurs sur les tableaux plus grand
		conteneur1 = tmp_conteneur1;
		conteneur2 = tmp_conteneur2;
	}


}
