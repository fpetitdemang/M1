package dico;

public class OrderedDictionary extends AbstractDictonary {
	
	public OrderedDictionary(){
		conteneur1 = new Object[1];
		conteneur2 = new Object[1];
		
		for (int i = 0; i < conteneur1.length; i++) {
			conteneur1[i] = 0;
		}	
	};
	
	public OrderedDictionary(int size){
		conteneur1 = new Object[size];
		conteneur2 = new Object[size];
		
		for (int i = 0; i < conteneur1.length; i++) {
			conteneur1[i] = 0;
		}	
		
		
	}

	@Override
	public int indexOf(Object key) {
		for (int i = 0; i < size(); i++) {
			if (conteneur1[i] == key)
				return i;
		}
		return -1;
	}

	@Override
	public int newIndexOf(Object key) {
		int index;

	
		//test si derniere elmt n'est pas vide
		index = conteneur1.length - 1;
		if (conteneur1[index] == (Object)0)
			return index;
		
		/**
		 * Augmente la taille des conteneurs
		 */
			
		//cree tableau plus grand
		Object tmp1[] = new Object[size()+1];
		Object tmp2[] = new Object[size()+1];
		
		//recopie valeur dans tableau plus grand
		for (int i = 0; i < conteneur1.length; i++) {
			tmp1[i]=conteneur1[i];
			tmp2[i]=conteneur2[i];
		}
		
		//switch les pointeurs sur les tableaux plus grand
		conteneur1 = tmp1;
		conteneur2 = tmp2;
		
		return conteneur1.length-1;
	}

	@Override
	public String toString() {
		for (int i = 0; i < size(); i++) {
			System.out.println(conteneur1[i]+" : "+conteneur2[i]);
		}
		return null;
	}

	







  


}
