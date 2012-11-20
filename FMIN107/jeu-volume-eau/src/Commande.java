
public abstract class Commande {
	
int volumeDeplacer;
int bidonManipuler;
jeu monJeux;

/**
 * Declatation accesseur
 */
void setVolumeDeplacer(int v){
	this.volumeDeplacer = v;
}

void setBidonManipuler(int b){
	bidonManipuler = b;
}

int getBidonManipuler(){
	return bidonManipuler;
}

int getVolumeDeplacer(){
	return volumeDeplacer;
}

/** 
 *Constructeurs
 */
public Commande(jeu monJeux){
	this.monJeux = monJeux;
}

/**
 * Execute la methode
 */
protected abstract void Do();

/**
 * Annule la derniere action
 */
protected void Undo(){
	monJeux.lBidon.get(getBidonManipuler()).ajouter(getBidonManipuler());
}

/**
 * Demande sur quel bidon s'execute la methode
 * @return indexe du bidon dans la liste
 */
protected int demanderChoixBidon() {
	System.out.println("Quel Bidon prendre ?\n");
	System.out.println("Bidon -> 0\n");
	
	return 0;
}

/**
 * Demande qu'elle volume d'eau deplacer
 * @return volume d'eau deplacer
 */
protected int demanderVolumeDeplacer(){
	System.out.println("Volume deplacer -> 20litre\n");
	volumeDeplacer = 20;
	
	return 20;
}



}
