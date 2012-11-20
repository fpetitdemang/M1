
public class Vider extends Commande{

	public Vider(jeu monJeux) {
		super(monJeux);
		// TODO Auto-generated constructor stub
	}

	public void Do(){
		int c = demanderChoixBidon(); 
		int v = demanderVolumeDeplacer();
		setVolumeDeplacer(monJeux.lBidon.get(c).getVolumeContenu());
		setBidonManipuler(c);
		monJeux.lBidon.get(c).vider();
	}

	protected void Undo() {		
		monJeux.lBidon.get(getBidonManipuler()).ajouter(getBidonManipuler());
	}

}
