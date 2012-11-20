
public class Remplir extends Commande{

	public Remplir(jeu monJeux) {
		super(monJeux);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected void Do() {
		int c = demanderChoixBidon(); 
		int v = demanderVolumeDeplacer();
		setVolumeDeplacer(monJeux.lBidon.get(c).getVolumeContenu());
		setBidonManipuler(c);
		monJeux.lBidon.get(c).remplir();
		
	}

	@Override
	protected void Undo() {
		// TODO Auto-generated method stub
		
	}


	
}
