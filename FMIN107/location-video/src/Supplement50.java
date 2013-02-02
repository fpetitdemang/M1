
public class Supplement50 extends Supplement{

	public Supplement50(Forfait f) {
		super(f);
		// TODO Auto-generated constructor stub
	}

	@Override
	public float calculer() {
		// TODO Auto-generated method stub
		return f.calculer()+30;
	}

}
