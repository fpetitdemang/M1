
public class SupplementIll extends Supplement {

	public SupplementIll(Forfait f) {
		super(f);
		// TODO Auto-generated constructor stub
	}

	@Override
	public float calculer() {
		// TODO Auto-generated method stub
		return f.calculer()+20;
	}


}
