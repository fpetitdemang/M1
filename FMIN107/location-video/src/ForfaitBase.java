
public class ForfaitBase extends Supplement{

	public ForfaitBase(Forfait f) {
		super(f);
	}
	
	public ForfaitBase() {
		super(null);
	}
	

	@Override
	public float calculer() {
		return 10;
	}


}
