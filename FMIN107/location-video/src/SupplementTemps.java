import java.util.Iterator;


public class SupplementTemps extends Supplement{

	public SupplementTemps(Forfait f) {
		super(f);
		// TODO Auto-generated constructor stub
	}

	@Override
	public float calculer() {
		return f.calculer()+10;
	}

}
