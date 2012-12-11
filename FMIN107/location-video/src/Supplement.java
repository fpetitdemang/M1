import java.util.Vector;


public abstract class Supplement implements Forfait {

	Forfait f;
	
	public Supplement(Forfait f){
		this.f = f;
	}
	
	@Override
	public abstract float calculer();

}