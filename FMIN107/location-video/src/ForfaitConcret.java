
public class ForfaitConcret implements Forfait{
	
	Supplement pInstanceSupplement;
	
	public ForfaitConcret(Supplement pI){
		this.pInstanceSupplement = pI;
	}
	
	@Override
	public float calculer() {
		return pInstanceSupplement.calculer();
	}

}
