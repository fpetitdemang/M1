
public class racine {
	protected int identifiant;
	
	public racine(int id){
		this.identifiant = id;	
	}
	
	public void whoiam(){
		System.out.println("Mon type dynamique est : racine\n" +
				"mon identifiant : "+identifiant);
	}
	
	public void foo(T t){//surcharge sur parametre
		this.whoiam();
	}
	
	//surcharge type de retour
}
