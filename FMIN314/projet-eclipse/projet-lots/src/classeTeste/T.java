package classeTeste;
public class T{

	protected int identifiant;
	
	public T(int id) {
		this.identifiant = id;
	}
	public void whoiam(){
		System.out.println("Mon type dynamique est : T\n" +
				"mon identifiant : "+identifiant);
	}
	
}
