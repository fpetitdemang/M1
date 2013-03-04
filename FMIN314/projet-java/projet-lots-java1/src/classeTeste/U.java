package classeTeste;
public class U {
	
	protected int identifiant;

	public U(int id) {
		this.identifiant = id;
	}
	public void whoiam(){
		System.out.println("Mon type dynamique est : U\n" +
				"mon identifiant : "+identifiant);
	}

}
