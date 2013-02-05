package classeTeste;

public class B extends A{

	public B(int id) {
		super(id);
		// TODO Auto-generated constructor stub
	}

	public void whoiam(){
		System.out.println("Mon type dynamique est : B\n" +
				"mon identifiant : "+identifiant);
	}
	
	public void foo(U u){
		this.whoiam();
	}
}
