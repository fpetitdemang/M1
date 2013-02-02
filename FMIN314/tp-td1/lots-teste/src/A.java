
public class A extends racine{

	public A(int id) {
		super(id);
	}

	//methode redefinit
	public void whoiam(){
		System.out.println("Mon type dynamique : A\n" +
				"mon identifiant : "+identifiant);
	}
	
	public void foo(T t){//surcharge sur parametre
		this.whoiam();
	}
}
