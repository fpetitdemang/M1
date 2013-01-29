
public class teste {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
			
		System.out.println("Mon type statique est : A");
		A  ab = new B(1);//declare variable polymorphique
		Tt Tt = new Tt(2);
		ab.foo(Tt);
		ab.foo(Tt);
	}

}
