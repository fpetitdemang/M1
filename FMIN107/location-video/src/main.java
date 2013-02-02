
public class main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		ForfaitConcret monForfait = new ForfaitConcret(new ForfaitBase());
		System.out.println(monForfait.calculer());
	}

}
