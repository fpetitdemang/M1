
public class main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		Aliment AlimentHerbe = new Herbe();
		Aliment AlimentViande = new Viande();
		
		Animal AnimalHerbivore = new Herbivore();
		
		AnimalHerbivore.manger(AlimentHerbe);

	}

}
