
public class main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		//Declare aliments
		Aliment AlimentHerbe = new Herbe();
		Aliment AlimentViande = new Viande();

		
		
		Aliment AlimentLiseret = new Liseret();
		Herbe HerbeLiseret = new Liseret();
		Liseret LiseretLiseret = new Liseret();
		
		Herbe HerbeHerbe = new Herbe();
	
		
		//Declare animaux
		Lapin LapinLapin = new Lapin();
		
		Herbivore HerbivoreHerbivore = new Herbivore();
		Animal animalAnimal = new Animal();

		
		HerbivoreHerbivore.manger(AlimentViande);
		//AnimalHerbivore.manger(HerbeHerbe);//test appel manger(Herbe h)
		
		//LapinLapin.manger(AlimentLiseret);
		

	}

}
