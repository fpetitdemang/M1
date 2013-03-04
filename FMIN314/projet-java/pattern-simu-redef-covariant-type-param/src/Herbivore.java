
public class Herbivore extends Animal{
	
	public void manger(Herbe h){
		System.out.println("void : manger(Herbe h) de Herbivore");
	}

	public void manger(Aliment a){
		try {
			manger((Herbe)a);//Test type dynamique est Herbe
			System.out.println("void : manger(Aliment a) de Herbivore");
		} catch (Exception e) {
			System.out.println("Herbivore essaie de manger "+a.affiche());
		}
	}
}
