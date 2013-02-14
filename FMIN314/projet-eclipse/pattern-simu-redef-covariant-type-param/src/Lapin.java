
public class Lapin extends Herbivore {

	public void manger (Aliment l){ //pas utile
		try {	
			manger((Liseret)l);//Test type dynamique est Liseret
			System.out.println("void : manger(Aliment l) de Lapin");
		} catch (Exception e) {
			System.out.println("Lapin essaie de manger "+l.affiche());
		}
	}
	
	public void manger (Herbe l){
		try {	
			manger((Liseret)l);//Test type dynamique est Liseret
			System.out.println("void : manger(Herbe l) de Lapin");
		} catch (Exception e) {
			System.out.println("Lapin essaie de manger "+l.affiche());
		}
	}
	
	
	public void manger (Liseret l){
		try {	
			if (!(l instanceof Liseret))
				throw new Exception();//Test type dynamique est Liseret
			System.out.println("void : manger(Liseret l) de Lapin");
		}catch (Exception e) {
			System.out.println("Lapin essaie de manger "+l.affiche());
		}
	}
}
