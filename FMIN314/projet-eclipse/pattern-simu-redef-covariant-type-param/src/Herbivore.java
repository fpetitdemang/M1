
public class Herbivore extends Animal{

	public void manger(Aliment a){
		if (a instanceof Herbe){
		super.manger((Herbe)a);
		}else{
			System.out.println("Herbivore essaie de manger "+a.affiche());
		}
	}
}
