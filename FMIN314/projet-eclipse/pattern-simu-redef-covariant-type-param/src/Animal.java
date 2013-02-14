
public class Animal {
	
	static int compteur; 
	int num;
	
	public Animal(){
		this.num = compteur++;
	}
	
	public void manger(Aliment a){
		System.out.println("void : manger(Aliment a)");
	}
	
	public void whoIam(){
		System.out.println("je suis l'objet "+compteur);
	}
}
