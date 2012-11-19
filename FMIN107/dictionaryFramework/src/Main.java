import dico.*;


public class Main{
	  public static void main(String[] args){
		OrderedDictionary monDico = new OrderedDictionary();
		monDico.put("nordine", "cf cas social");
		monDico.put("nordine1", "cf cas social");
		//monDico.toString();
		System.out.println(monDico.get("nordine"));
		System.out.println(monDico.get("nordine1"));
		}
}
