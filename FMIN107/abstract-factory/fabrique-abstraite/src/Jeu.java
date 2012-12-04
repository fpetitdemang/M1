
public class Jeu {
  // Fields
  // 
	Alphabet myAlphabet;
	GestionnairePhraseMystere myGestionnaire;
  // Methods
  /**
   */
  public Jeu (AbstractFactoryJeu j) {
    myAlphabet = j.createAlphabet();
    myGestionnaire = j.createGPM();
  }
  // Accessor Methods
  // Operations
}

