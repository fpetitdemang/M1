
public class FactoryJeuDifficile extends AbstractFactoryJeu {

	@Override
	public Alphabet createAlphabet() {
		return new AlphabetLettre();
	}

	@Override
	public GestionnairePhraseMystere createGPM() {
		return new GPMnonGuide();
	}
	
}

