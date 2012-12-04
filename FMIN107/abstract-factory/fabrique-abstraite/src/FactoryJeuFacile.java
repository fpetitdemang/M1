public class FactoryJeuFacile extends AbstractFactoryJeu {

	@Override
	public Alphabet createAlphabet() {
		return new AlphabetSymbol();
	}

	@Override
	public GestionnairePhraseMystere createGPM() {
		return new GPMGuide();
	}
}

