
import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import dico.OrderedDictionary;


public class test {
	OrderedDictionary dico;

	@Before
	public void setUp() throws Exception {
		dico = new OrderedDictionary();
	}
	
	@Test
	public void testMethodeconstaintsKey(){
		assertFalse(dico.constaintsKey("1"));
		dico.put("1", "un");
		
		assertEquals(1,dico.size());
		assertTrue(dico.constaintsKey("1"));
	}
	
	@Test
	public void testMethodIndexOf(){
		//elmt pas dans index 
		assertEquals(dico.indexOf("1"), (Object)(-1));

		//bonne indexation d'elmts
		for (int i = 0; i < 5; i++) {
			dico.put(i, i);
			assertEquals(dico.indexOf(i), (Object)(i));
		}
	}
	
	@Test
	public void testMethodnewIndexOfFullDico(){
		//rempli le dico
		dico.put("1", "un");
		
		//verifie augmentation conteneur
		dico.newIndexOf("deux");
		assertEquals(dico.size(), 2);	
		
		//verifie rend bon index
		assertEquals(1, dico.newIndexOf("deux"));
	}
	
	@Test
	public void testMethodnewIndexOfEmptyDico(){
		//dico est bien vide
		assertTrue(dico.isEmpty());
		
		//verifie que conteneur n'augmente pas
		assertEquals(dico.size(), 1);	
		dico.newIndexOf("deux");
		assertEquals(dico.size(), 1);	
		
		//verifie rend bon index
		assertEquals(0, dico.newIndexOf("deux"));
	}
	
	@Test
	public void testMethodsize(){
		assertEquals(dico.size(), 0);
	}
	
	@Test
	public void testMethodisEmpty(){
		assertTrue(dico.isEmpty());	

		dico.put("1", "un");
		assertFalse(dico.isEmpty());
	}
	
	@Test
	public void testAddOneElementToEmptyDico(){
		dico.put("1", "un");
		assertEquals(1,dico.size());
		assert dico.get("1") != (Object)(-1);
		assertEquals("un", dico.get("1"));
	}
	
	@Test
	public void testConsultationElementToDico(){
		dico.put("1", "un");
		assertEquals("un", dico.get("1"));
		
	}
	
	
	/**
	 * Test que le conteneur est toujours plein
	 */
	@Test
	public void testConteneurSize(){
		dico.put("1", "un");
		dico.put("2", "deux");
		dico.put("3", "trois");
		dico.put("4", "quatre");
		assertEquals(4, dico.size());
	}

}
