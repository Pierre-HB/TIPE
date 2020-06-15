package main;


public class Vect {
	private float[] data;
	private int nbValues = 0;
	private int increment = 5;
	private float moyenne = 0;
	private int sizeMin = 5;

	public Vect() {
		data = new float[increment];
	}
	public Vect(int size, int incr) {
		data = new float[size];
		increment = incr;
		sizeMin = size;
	}
	
	public void add(float value) {
		if(nbValues >= data.length) {
			float[] temp = new float[nbValues + increment];
			for(int i = 0; i < nbValues; i++) {
				temp[i] = data[i];
			}	
			data = temp;
		}
		data[nbValues] = value;
		nbValues++;
	}	
	public void clear() {
		nbValues = 0;
	}
	public void reset() {
		clear();
		data = new float[sizeMin];
	}
	public float get(int i) {
		if(i >= nbValues) {
			System.out.print("ERREUR dépassement de la limite dans le vecteur");
		}
		return data[i];
	}
	public int getNbValues() {
		return nbValues;
	}
	
	public void compute_moyenne() {
		moyenne = 0f;
		for(int i = 0; i < nbValues; i++) {
			moyenne+=data[i];
		}
		if(nbValues != 0f) {
			moyenne/=(float)nbValues;
		}
	}
	
	public float get_moyenne() {
		return moyenne;
	}

}
