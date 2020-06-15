package main;


public class BoolVect {
	private boolean[] data;
	private int nbValues = 0;
	private int increment = 5;
	private int sizeMin = 5;
	private boolean isCompute = false;

	public BoolVect() {
		data = new boolean[increment];
	}
	public BoolVect(int size, int incr) {
		data = new boolean[size];
		increment = incr;
		sizeMin = size;
	}
	
	public void add(boolean value) {
		if(nbValues >= data.length) {
			boolean[] temp = new boolean[nbValues + increment];
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
		data = new boolean[sizeMin];
	}
	public boolean get(int i) {
		if(i >= nbValues) {
			System.out.print("ERREUR dépassement de la limite dans le vecteur");
		}
		return data[i];
	}
	public int getNbValues() {
		return nbValues;
	}
	
	public void setCompute(boolean compute) {
		isCompute = compute;
	}
	
	public boolean isCompute() {
		return isCompute;
	}

}
