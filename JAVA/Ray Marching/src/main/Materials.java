package main;

public interface Materials {
	public static Material red() {
		return new Material(0.01f, 0.8f, 0.8f, 30, 20, 20);
	}
	
	public static Material green() {
		return new Material(0.8f, 0.01f, 0.8f, 20, 30, 20);
	}
	
	public static Material blue() {
		return new Material(0.8f, 0.8f, 0.01f, 20, 20, 30);
	}
	
	public static Material white() {
		return new Material(0.02f, 0.02f, 0.02f, 30, 30, 30);
	}
	
	public static Material black() {
		return new Material(0.8f, 0.8f, 0.8f, 20, 20, 20);
	}
	
	public static Material grey() {
		return new Material(0.5f, 0.5f, 0.5f, 25, 25, 25);
	}
}
