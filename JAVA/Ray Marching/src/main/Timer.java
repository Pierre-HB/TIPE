package main;

public class Timer {

	private static long[] duree;
	private static long[] starts;
	private static String[] names;
	private static int nbChanel;
	private static boolean on = true;
	
	public static long nbSteps = 0L;
	public static long nbRayCast = 0L; 
	public static long nbStepsInWater = 0L;
	public static long nbRayCastInWater = 0L; 
	public static long nbDichotomie = 0L;
	
	public static void initialize(int nbDuree) {
		duree = new long[nbDuree];
		starts = new long[nbDuree];
		names = new String[nbDuree];
		nbChanel = nbDuree;
		for (int i = 0; i < nbDuree; i++) {
			duree[i] = 0L;
			names[i] = "";
		}
	}
	
	public static void start(int chanel) {
		if(on) {
			starts[chanel] = System.nanoTime();
		}
	}
	public static void stop(int chanel) {
		if(on) {
			duree[chanel] += System.nanoTime() - starts[chanel];
		}
	}
	public static void nameChanel(int chanel, String name) {
		names[chanel] = name;
	}
	public static void off() {
		on = false;
	}
	public static void on() {
		on = true;
	}
	public static void displayNumber(long nb) {
		if(nb < 1000L) {
			System.out.print(nb);
		}
		else {
			displayNumber(nb / 1000);
			System.out.print(" ");
			System.out.print(nb % 999);
		}
		
	}
	
	public static void displayTime() {
		System.out.println("");
		for (int i = 0; i < nbChanel; i++) {
			System.out.print("chanel : ");
			System.out.print(names[i]);
			System.out.println(" : ");
			displayNumber(duree[i]);
			System.out.println("ns");
		}

	}
	

}
