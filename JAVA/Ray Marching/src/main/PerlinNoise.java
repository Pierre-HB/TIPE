package main;

public class PerlinNoise {
	private int[] numbers = new int[256];
	private float[][] vectors = {{1, 0},{-1, 0},{0, 1},{0, -1}, {1.4142135f, 1.4142135f}, {-1.4142135f, 1.4142135f}, {1.4142135f, -1.4142135f}, {-1.4142135f, -1.4142135f}};
	private float res;
	
	public static int randint(int a, int b) {
		return (int)(Math.random()*(b-a)+a);
	}

	public PerlinNoise(float res) {
		this.res = res;
		for(int i = 0; i < 256; i++) {
			numbers[i] = i;
		}
		//On mélange les nombre pour avoir une distribution aléatoire
		int a, b, temp;
		for(int i = 0; i < 512; i++) {
			a = randint(0, 256);
			b = randint(0, 256);
			temp = numbers[a];
			numbers[a] = numbers[b];
			numbers[b] = temp;
		}
		
	}
	
	private float[] getVect(int x, int y) {
		return vectors[numbers[(y + numbers[x%256])%256]%8];
	}
	private static float scalar(float x, float y, float[] v) {
		return x*v[0] + y*v[1];
	}
	
	public float getNoise(float x, float y) {
		x/=res;
		y/=res;
		float dx = x - (int)x;
		float dy = y - (int)y;
		
		
		
		float s00 = scalar(dx, dy, getVect((int)x, (int)y));
		float s10 = scalar(1f-dx, dy, getVect((int)x+1, (int)y));
		float s01 = scalar(dx, 1f-dy, getVect((int)x, (int)y+1));
		float s11 = scalar(1f-dx, 1f-dy, getVect((int)x+1, (int)y+1));
		
		dx = 3*dx*dx - 2*dx*dx*dx;
		dy = 3*dy*dy - 2*dy*dy*dy;
		
		float l1 = dx*s10 + (1f-dx)*s00;
		float l2 = dx*s11 + (1f-dx)*s01;
		
		return dy*l2 + (1f-dy)*l1;

	}

}
