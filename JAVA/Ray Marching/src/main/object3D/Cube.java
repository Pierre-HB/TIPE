package main.object3D;

public class Cube extends Object3D {
	private float x, y, z, r;
	private float cr = 0f;
	private float cg = 235f;
	private float cb = 0f;
	private int mr = 0;
	private int mg = 20;
	private int mb = 0;

	public Cube(float x, float y, float z, float r) {
		// ATTENTION, cet objet représente une sphere, mais il est entourer de la POO
		// Pour représenter les vague NE PAS UTILISER CETTE SPHERE. utiliser directement des tableau de floattant pour aller plus vite... on en a besoin
		this.x = x;
		this.y = y;
		this.z = z;
		this.r = r;
	}
	public Cube(float x, float y, float z, float r, float cr, float cg, float cb, int mr, int mg, int mb) {
		// ATTENTION, cet objet représente une sphere, mais il est entourer de la POO
		// Pour représenter les vague NE PAS UTILISER CETTE SPHERE. utiliser directement des tableau de floattant pour aller plus vite... on en a besoin
		this.x = x;
		this.y = y;
		this.z = z;
		this.r = r;
		this.cr = cr;
		this.cg = cg;
		this.cb = cb;
		this.mg = mg;
		this.mr = mr;
		this.mb = mb;
	}
	
	public float distance(float x, float y, float z) {
		
		
		float x_ = Math.max(0, Math.abs(x - this.x) - r/2);
		float y_ = Math.max(0, Math.abs(y - this.y) - r/2);
		float z_ = Math.max(0, Math.abs(z - this.z) - r/2);
		return (float)Math.sqrt(x_*x_ + y_*y_ + z_*z_);
	}
			
		
	

}
