package main.object3D;

import main.Material;

public class Sphere extends Object3D {
	private float x, y, z, r;
	private float cr = 0f;
	private float cg = 235f;
	private float cb = 0f;
	private int mr = 0;
	private int mg = 20;
	private int mb = 0;

	public Sphere(float x, float y, float z, float r) {
		// ATTENTION, cet objet représente une sphere, mais il est entourer de la POO
		// Pour représenter les vague NE PAS UTILISER CETTE SPHERE. utiliser directement des tableau de floattant pour aller plus vite... on en a besoin
		this.x = x;
		this.y = y;
		this.z = z;
		this.r = r;
//		super.opaque = false;
//		super.refract = true;
//		super.nRefraction = 1.33f;
	}
	public Sphere(float x, float y, float z, float r, float cr, float cg, float cb, int mr, int mg, int mb) {
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
		
		super.haveReflexion = true;
		super.reflexionLevel = 0.8f;
		super.haveReflect = true;
		
		super.material = new Material(1-cr/255f, 1-cg/255f, 1-cb/255f, mr, mg, mb);
	}
	
	public float distance(float x, float y, float z) {
		float x_ = x - this.x;
		float y_ = y - this.y;
		float z_ = z - this.z;
		return (float)Math.sqrt(x_*x_ + y_*y_ + z_*z_) - r;
	}

	

}
