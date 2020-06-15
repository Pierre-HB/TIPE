package main.object3D;

import main.LightColor;
import main.Material;
import main.Vector3D;

public class Plan extends Object3D {
	private boolean x, y, z;
	private float p;
	private float cr = 0f;
	private float cg = 235f;
	private float cb = 0f;
	private int mr = 0;
	private int mg = 20;
	private int mb = 0;
	private boolean damier = false;
	private Material white = new Material(0.1f, 0.1f, 0.1f, 50f, 50f, 50f);
	private Material black = new Material(0.8f, 0.8f, 0.8f, 20f, 20f, 20f);

	public Plan(boolean x, boolean y, boolean z, float p, float cr, float cg, float cb, int mr, int mg, int mb) {
		// ATTENTION, cet objet représente une sphere, mais il est entourer de la POO
		// Pour représenter les vague NE PAS UTILISER CETTE SPHERE. utiliser directement des tableau de floattant pour aller plus vite... on en a besoin
		this.x = x;
		this.y = y;
		this.z = z;
		this.p = p;
		this.cr = cr;
		this.cg = cg;
		this.cb = cb;
		this.mg = mg;
		this.mr = mr;
		this.mb = mb;
		super.material = new PlanMaterial();
	}
	
	public Plan(float p) {
		this.p = p;
		this.x = false;
		this.y = true;
		this.z = false;
		this.damier = true;
		super.material = new PlanMaterial();
	}
	
	public Plan(boolean x, boolean y, boolean z, float p) {
		// ATTENTION, cet objet représente une sphere, mais il est entourer de la POO
		// Pour représenter les vague NE PAS UTILISER CETTE SPHERE. utiliser directement des tableau de floattant pour aller plus vite... on en a besoin
		this.x = x;
		this.y = y;
		this.z = z;
		this.p = p;
		super.material = new PlanMaterial();
	}
	
	public float distance(float x, float y, float z) {
		float d = 0;
		if (this.x){
			d+=Math.abs(x-p);
		}
		if (this.y){
			d+=Math.abs(y-p);
		}
		if (this.z){
			d+=Math.abs(z-p);
		}
		return d;
	}
	
	public Vector3D getNormal(float x, float y, float z) {
		return new Vector3D(0, 1f, 0);
	}
	

}

