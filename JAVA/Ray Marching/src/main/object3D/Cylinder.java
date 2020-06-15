package main.object3D;

import main.Material;
import main.Vector3D;

public class Cylinder extends Object3D {

	private Vector3D o1, o2, u;
	private float l;
	private float r;
	

	public Cylinder(Vector3D o1, Vector3D o2, float r, Material material) {
		super.material = material;
		
		super.opaque = true;
		super.haveReflexion = false;
		super.reflexionLevel = 0.6f;
		super.haveReflect = false;
		super.refract = false;
		super.refractionLevel = 0.5f;
		
		this.o1 = o1;
		this.o2 = o2;
		this.r = r;
		u = Vector3D.sub(o2, o1);
		l = u.norme();
		u.normalize();
	}

	@Override
	public float distance(float x, float y, float z) {
		Vector3D v = new Vector3D(x, y, z);
		Vector3D v_ = Vector3D.sub(v, o1);
		float h = 0;
		float s = Vector3D.scalar(u,  v_);
		if (s < 0) {
			h = -s;
		}
		else if(s > l) {
			h = s-l;
		}
		float l;
		l = Math.max(0, Vector3D.sub(v_, Vector3D.dot(s, u)).norme() - r);		
		return  (float) Math.sqrt(h*h + l*l);
	}
	

}