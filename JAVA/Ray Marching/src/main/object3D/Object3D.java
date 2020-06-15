package main.object3D;

import main.LightColor;
import main.Main;
import main.Material;
import main.Vector3D;

public abstract class Object3D {
	protected static final float epsilon10 = Main.epsilon/10f;
	protected Material material;
	protected boolean opaque = true;
	protected boolean refract = false;
	protected float nRefraction = 1f;
	protected boolean haveReflexion = false;
	protected float reflexionLevel = 0.5f;
	protected boolean haveReflect = false;
	protected float refractionLevel = 0.5f;

	public Object3D() {
		material = new Material(0.5f, 0.5f, 0.5f, 20f, 20f, 20f);
		// TODO Auto-generated constructor stub
	}
	
	public abstract float distance(float x, float y, float z);
	public float distanceAdvanced(float x, float y, float z, Vector3D direction) {
		return distance(x, y, z);
	}
	
	public float distance_opti(float x, float y, float z, Vector3D direction) {
		return this.distance(x, y, z);
	}
	
	public float getRefractionLevel() {
		return refractionLevel;
	}
	
	public LightColor getShadeColor(float x, float y, float z) {
		return material.getShadeColor(x, y, z);
	}
	public Material getMaterial() {
		return material;
	}
	
	public Vector3D getNormal(float x, float y, float z) {
		float d = distance(x, y, z);
		return new Vector3D(d-distance(x-epsilon10, y, z), d-distance(x, y-epsilon10, z), d-distance(x, y, z-epsilon10));
	}
	
	public boolean isOpaque() {
		return opaque;
	}
	public boolean haveRefraction() {
		return refract;
	}
	public boolean haveReflexion() {
		return haveReflexion;
	}
	public boolean haveReflect() {
		return haveReflect;
	}
	public float getReflexionLevel() {
		return reflexionLevel;
	}
	public float getRefractionIndice() {
		return nRefraction;
	}

}
