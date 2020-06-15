package main;

public class Material {
	private float ar, ag, ab, mr, mg, mb;

	public Material(float ar, float ag, float ab, float mr, float mg, float mb) {
		this.ar = ar;
		this.ag = ag;
		this.ab = ab;
		this.mg = mg;
		this.mr = mr;
		this.mb = mb;
	
	}
	
	public LightColor getColor(LightColor light, float s) {
		return light;
	}
	public float getAR(float x, float y, float z) {
		return ar;
	}
	public float getAG(float x, float y, float z) {
		return ag;
	}
	public float getAB(float x, float y, float z) {
		return ab;
	}
	
	public LightColor getShadeColor(float x, float y, float z) {
		return new LightColor(mr, mg, mb);
	}

}
