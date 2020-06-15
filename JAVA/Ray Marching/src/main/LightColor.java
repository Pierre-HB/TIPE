package main;

public class LightColor {
	private float r, g, b;
	private float initR, initG, initB;

	public LightColor(float r, float g, float b) {
		this.r = r;
		this.g = g;
		this.b = b;
		initR = r;
		initG = g;
		initB = b;
	}
	
	public void reset() {
		r = initR;
		g = initG;
		b = initB;
	}
	public LightColor duplicate() {
		return new LightColor(initR, initG, initB);
	}
	
	public float getR() {
		return r;
	}
	public float getG() {
		return g;
	}
	public float getB() {
		return b;
	}
	
	public void hitMaterial(Material material, float s, float x, float y, float z) {
		r = r*s*(1f-material.getAR(x, y, z));
		g = g*s*(1f-material.getAG(x, y, z));
		b = b*s*(1f-material.getAB(x, y, z));
	}
	
	public void fusionLight(LightColor light, float s) {
		r = s*r + (1f - s)*light.getR();
		g = s*g + (1f - s)*light.getG();
		b = s*b + (1f - s)*light.getB();		
	}
	
	public int getColor() {
		return ((int)r & 255) << 16 | ((int)g & 255) << 8 | ((int)b & 255);
	}

}
