package main.object3D;

import main.LightColor;
import main.Material;

public class PlanMaterial extends Material {
	private float awr, awg, awb;
	private float abr, abg, abb;
	private float mwr, mwg, mwb;
	private float mbr, mbg, mbb;
	private float s = 1f;//dimension des carreaux

	public PlanMaterial() {
		super(0f, 0f, 0f, 0f, 0f, 0f);
		awr = 0.1f;
		awg = 0.1f;
		awb = 0.1f;
		abr = 0.8f;
		abg = 0.8f;
		abb = 0.8f;
		
		mwr = (float)20;
		mwg = (float)20;
		mwb = (float)20;
		mbr = (float)10;
		mbg = (float)10;
		mbb = (float)10;
		
		
	}

	
	private float getCoef(float x, float y, float z) {
		return 1f/Math.max(1f, (float)Math.sqrt((x*x + z*z)/4));
	}
	private float whiteCoef(float x, float y, float z) {
		return 1f+9f*(1f-getCoef(x, y, z));
	}
	private float blackCoef(float x, float y, float z) {
		return 1f+0.25f*(1f-getCoef(x, y, z));
	}
	
	
	
	private boolean isWhite(float x, float y, float z) {
		return (Math.abs((Math.floor(x*s) + Math.floor(z*s))) % 2 == 1);
	}


	
	public float getAR(float x, float y, float z) {
		if (isWhite(x, y, z)) {
			return awr*whiteCoef(x, y, z);
		}
		return abr*blackCoef(x, y, z);
	}
	public float getAG(float x, float y, float z) {
		if (isWhite(x, y, z)) {
			return awg*whiteCoef(x, y, z);
		}
		return abg*blackCoef(x, y, z);
	}
	public float getAB(float x, float y, float z) {
		if (isWhite(x, y, z)) {
			return awb*whiteCoef(x, y, z);
		}
		return abb*blackCoef(x, y, z);
	}

	public LightColor getShadeColor(float x, float y, float z) {
		if (isWhite(x, y, z)) {
			float coef = whiteCoef(x, y, z);
			return new LightColor(mwr*coef, mwg*coef, mwb*coef);
		}
		float coef = blackCoef(x, y, z);
		return new LightColor(mbr*coef, mbg*coef, mbb*coef);
	}
	
}
