package main.object3D;

import main.LightColor;
import main.Material;

public class TestMaterial extends Material {

	public TestMaterial(float ar, float ag, float ab, float mr, float mg, float mb) {
		super(ar, ag, ab, mr, mg, mb);
		// TODO Auto-generated constructor stub
	}
	
	public LightColor getShadeColor() {
		return new LightColor(255, 0, 0);
	}

}
