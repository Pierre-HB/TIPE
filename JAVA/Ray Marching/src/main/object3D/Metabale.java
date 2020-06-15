package main.object3D;

public class Metabale extends Object3D {
	private float x, y, z, r;
	private float x2, y2, z2, r2;
	private float x3, y3, z3, r3;
	private float rmax;
	private float cr = 0f;
	private float cg = 235f;
	private float cb = 0f;
	private int mr = 0;
	private int mg = 20;
	private int mb = 0;

	public Metabale() {
		this.x = 0f;
		this.y = 0f;
		this.z = 0f;
		this.r = 0.1f;
		this.x2 = 0f;
		this.y2 = 0f;
		this.z2 = 0.2f;
		this.x3 = 0f;
		this.y3 = 0f;
		this.z3 = -1.5f;
		this.rmax = 1f;
	}

	private float distance_min(float x, float y, float z) {
		float x_1 = x - this.x;
		float y_1 = y - this.y;
		float z_1 = z - this.z;
		float x_2 = x - this.x2;
		float y_2 = y - this.y2;
		float z_2 = z - this.z2;
		float x_3 = x - this.x3;
		float y_3 = y - this.y3;
		float z_3 = z - this.z3;
		float r_1 = (float)Math.sqrt(x_1*x_1 + y_1*y_1 + z_1*z_1) - r;
		float r_2 = (float)Math.sqrt(x_2*x_2 + y_2*y_2 + z_2*z_2) - r;
		float r_3 = (float)Math.sqrt(x_3*x_3 + y_3*y_3 + z_3*z_3) - r;
		return Math.min(r_1, Math.min(r_2, r_3));
	}
	
	private float distance_meta(float x, float y, float z) {
		float x_1 = x - this.x;
		float y_1 = y - this.y;
		float z_1 = z - this.z;
		float x_2 = x - this.x2;
		float y_2 = y - this.y2;
		float z_2 = z - this.z2;
		float x_3 = x - this.x3;
		float y_3 = y - this.y3;
		float z_3 = z - this.z3;
		float r_1 = (float)Math.sqrt(x_1*x_1 + y_1*y_1 + z_1*z_1);

		
		
		
		
		float r_2 = (float)Math.sqrt(x_2*x_2 + y_2*y_2 + z_2*z_2);
		float r_3 = (float)Math.sqrt(x_3*x_3 + y_3*y_3 + z_3*z_3);
		
		return 1/((1/r_1)+(1/r_2)+(1/r_3)) - r/1f;
	}
	
	private float distance_meta_2(float x, float y, float z) {
		float x_1 = x - this.x;
		float y_1 = y - this.y;
		float z_1 = z - this.z;
		float x_2 = x - this.x2;
		float y_2 = y - this.y2;
		float z_2 = z - this.z2;
		float x_3 = x - this.x3;
		float y_3 = y - this.y3;
		float z_3 = z - this.z3;
		float r_1 = x_1*x_1 + y_1*y_1 + z_1*z_1;
		float r_2 = x_2*x_2 + y_2*y_2 + z_2*z_2;
		float r_3 = x_3*x_3 + y_3*y_3 + z_3*z_3;
		
		float s = 0;
		s+= 1/r_1;
		s+= 1/r_2;
		s+= 1/r_3;
		
		return (float)Math.sqrt(1/s) - r;
	}
	
	private static float f(float x) {
		return 1f-3f*x*x+2f*x*x*x;
	}
	private static float g(float x, float a, float b) {
		return (x-a)/(b-a);
	}
	
	private static float h(float x, float a) {
		
		return 1+(float)Math.tan(Math.PI*x/(2*a));
	}
	
	private float distance_meta_3(float x, float y, float z) {
		float x_1 = x - this.x;
		float y_1 = y - this.y;
		float z_1 = z - this.z;
		float x_2 = x - this.x2;
		float y_2 = y - this.y2;
		float z_2 = z - this.z2;
		float x_3 = x - this.x3;
		float y_3 = y - this.y3;
		float z_3 = z - this.z3;
		float r_1 = x_1*x_1 + y_1*y_1 + z_1*z_1;
		float r_2 = x_2*x_2 + y_2*y_2 + z_2*z_2;
		float r_3 = x_3*x_3 + y_3*y_3 + z_3*z_3;
		
		float s = 0f;
		float rcarre = rmax*rmax;
		if (r_1 < rcarre){
			s+=f(r_1/rcarre)/r_1;
		}
		if (r_2 < rcarre){
			s+=f(r_2/rcarre)/r_2;
		}
		if (r_3 < rcarre){
			s+=f(r_3/rcarre)/r_3;
		}
		if(s == 0f) {
			return rmax;
		}
		return (float)Math.sqrt(1/s) - r;
		
	}
	
	private float distance_meta_4(float x, float y, float z) {
		float x_1 = x - this.x;
		float y_1 = y - this.y;
		float z_1 = z - this.z;
		float x_2 = x - this.x2;
		float y_2 = y - this.y2;
		float z_2 = z - this.z2;
		float x_3 = x - this.x3;
		float y_3 = y - this.y3;
		float z_3 = z - this.z3;
		float r_1 = x_1*x_1 + y_1*y_1 + z_1*z_1;
		float r_2 = x_2*x_2 + y_2*y_2 + z_2*z_2;
		float r_3 = x_3*x_3 + y_3*y_3 + z_3*z_3;
		
		float s = 0f;
		float rcarre = rmax*rmax;
		float off = 1/rcarre;
		if (r_1 < rcarre){
			s+=1/r_1 - off;
		}
		if (r_2 < rcarre){
			s+=1/r_2 - off;
		}
		if (r_3 < rcarre){
			s+=1/r_3 - off;
		}
		if(s == 0f) {
			return rmax;
		}
		return (float)Math.sqrt(1/s) - r;
		
	}
	
	
	private float distance_meta_5(float x, float y, float z) {
		float x_1 = x - this.x;
		float y_1 = y - this.y;
		float z_1 = z - this.z;
		float x_2 = x - this.x2;
		float y_2 = y - this.y2;
		float z_2 = z - this.z2;
		float x_3 = x - this.x3;
		float y_3 = y - this.y3;
		float z_3 = z - this.z3;
		float r_1 = x_1*x_1 + y_1*y_1 + z_1*z_1;
		float r_2 = x_2*x_2 + y_2*y_2 + z_2*z_2;
		float r_3 = x_3*x_3 + y_3*y_3 + z_3*z_3;
		
		float s = 0f;
		float rcarre = rmax*rmax;
		if (r_1 < rcarre){
			s+=1/h(r_1,rcarre);
		}
		if (r_2 < rcarre){
			s+=1/h(r_2,rcarre);
		}
		if (r_3 < rcarre){
			s+=1/h(r_3,rcarre);
		}
		if(s == 0f || s >= rmax) {
			return rmax;
		}
		return (float)Math.sqrt(1/s) - r;
		
	}
	
	private float distance_meta_6(float x, float y, float z) {
		float x_1 = x - this.x;
		float y_1 = y - this.y;
		float z_1 = z - this.z;
		float x_2 = x - this.x2;
		float y_2 = y - this.y2;
		float z_2 = z - this.z2;
		float x_3 = x - this.x3;
		float y_3 = y - this.y3;
		float z_3 = z - this.z3;
		float r_1 = x_1*x_1 + y_1*y_1 + z_1*z_1;
		float r_2 = x_2*x_2 + y_2*y_2 + z_2*z_2;
		float r_3 = x_3*x_3 + y_3*y_3 + z_3*z_3;
		
		float s = 0f;
		float s_ = 0;
		float rcarre = rmax*rmax;
		if (r_1 < rcarre){
			s+=f(r_1/rcarre)/r_1;
			s_+=f(r_1/rcarre);
		}
		if (r_2 < rcarre){
			s+=f(r_2/rcarre)/r_2;
			s_+=f(r_2/rcarre);
		}
		if (r_3 < rcarre){
			s+=f(r_3/rcarre)/r_3;
			s_+=f(r_3/rcarre);
		}
		if(s == 0f) {
			return rmax;
		}
		return (float)Math.sqrt(s_/s) - r;
		
	}
	
	
	public float distance(float x, float y, float z) {
		return distance_meta_6(x, y, z);

	}

	public int color(float x, float y, float z, float light) {
		return (mr+(int) (light*cr)) << 16 | (mg+(int) (light*cg)) << 8 | (mb+(int) (light*cb));
	}

	public int shadColor(float x, float y, float z) {
		return mr << 16 | mg<<8 | mb;
	}
	

}
