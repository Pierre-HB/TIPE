package main;

public class Vector3D {

	private float x, y, z;
	
	static public Vector3D add(Vector3D v1, Vector3D v2) {
		return new Vector3D(v1.getX()+v2.getX(), v1.getY()+v2.getY(), v1.getZ()+v2.getZ());
	}
	static public Vector3D sub(Vector3D v1, Vector3D v2) {
		return new Vector3D(v1.getX()-v2.getX(), v1.getY()-v2.getY(), v1.getZ()-v2.getZ());
	}
	static public Vector3D dot(float a, Vector3D vector) {
		return new Vector3D(a*vector.getX(), a*vector.getY(), a*vector.getZ());
	}
	static public Vector3D product(Vector3D v1, Vector3D v2) {
		return new Vector3D(v1.getY()*v2.getZ()-v1.getZ()*v2.getY(), v1.getZ()*v2.getX()-v1.getX()*v2.getZ(), v1.getX()*v2.getY()-v1.getY()*v2.getX());
	}
	static public float scalar(Vector3D v1, Vector3D v2) {
		return v1.getX()*v2.getX() + v1.getY()*v2.getY() + v1.getZ()*v2.getZ();
	}
	
	public Vector3D(float x, float y, float z) {
		this.x = x;
		this.y = y;
		this.z = z;
	}
	public Vector3D(double x, double y, double z) {
		this.x = (float)x;
		this.y = (float)y;
		this.z = (float)z;
	}
	public void add(Vector3D vector) {
		x+=vector.getX();
		y+=vector.getY();
		z+=vector.getZ();
	}
	public void sub(Vector3D vector) {
		x-=vector.getX();
		y-=vector.getY();
		z-=vector.getZ();
	}
	public void dot(float a) {
		x*=a;
		y*=a;
		z*=a;
	}
	public void product(Vector3D vector) {
		float x_ = this.y*vector.getZ() - this.z*vector.getY();
		float y_ = this.z*vector.getX() - this.x*vector.getZ();
		float z_ = this.x*vector.getY() - this.y*vector.getX();
		
		this.x = x_;
		this.y = y_;
		this.z = z_;
	}
	public float norme() {
		return (float)Math.sqrt(x*x + y*y + z*z);
	}
	public void normalize() {
		float n = this.norme();
		x/=n;
		y/=n;
		z/=n;
	}
	
	public void print() {
		System.out.print("(");
		System.out.print(x);
		System.out.print(", ");
		System.out.print(y);
		System.out.print(", ");
		System.out.print(z);
		System.out.println(")");
	}
	
	

	public float getY() {
		return y;
	}
	public void setY(float y) {
		this.y = y;
	}
	public float getX() {
		return x;
	}
	public void setX(float x) {
		this.x = x;
	}
	public float getZ() {
		return z;
	}
	public void setZ(float z) {
		this.z = z;
	}

}
