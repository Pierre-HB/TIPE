package main;

public class Ray {
	private float x, y, z, dx, dy, dz;
	private float distance = 0f;

	public Ray(float x, float y, float z, float dx, float dy, float dz) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.dx = dx;
		this.dy = dy;
		this.dz = dz;
	}
	
	public void march(float a) {
		x += dx*a;
		y += dy*a;
		z += dz*a;
		distance+=a;
	}
	public void setDirection(float dx, float dy, float dz) {
		this.dx = dx;
		this.dy = dy;
		this.dz = dz;
	}
	public void normalizeDirection() {
		float n = (float)Math.sqrt(dx*dx + dy*dy + dz * dz);
		dx /= n;
		dy /= n;
		dz /= n;
		
	}
	public float getX() {
		return x;
	}
	public float getY() {
		return y;
	}
	public float getZ() {
		return z;
	}
	public float getDistance() {
		return distance;
	}
	public Vector3D getPosition() {
		return new Vector3D(x, y, z);
	}
	public Vector3D getDirection() {
		return new Vector3D(dx, dy, dz);
	}
	
	public void print() {
		System.out.print("position : ");
		System.out.print("(");
		System.out.print(x);
		System.out.print(", ");
		System.out.print(y);
		System.out.print(", ");
		System.out.print(z);
		System.out.println(")");
		System.out.print("direction : ");
		System.out.print("(");
		System.out.print(dx);
		System.out.print(", ");
		System.out.print(dy);
		System.out.print(", ");
		System.out.print(dz);
		System.out.println(")");
		
	}

}
