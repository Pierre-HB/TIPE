package main;
import main.Ray;

public class Camera {
	private float nx, ny;
	private int width, height;
	private Vector3D o, u, v, e;
	
	public Camera(Vector3D o, Vector3D direction, Vector3D up,float ouverture, int width, int height) {
		if(width < height) {
			ny = 1f;
			nx = (float)width/(float)height;
		}
		else {
			nx = 1f;
			ny = (float)height/(float)width;
		}
		
		Vector3D horizontal = Vector3D.product(direction, up);
		
		horizontal.normalize();
		direction.normalize();
		up.normalize();
		
		horizontal.dot(nx);
		up.dot(-ny);		
		
		float d = nx/(2*(float)Math.tan(ouverture/2));

		this.u = horizontal;
		this.v = up;
		this.o = o;
		this.width = width;
		this.height = height;		
		
		float ex = o.getX() + d*direction.getX() - u.getX()/2 - v.getX()/2;
		float ey = o.getY() + d*direction.getY() - u.getY()/2 - v.getY()/2;
		float ez = o.getZ() + d*direction.getZ() - u.getZ()/2 - v.getZ()/2;
		e = new Vector3D(ex, ey, ez);
	}
	
	public Ray getRay(int x, int y) {
		float px = e.getX() + u.getX()*(float)x/(float)this.width + v.getX()*(float)y/(float)this.height;
		float py = e.getY() + u.getY()*(float)x/(float)this.width + v.getY()*(float)y/(float)this.height;
		float pz = e.getZ() + u.getZ()*(float)x/(float)this.width + v.getZ()*(float)y/(float)this.height;
		
		Ray ray = new Ray(px, py, pz, px - o.getX(), py - o.getY(), pz - o.getZ());
		ray.normalizeDirection();
		return ray;
	}
	public int getWidth() {
		return width;
	}
	public int getHeight() {
		return height;
	}
	public Vector3D getOrigine() {
		return o;
	}

}
