package main;

import main.object3D.Object3D;
import main.Vector3D;



public class Scene {
	private Object3D[] objects = new Object3D[2000];
	private Vector3D light;
	private int nbObjects = 0;
	
	private LightColor lightColor = new LightColor(230f, 230f, 230f);
	private LightColor sky = new LightColor(0f, 10f, 0f);
	private float nRefraction = 1f;
	

	public Scene(Vector3D light) {
		this.light = light;
		this.light.normalize();
	}
	public Vector3D getLight() {
		return light;
	}
	public float distanceMin(float x, float y, float z, boolean onlyOpaque, int[] lastNearestObj) {
		float d = objects[0].distance(x, y, z);
		float temp;
		lastNearestObj[0] = 0;
		for(int i = 1; i < nbObjects; i++) {
			if(!onlyOpaque || objects[i].isOpaque()) {
				temp = objects[i].distance(x, y, z);
				if(temp < d) {
					d = temp;
					lastNearestObj[0] = i;
				}
			}
		}
		return d;
	}
	public float distanceMin(float x, float y, float z, Vector3D direction, boolean onlyOpaque, int[] lastNearestObj) {
		float d = objects[0].distanceAdvanced(x, y, z, direction);
		float temp;
		lastNearestObj[0] = 0;
		for(int i = 1; i < nbObjects; i++) {
			if(!onlyOpaque || objects[i].isOpaque()) {
				temp = objects[i].distanceAdvanced(x, y, z, direction);
				if(temp < d) {
					d = temp;
					lastNearestObj[0] = i;
				}
			}
		}
		return d;
	}
	
	public void addObject(Object3D obj) {
		//Il faut veiller a ce que le premier objet ajoute soit opaque
		objects[nbObjects] = obj;
		nbObjects++;
	}
	public Object3D getObject(int i) {
		return objects[i];
	}
	public LightColor getLightColor() {
		return lightColor;
	}
	public LightColor getSkyColor() {
		return sky;
	}
	public float getRefractionIndice() {
		return nRefraction;
	}
	public void setRefractionIndice(float n) {
		nRefraction = n;
	}
	public void setLightColor(LightColor light) {
		lightColor = light;
	}
	public void setSkyColor(LightColor sky) {
		this.sky = sky;
	}
	
	
	public static float norme(float[] v) {
		return (float)Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
	}
	public static float norme(float x, float y, float z) {
		return (float)Math.sqrt(x*x + y*y + z*z);
	}
	public static void scale(float alpha, float[] v) {
		v[0]*=alpha;
		v[1]*=alpha;
		v[2]*=alpha;
	}
	
	public static void rotation_axe(float[] u, float theta, float[][] rot) {
		//modifie la matrice rot pour quelle corresponde a la reotation d'angle theta par rapport a u
		float c = (float)Math.cos(theta);
		float s = (float)Math.sin(theta);
		
		float x = u[0];
		float y = u[1];
		float z = u[2];

		rot[0][0] = x*x*(1-c) + c;
		rot[0][1] = x*y*(1-c) - z*s;
		rot[0][2] = x*z*(1-c) + y*s;

		rot[1][0] = x*y*(1-c) + z*s;
		rot[1][1] = y*y*(1-c) + c;
		rot[1][2] = y*z*(1-c) - x*s;

		rot[2][0] = x*z*(1-c) - y*s;
		rot[2][1] = y*z*(1-c) + x*s;
		rot[2][2] = z*z*(1-c) + c;		
	}
	
	public static void vector_product(float[] v1, float[] v2, float[] u) {
		//u prend le résultat du produit vectoriel entre v1 et v2
		u[0] = v1[1]*v2[2] - v1[2]*v2[1];
		u[1] = v1[2]*v2[0] - v1[0]*v2[2];
		u[2] = v1[0]*v2[1] - v1[1]*v2[0];
	}
	
	public static float scalar(float[] u, float[] v) {
		return u[0]*v[0] + u[1]*v[1] + u[2]*v[2];
	}
	
	public static void matrice_product(float[][] rot, float[] v, float[] u) {
		//u prend le resultat du produit matricielle entre la matrice de rotation rot et le vecteur v
		for(int i = 0; i < 3; i++) {
			u[i] = rot[i][0]*v[0] + rot[i][1]*v[1] + rot[i][2]*v[2];
		}
	}
	

	public static boolean refraction(Vector3D v, Vector3D n, float n1, float n2, Vector3D refracter) {
		//renvoi la direction du rayon refracter a l'interface de deux mileux d'indice de refraction n1 et n2
		// et ayant une normal n avec un rayon incidant v
		float[][] rot = {{0f, 0f, 0f}, {0f, 0f, 0f}, {0f, 0f, 0f}};
		float[] v_ = {v.getX(), v.getY(), v.getZ()};
		float[] n_ = {-n.getX(), -n.getY(), -n.getZ()};
		//on travaille avec l'opposé du vecteur normal
		//faire shema, ça marche mieux
		float[] u = {0f, 0f, 0f};
		float[] ortho = {0f, 0f, 0f};
		
		vector_product(n_, v_, ortho);
		float sin_i1 = norme(ortho);
		float sin_i2 = n1*sin_i1/n2;
		scale(1f/sin_i1, ortho);
		rotation_axe(ortho, (float)Math.asin(sin_i2), rot);
		matrice_product(rot, n_, u);
		refracter.setX(u[0]);
		refracter.setY(u[1]);
		refracter.setZ(u[2]);
		
		return -1f < sin_i2 && sin_i2 < 1f;		
	}
	
	public static boolean reflexion(Vector3D v, Vector3D n, Vector3D reflexion) {
		float s = Vector3D.scalar(v,  n);
		reflexion.setX(v.getX() - 2*s*n.getX());
		reflexion.setY(v.getY() - 2*s*n.getY());
		reflexion.setZ(v.getZ() - 2*s*n.getZ());
		
		return true;
	}
}
