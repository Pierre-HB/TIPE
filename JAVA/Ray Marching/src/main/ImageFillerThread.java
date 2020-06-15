package main;

import java.awt.image.BufferedImage;

import main.object3D.Object3D;

public class ImageFillerThread extends Thread {
	private int offset, pas;
	private BufferedImage buffer;
	private int delta = 10;
	private Vector3D reflexionDirection = new Vector3D(0f, 0f, 0f);
	private Vector3D refractionDirection = new Vector3D(0f, 0f, 0f);
	private int maxReccursion;
	private int[] lastNearestObj = {0};
	private Scene scene;
	private static final int maxMarch = 1000;
	private static final float dMax = 40f;
	private static final float dMin = -40f;
	
	public ImageFillerThread(int pas, int offset, int maxReccursion, BufferedImage buffer, Scene scene) {
		this.pas = pas;
		this.offset = offset;
		this.buffer = buffer;
		this.maxReccursion = maxReccursion;
		this.scene = scene;
	}
	
	public boolean rayCast(Ray ray, boolean onlyOpaque, int[] lastNearestObj) {
    	boolean hit = false;
		float d = 0;
		int march = 0;
		Timer.nbRayCast+=1;
    	while(!hit && march < maxMarch && ray.getX() > dMin && ray.getX() < dMax && ray.getY() > dMin && ray.getY() < dMax && ray.getZ() > dMin && ray.getZ() < dMax) {
    		d = scene.distanceMin(ray.getX(), ray.getY(), ray.getZ(), ray.getDirection(), onlyOpaque, lastNearestObj);
    		hit = Math.abs(d) < Main.epsilon;
			ray.march(d);
			march++;
			Timer.nbSteps+=1;
    	}
    	return hit;
    }
	
	private LightColor getLightColor(Ray ray, float n1, boolean onlyOpaque, int recurtionLevel) {
    	LightColor l = scene.getLightColor().duplicate();
    	Vector3D light = scene.getLight();
    	if(recurtionLevel == 0) {
    		//On termine l'agorithme
    		return l;
    	}
    	
    	boolean hit = rayCast(ray, onlyOpaque, lastNearestObj);
    	
    	if (hit) {
			Object3D obj = scene.getObject(lastNearestObj[0]);
			
			Vector3D n = obj.getNormal(ray.getX(), ray.getY(), ray.getZ());
			n.normalize();
			
			//on recul un peux pour que l'objet ne soit pas a l'ombre de lui même
			ray.march(-Main.epsilon*delta);
			Ray isInLight = new Ray(ray.getX(), ray.getY(), ray.getZ(), -light.getX(), -light.getY(), -light.getZ());	
			boolean inDark = rayCast(isInLight, true, lastNearestObj);
			ray.march(Main.epsilon*delta);	
			
			
			if(inDark) {		
				l = obj.getShadeColor(ray.getX(), ray.getY(), ray.getZ()).duplicate();	
			}
			else {
				l.hitMaterial(obj.getMaterial(), ImageMaker.illumination(light, n), ray.getX(), ray.getY(), ray.getZ());
			}
			
			
			//refraction
			if(obj.haveRefraction()) {
				boolean notRefractionTotal = Scene.refraction(ray.getDirection(), n, n1, obj.getRefractionIndice(), refractionDirection);
				if(notRefractionTotal) {
					Ray refraction = new Ray(ray.getX(), ray.getY(), ray.getZ(), refractionDirection.getX(), refractionDirection.getY(), refractionDirection.getZ());
					refraction.march(Main.epsilon*delta);
					l.fusionLight(getLightColor(refraction, obj.getRefractionIndice(), true, recurtionLevel-1), obj.getRefractionLevel());
				}
			}	
			
			
			//reflection
			if(obj.haveReflect() || obj.haveReflexion()) {
				Scene.reflexion(ray.getDirection(), n, reflexionDirection);
			}
			
			if(obj.haveReflexion()) {				
				Ray reflexion = new Ray(ray.getX(), ray.getY(), ray.getZ(), reflexionDirection.getX(), reflexionDirection.getY(), reflexionDirection.getZ());
				reflexion.march(Main.epsilon*delta);
				
				l.fusionLight(getLightColor(reflexion, n1, false, recurtionLevel-1), obj.getReflexionLevel());
			}
			
			
			//reflet
			if(obj.haveReflect()) {				
				l.fusionLight(scene.getLightColor().duplicate(), ImageMaker.reflet(reflexionDirection, light));
			}			
    	}else {
			l = scene.getSkyColor().duplicate();;
		}
    	return l;
    }
	
	public void run() {
		for(int x = offset; x < ImageMaker.camera.getWidth(); x += pas) {
			for(int y = 0; y < ImageMaker.camera.getHeight(); y++) {
				Ray ray = ImageMaker.camera.getRay(x, y);
				buffer.setRGB(x, y, getLightColor(ray, scene.getRefractionIndice(), false, maxReccursion).getColor());	
				ImageMaker.addPixel();
			}
		}
		ImageMaker.endThread();
	}

}
