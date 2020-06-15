package main;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import main.object3D.Object3D;

public class ImageMaker {
	public Scene scene;
	public static Camera camera;
	private static final float epsilon_ = Main.epsilon;
	private static final int maxMarch = 1000;
	private static final float dMax = 40f;
	private static final float dMin = -40f;
	private static final float delta = 10f;	
	private Vector3D reflexionDirection = new Vector3D(0f, 0f, 0f);
	private Vector3D refractionDirection = new Vector3D(0f, 0f, 0f);
	private int[] lastNearestObj = {0};
	
	private static int threadsEnd = 0;
	private static long nbPixels = 0;

	public ImageMaker(Scene scene, Camera camera) {
		ImageMaker.camera = camera;
		this.scene = scene;
	}
	public static float illumination(Vector3D light, Vector3D n) {
		return Math.max(0f, -Vector3D.scalar(light, n));
	}
	public static float reflet(Vector3D reflexionDirection, Vector3D light) {
		float s = (float)Math.exp(-Vector3D.scalar(reflexionDirection,  light)-1);
		return 1-s*s*s*s*s;
	}
	
	public static BufferedImage artifact_filter(BufferedImage image, float[][] kernel, float thresold) {
		int width = image.getWidth();
		int height = image.getHeight();
		int n = kernel.length/2;
		float coefTotal = 0f;
		for(int x = 0; x < kernel.length; x++) {
			for(int y = 0; y < kernel.length; y++) {
				coefTotal += kernel[x][y];
			}
		}
		
		
		BufferedImage buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		float r = 0f;
		float g = 0f;
		float b = 0f;
		int p;
		for(int i = n; i < width-n; i++) {
			for(int j = n; j < height - n; j++) {
				if(i >=n && i < width - n && j >= n && j < height - n) {
					r = 0f;
					g = 0f;
					b = 0f;
					for(int x = 0; x < kernel.length; x++) {
						for(int y = 0; y < kernel.length; y++) {
							p = image.getRGB(i+n-x, j+n-y);
							r+= kernel[x][y]*(float)((p >> 16) & 255);
							g+= kernel[x][y]*(float)((p >> 8) & 255 );
							b+= kernel[x][y]*(float)(p & 255);
						}
					}
					p = image.getRGB(i, j);
					r/=coefTotal;
					g/=coefTotal;
					b/=coefTotal;
					if(r > 255f) {
						System.out.print(" r : ");
						System.out.println(r);
					}
					if(g > 255f) {
						System.out.print(" g : ");
						System.out.println(g);
					}
					if(b > 255f) {
						System.out.print(" b : ");
						System.out.println(b);
					}
					if(Math.abs((float)((p >> 16) & 255) - r) > thresold) {
						p &= (255 << 8) | 255;
						p |= (((int)r) & 255) << 16;
					}
					if(Math.abs((float)((p >> 8) & 255) - g) > thresold) {
						p &= (255 << 16) | 255;
						p |= (((int)g) & 255) << 8;
					}
					if(Math.abs((float)(p & 255) - b) > thresold) {
						p &= (255 << 16) | (255 << 8);
						p |= ((int)b) & 255;
					}
					buffer.setRGB(i, j, p);
				}
			}
		}		
		return buffer;
	}
	
	public static BufferedImage convolution(BufferedImage image, float[][] kernel) {
		int width = image.getWidth();
		int height = image.getHeight();
		int n = kernel.length/2;
		BufferedImage buffer = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_RGB);
		float r = 0f;
		float g = 0f;
		float b = 0f;
		for(int i = n; i < width-n; i++) {
			for(int j = n; j < height - n; j++) {
				if(i >=n && i < width - n && j >= n && j < height - n) {
					r = 0f;
					g = 0f;
					b = 0f;
					for(int x = 0; x < kernel.length; x++) {
						for(int y = 0; y < kernel.length; y++) {
							int p = image.getRGB(i+n-x, j+n-y);
							r+= kernel[x][y]*(float)((p >> 16) & 255);
							g+= kernel[x][y]*(float)((p >> 8) & 255 );
							b+= kernel[x][y]*(float)(p & 255);
						}
					}
					buffer.setRGB(i, j, (Math.abs((int)r) << 16) | (Math.abs((int)g) << 8) | Math.abs((int)b));
				}
				else {
					buffer.setRGB(i, j, image.getRGB(i, j));
				}
			}
		}
		return buffer;
	}

	
    public boolean rayCast(Ray ray, boolean onlyOpaque, int[] lastNearestObj) {
    	Timer.start(0);
    	boolean hit = false;
		float d = 0;
		int march = 0;
		Timer.nbRayCast+=1;
    	while(!hit && march < maxMarch && ray.getX() > dMin && ray.getX() < dMax && ray.getY() > dMin && ray.getY() < dMax && ray.getZ() > dMin && ray.getZ() < dMax) {
    		d = scene.distanceMin(ray.getX(), ray.getY(), ray.getZ(), ray.getDirection(), onlyOpaque, lastNearestObj);
    		hit = Math.abs(d) < epsilon_;
			ray.march(d);
			march++;
			Timer.nbSteps+=1;
    	}
    	Timer.stop(0);
    	return hit;
    }
    
    public LightColor getLightColor(Ray ray, float n1, boolean onlyOpaque, int recurtionLevel) {
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
			ray.march(-epsilon_*delta);
			Ray isInLight = new Ray(ray.getX(), ray.getY(), ray.getZ(), -light.getX(), -light.getY(), -light.getZ());	
			boolean inDark = rayCast(isInLight, true, lastNearestObj);
			ray.march(epsilon_*delta);	
			
			
			if(inDark) {		
				l = obj.getShadeColor(ray.getX(), ray.getY(), ray.getZ()).duplicate();	
			}
			else {
				l.hitMaterial(obj.getMaterial(), illumination(light, n), ray.getX(), ray.getY(), ray.getZ());
			}
			
			
			//refraction
			if(obj.haveRefraction()) {
				boolean notRefractionTotal = Scene.refraction(ray.getDirection(), n, n1, obj.getRefractionIndice(), refractionDirection);
				if(notRefractionTotal) {
					Ray refraction = new Ray(ray.getX(), ray.getY(), ray.getZ(), refractionDirection.getX(), refractionDirection.getY(), refractionDirection.getZ());
					refraction.march(epsilon_*delta);
					l.fusionLight(getLightColor(refraction, obj.getRefractionIndice(), true, recurtionLevel-1), obj.getRefractionLevel());
				}
			}	
			
			
			//reflection
			if(obj.haveReflect() || obj.haveReflexion()) {
				Scene.reflexion(ray.getDirection(), n, reflexionDirection);
			}
			
			if(obj.haveReflexion()) {				
				Ray reflexion = new Ray(ray.getX(), ray.getY(), ray.getZ(), reflexionDirection.getX(), reflexionDirection.getY(), reflexionDirection.getZ());
				reflexion.march(epsilon_*delta);
				
				l.fusionLight(getLightColor(reflexion, n1, false, recurtionLevel-1), obj.getReflexionLevel());
			}
			
			
			//reflet
			if(obj.haveReflect()) {				
				l.fusionLight(scene.getLightColor().duplicate(), reflet(reflexionDirection, light));
			}			
    	}else {
			l = scene.getSkyColor().duplicate();;
		}
    	return l;
    }
	 
    
    public BufferedImage createImage(int maxReccursion) {
		BufferedImage buffer = new BufferedImage(camera.getWidth(), camera.getHeight(), BufferedImage.TYPE_INT_RGB);

		float pas = 0.5f;
		int pixels = (int)((camera.getWidth() * camera.getHeight())*pas/100f);
		int counter = 0;
		float percent = 0;
				
		for(int x = 0; x < camera.getWidth(); x++) {
			for(int y = 0; y < camera.getHeight(); y++) {
				counter+=1;
				if (counter >= pixels) {
					counter = 0;
					percent+=pas;
					System.out.print(percent);
					System.out.println("%");
				}


				Ray ray = camera.getRay(x,  y);
				
				buffer.setRGB(x, y, getLightColor(ray, scene.getRefractionIndice(), false, maxReccursion).getColor());	
			}
		}
		return buffer; 
    }
    
    public static void endThread() {
    	threadsEnd++;
    }
    public static void addPixel() {
    	nbPixels++;
    }
    public BufferedImage createImageThreading(int maxReccursion, int nbThreads) {
		BufferedImage buffer = new BufferedImage(camera.getWidth(), camera.getHeight(), BufferedImage.TYPE_INT_RGB);

		
		ImageFillerThread[] fillers = new ImageFillerThread[nbThreads];
		threadsEnd = 0;
		nbPixels = 0;
		double pixels = (camera.getWidth() * camera.getHeight());

		
		for(int i = 0; i < nbThreads; i++) {
			fillers[i] = new ImageFillerThread(nbThreads, i, maxReccursion, buffer, scene);
		}
		for(int i = 0; i < nbThreads; i++) {
			fillers[i].start();
		}
		while(threadsEnd < nbThreads) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			System.out.println((((double)((nbPixels*100)/pixels)))+"%");
			
		}
		return buffer; 
    }
	
	public static void save_image(String file, BufferedImage buffer) {
		File image = new File(file);
        try {
 			ImageIO.write(buffer, "png", image);
 		} catch (IOException e) {
 			e.printStackTrace();
 		}
	}
}
