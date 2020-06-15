package main;

import java.awt.image.BufferedImage;

import main.object3D.Cube;
import main.object3D.Cylinder;
import main.object3D.Metabale;
import main.object3D.Plan;
import main.object3D.Sphere;
import main.object3D.SphereCloud;

public class Main implements Materials{
	static public final float epsilon = 0.001f;
	
	public static float log(float x, float base) {
		return ((float)Math.log(x))/((float)Math.log(base));
	}

	public Main() {
	}

	public static void main(String[] args) {

		
		long start = System.nanoTime();
		
		Vector3D light = new Vector3D(3f, -2f, 1f);
		
		Scene scene = new Scene(light);
		Scene scene2 = new Scene(light);
		scene.addObject(new Plan(-2f));
		scene2.addObject(new Plan(-2f));
		
		
		
		
		Vector3D p1 = new Vector3D(-0.02f, -1f, -0.02f);
		Vector3D p2 = new Vector3D(0.02f, 1f, 0.02f);

		
		
		// subDiv, nbSphere, radius
		boolean shere = true;
		SphereCloud cloud = new SphereCloud(p1, p2, 2, 0.01f, 0.03f, true, scene);
		SphereCloud cloud2 = new SphereCloud(p1, p2, 78, 0.01f, 0.03f, false, scene2);
		scene.addObject(cloud);
		scene2.addObject(cloud2);
//		
		
		SphereCloud[] clouds = {cloud, cloud2};
		
//		PhysicEngine engine = new PhysicEngine(clouds, 0.01f, 10000000, 1f, p1, p2);
		//la masse d'une molecule d'eau est de 0.0005
		PhysicEngine engine = new PhysicEngine(clouds, 0.05f, 1000000, 600f, 20, 0.00000005f, cloud, p1, p2);

		
		
		
		
		boolean diag = false;
		boolean rotate = true;
		boolean above = false;
		boolean rotateUp = false;
		float rot = (float)-Math.PI*0.15f;
		float alpha = (float)-Math.PI/12f;
		float x = (float)Math.sqrt(Math.abs(1-Math.sin(alpha)*Math.sin(alpha))/2f);

		Vector3D direction = new Vector3D((float)Math.cos(alpha)/2, (float)Math.sin(alpha), -(float)Math.cos(alpha)/2);
		Vector3D up = new Vector3D((float)Math.sin(alpha)/2, (float)Math.cos(alpha), -(float)Math.sin(alpha)/2);
		
		
		Vector3D o = new Vector3D(-3f, 1.3f, 2.5f);
		if(!diag) {
			

			direction = new Vector3D(1f, 0f, 0f);
			up = new Vector3D(0f, 1f, 0f);
			
			
			o = new Vector3D(-4f, 0f, 0f);
			
			
		}
		if(rotate) {
			float r = 4.5f;
			o = new Vector3D(-r*Math.cos(rot), 0.25f, -r*Math.sin(rot));
			direction = new Vector3D(Math.cos(rot), 0f, Math.sin(rot));
			direction.print();
			up = new Vector3D(0f, 1f, 0f);
		}
		if (above) {
			o = new Vector3D(0, 10f, 0);
			direction = new Vector3D(0f, -1f, 0f);
			up = new Vector3D(1f, 0f, 0f);
		}
		if(rotateUp) {
			float r = 5f;
			o = new Vector3D(-r*Math.cos(rot), r*Math.sin(rot), 0.5f);
			direction = new Vector3D(Math.cos(rot), -Math.sin(rot), 0f);
			direction.print();
			up = new Vector3D(Math.sin(rot), Math.cos(rot), 0f);
		}
		
		
		
		
		
		float dezoom =  0.5f;
		Camera camera = new Camera(o, direction, up, (float)Math.PI/3f, (int)(1920f*dezoom), (int)(1080f*dezoom));

		ImageMaker image = new ImageMaker(scene, camera);
		ImageMaker image2 = new ImageMaker(scene2, camera);
		
		long secondStart = System.nanoTime();
		
	
		
		
		int nbThreads = 2;
		BufferedImage buffer = image.createImageThreading(2, nbThreads);
		BufferedImage buffer2 = image2.createImageThreading(2, nbThreads);
		
		boolean video = true;
		int frames = 500;
		String file = "C:\\Users\\pierre\\Desktop\\videos\\video";
		String imgFile = "C:\\Users\\pierre\\Desktop\\videos";
		float[][] blur = {{0.11f, 0.11f, 0.11f},
				{0.11f, 0.11f, 0.11f},
				{0.1f, 0.11f, 0.11f}};
		
		if(video) {
			for(int i = 0; i < frames-1; i++) {
				image.save_image(file+"\\wave_sphere"+i+".png", buffer);
				image2.save_image(file+"\\wave_"+i+".png", buffer2);

				System.out.println("start update : " + i);
				engine.update();
				System.out.println("start refresh : " + i);
				engine.refreshClouds();
				buffer = image.createImageThreading(2, nbThreads);
				buffer2 = image2.createImageThreading(2, nbThreads);
			}
			image.save_image(file+"\\wave_"+(frames-1)+".png", buffer);
			image2.save_image(file+"\\wave_"+(frames-1)+".png", buffer2);

		}
		else {
			
			ImageMaker.save_image(imgFile+"\\image.png", buffer);
			
			for(int i = 0; i < 1; i++) {
				buffer = ImageMaker.convolution(buffer, blurCreator(10));
				ImageMaker.save_image(imgFile+"\\image blu " + i + ".png", buffer);
			}
		
		
		}
		
		
		
		
		System.out.print("fin création de l'image en : ");
		System.out.print(( System.nanoTime() - secondStart)/1000000);
		System.out.print("ms\n");
		
		secondStart = System.nanoTime();
		
		
		
		
		float[][] sharpen = {{0f, -1f, 0f},
							{-1f, 5f, -1f},
							{0f, -1f, 0f}};
		
		float[][] noiseReductor = {{0.7f, 1f, 0.7f},
								  {1f, 0f, 1f},
								  {0.7f, 1f, 0.7f}};
		float[][] noiseReductor2 = {{0.2f, 0.5f, 0.7f, 0.5f, 0.2f},
								   {0.5f, 0.7f, 1f, 0.7f, 0.5f},
								   {0.7f, 1f, 0f, 1f, 0.7f},
								   {0.5f, 0.7f, 1f, 0.7f, 0.5f},
								   {0.2f, 0.5f, 0.7f, 0.5f, 0.2f}};
		float[][] noiseReductor3 = {{1f, 1f, 1f, 1f, 1f},
									{1f, 1f, 1f, 1f, 1f},
									{1f, 1f, 0f, 1f, 1f},
									{1f, 1f, 1f, 1f, 1f},
									{1f, 1f, 1f, 1f, 1f}};
		


		buffer = image.artifact_filter(buffer, noiseReductor3, 100f);
		image.save_image(imgFile+"\\image proccessing filter.png", buffer);

		System.out.print("fin de la convolution en : ");
		System.out.print(( System.nanoTime() - secondStart)/1000000);
		System.out.print("ms\n");
		
		
		System.out.println("fin du programe en : ");
		Timer.displayNumber(( System.nanoTime() - start));

	}
	
	private static float[][] blurCreator(int n){
		float[][] kernel = new float[n][n];
		float b = 1f/(float)(n*n);
		
		
		for (int i = 0; i < n; i++) {
			for(int j = 0; j < n; j++) {
				kernel[i][j] = b;
			}
		}
		
		return kernel;
	}
	
	public static void initCloud(int nbSphere, Vector3D p1, Vector3D p2, SphereCloud[] clouds) {
		boolean randomSphere = true;
		boolean airSphere = true;
		boolean perlin = false;
		boolean goutlette = false;
		
		
		
		int nb = clouds.length;
		for(int c = 0; c < nb; c++) {
			clouds[c].resetData();
		}
		
		float p1x = Math.min(p1.getX(), p2.getX());
		float p1y = Math.min(p1.getY(), p2.getY());
		float p1z = Math.min(p1.getZ(), p2.getZ());
		float p2x = Math.max(p1.getX(), p2.getX());
		float p2y = Math.max(p1.getY(), p2.getY());
		float p2z = Math.max(p1.getZ(), p2.getZ());
		
		float nx = p2x - p1x;
		float ny = p2y - p1y;
		float nz = p2z - p1z;
		//generation des spheres de manière aléatoire
		if(randomSphere) {
			if(airSphere) {
				for(int i = 0; i < nbSphere; i++) {
					float x = 0.999f*(float)Math.random();
					float y = 0.65f*(float)Math.random();
					float z = 0.999f*(float)Math.random();
					
					if(y+z-x < 0.7 && ((z-0.5)*(z-0.5) + (y-0.6)*(y-0.6) + (x-0.5)*(x-0.5) > 0.3)) {
						for(int c = 0; c < nb; c++) {
							clouds[c].addSphere(p1x + x*nx, p1y + y*ny, p1z + z*nz);
						}
					}
				}
			}
			else if(perlin) {
				PerlinNoise noise = new PerlinNoise(1.00987f);
				for(int i = 0; i < nbSphere; i++) {
					float x = 0.999f*(float)Math.random();
					float y = 0.999f*(float)Math.random();
					float z = 0.999f*(float)Math.random();
					
					if(noise.getNoise(x, z)/8+0.4 > y) {
					
						for(int c = 0; c < nb; c++) {
							clouds[c].addSphere(p1x + x*nx, p1y + y*ny, p1z + z*nz);
						}
					}
				}
			}
			else {
				for(int i = 0; i < nbSphere; i++) {
					float x = 0.999f*(float)Math.random();
					float y = 0.65f*(float)Math.random();
					float z = 0.999f*(float)Math.random();
					
					for(int c = 0; c < nb; c++) {
						clouds[c].addSphere(p1x + x*nx, p1y + y*ny, p1z + z*nz);
					}
				}
			}
		}
		
		
		
		if(goutlette) {
			for (int i = 0; i < 10; i++) {
				float x = 0.1f*(float)Math.random()+0.5f;
				float y = 0.1f*(float)Math.random() + 0.9f;
				float z = 0.1f*(float)Math.random()+0.5f;
				
				for(int c = 0; c < nb; c++) {
					clouds[c].addSphere(p1x + x*nx, p1y + y*ny, p1z + z*nz);
				}
			}
		}
	}

}
