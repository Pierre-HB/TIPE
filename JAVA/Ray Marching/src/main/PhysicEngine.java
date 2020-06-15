package main;

import main.object3D.SphereCloud;

public class PhysicEngine {
	private SphereCloud[] clouds;
	private float dt = 0.01f;
	private int subDiv;
	private static Vect[][][][] spheres;
	private static Vect[][][][] spheresTemp;//pour calculer les position et vitesse au temps t+dt
	float p1x, p1y, p1z, p2x, p2y, p2z;
	float nx, ny, nz;
	final static public float trois_deuxpi = (float) (3/(2*Math.PI));
	private static float h = 0.3f;//distance du kernel
	private int pas = 1;//le rayon des subdivisions a tester
	private static float un_h = 1/h;
	private static float h3 = h*h*h;
	static private float un_h3 = 1/h3;
	private float m = 1f;
	public static final float g = 9.81f;
	public static float epsilon10 = Main.epsilon/10;
	
	private float density0 = 1000f;
	private float pressur0 = 100000f;
	private float k = 10f;//coeficiant d'impact de variation de densité sur la pression
	private float friction = 0.99f;
	private float wallFriction = 0.7f;
	
	private float[][] airSpheres;
	private boolean[][][][] airSphereBool;
	private boolean[][][] isComputeOutAirSphere;
	private float airMass;
	private float airDensity = 1f;
	private float nbAirSphereDiv;
	private SphereCloud potentielComputer;
	
	private boolean robinet = true;
	private int nbSPhereMore = 10;
	

	public PhysicEngine(SphereCloud[] clouds, float h, int nbSphere, float masseFluid, int nbAirSphereDiv, float massAirMolecule, SphereCloud potentielComputer, Vector3D p1, Vector3D p2) {
		p1x = Math.min(p1.getX(), p2.getX());
		p1y = Math.min(p1.getY(), p2.getY());
		p1z = Math.min(p1.getZ(), p2.getZ());
		p2x = Math.max(p1.getX(), p2.getX());
		p2y = Math.max(p1.getY(), p2.getY());
		p2z = Math.max(p1.getZ(), p2.getZ());
		
		nx = p2x - p1x;
		ny = p2y - p1y;
		nz = p2z - p1z;
		
		this.h= h;
		un_h = 1/h;
		h3 = h*h*h;
		un_h3 = 1/h3;
		this.airMass = airDensity*h3/nbAirSphereDiv;
		
		this.clouds = clouds;
		this.subDiv = (int)((Math.max(Math.max(nx, ny), nz))/(2*h));
		this.subDiv = 100;
		this.pas = Math.max(1, (int)(2*h*subDiv/((Math.max(Math.max(nx, ny), nz)))));
		this.m = masseFluid/nbSphere;
		this.nbAirSphereDiv = nbAirSphereDiv;
		this.potentielComputer = potentielComputer;
		System.out.println("subDiv physic engine : " + subDiv + " pas physic engine : " + pas);
		
		spheres = new Vect[subDiv][subDiv][subDiv][6];
		spheresTemp = new Vect[subDiv][subDiv][subDiv][6];
		isComputeOutAirSphere = new boolean[subDiv][subDiv][subDiv];
		airSpheres = new float[3][nbAirSphereDiv];
		airSphereBool = new boolean[subDiv][subDiv][subDiv][nbAirSphereDiv];
		

		//Les divisions doivent avoir une norme superieur ou egal a h le rayon d'action (la moitier du rayon d'influance)
		for(int x = 0; x < subDiv; x++) {
			for(int y = 0; y < subDiv; y++) {
				for(int z = 0; z < subDiv; z++) {
					spheres[x][y][z][0] = new Vect(5, 5);
					spheres[x][y][z][1] = new Vect(5, 5);
					spheres[x][y][z][2] = new Vect(5, 5);
					
					//pour traiter la vitesse des spheres
					spheres[x][y][z][3] = new Vect(5, 5);
					spheres[x][y][z][4] = new Vect(5, 5);
					spheres[x][y][z][5] = new Vect(5, 5);
					spheresTemp[x][y][z][0] = new Vect(5, 5);
					spheresTemp[x][y][z][1] = new Vect(5, 5);
					spheresTemp[x][y][z][2] = new Vect(5, 5);
					spheresTemp[x][y][z][3] = new Vect(5, 5);
					spheresTemp[x][y][z][4] = new Vect(5, 5);
					spheresTemp[x][y][z][5] = new Vect(5, 5);
					
					isComputeOutAirSphere[x][y][z] = false;
					for(int i = 0; i < nbAirSphereDiv; i++) {
						airSphereBool[x][y][z][i] = false;						
					}
					
				}
			}
		}
		for(int i = 0; i < nbAirSphereDiv; i++) {
			airSpheres[0][i] = (float)Math.random();
			airSpheres[1][i] = (float)Math.random();
			airSpheres[2][i] = (float)Math.random();			
		}
			if(!robinet) {
			initSpheres(nbSphere);
		}	
	}
	
	

	private static float f(float q) {
		float t;
		if(q < 1f) {
			t = q*q;
			return (0.66666f-t+0.5f*t*q);
		}
		else if(q < 2) {
			t = 2-q;
			return 0.16666f*t*t*t;
		}
		return 0f;
	}
	
	private static float kernelGaussian(float d) {
		return un_h3*f(d*un_h);
	}
	
	private void computeAirSphere(int x_, int y_, int z_) {
		float divX = p1x+nx/subDiv;
		float divY = p1y+ny/subDiv;
		float divZ = p1z+nz/subDiv;
		for(int i = 0; i < nbAirSphereDiv; i++) {
			//utilise le potentiel a support finit
			//verifie juste si la sphere est dans une molecule d'eau ou pas
			airSphereBool[x_][y_][z_][i] = (potentielComputer.distance(airSpheres[0][i] + divX, airSpheres[1][i] + divY, airSpheres[2][i] + divZ) > 0);
			
		}
		isComputeOutAirSphere[x_][y_][z_] = true;
	}
	
	
	private float density(float x, float y, float z) {
		if(x < p1x || x > p2x || y < p1y || y > p2y || z < p1z || z > p2z) {
			return density0;
		}
		float d = 0f;
		float temp;
		int x_ = (int)(subDiv*(x-p1x)/nx);
		int y_ = (int)(subDiv*(y-p1y)/ny);
		int z_ = (int)(subDiv*(z-p1z)/nz);
		float divX_x, divY_y, divZ_z;
		for(int i = Math.max(0, x_ - pas); i < Math.min(subDiv, x_ + 1 + pas); i++) {
			for(int j = Math.max(0, y_ - pas); j < Math.min(subDiv, y_ + 1 + pas); j++) {
				for(int k = Math.max(0, z_ - pas); k < Math.min(subDiv, z_ + 1 + pas); k++) {
					
					
					for(int l = 0; l < spheres[i][j][k][0].getNbValues(); l++) {
						temp = Scene.norme(x - spheres[i][j][k][0].get(l), y - spheres[i][j][k][1].get(l), z - spheres[i][j][k][2].get(l));
						if(temp < h*2) {
							d+=m*kernelGaussian(temp);
						}
					}
					
					//molecules d'air
					//position de la division relativement a x, y et z
					divX_x = p1x+nx/subDiv - x;
					divY_y = p1y+ny/subDiv - y;
					divZ_z = p1z+nz/subDiv - z;
					if(!isComputeOutAirSphere[i][j][k]) {
						computeAirSphere(i, j, k);
					}
					for(int l = 0; l < nbAirSphereDiv; l++) {
						if(airSphereBool[i][j][k][l]) {
							temp = Scene.norme(divX_x + airSpheres[0][l], divY_y + airSpheres[1][l], divZ_z + airSpheres[2][l]);
							if(temp < h*2) {
								d+=airMass*kernelGaussian(temp);
							}
						}
					}
				}
			}
		}
		return d;
	}
	
	private float pressur(float x, float y, float z) {
		
		
		
		float d = density(x, y, z)/density0;
				
		return pressur0*d*d*d*d*d*d*d;

	}
	
	private float[] pressurGradian(float x, float y, float z) {
		float p = pressur(x, y, z);
		float[] gradiant = new float[3];
		float c = 1f;
		gradiant[0] = ((pressur(x+epsilon10*c, y, z) - p) - (pressur(x-epsilon10*c, y, z) - p))/(2*epsilon10*c);
		gradiant[1] = ((pressur(x, y+epsilon10*c, z) - p) - (pressur(x, y-epsilon10*c, z) - p))/(2*epsilon10*c);
		gradiant[2] = ((pressur(x, y, z+epsilon10*c) - p) - (pressur(x, y, z-epsilon10*c) - p))/(2*epsilon10*c);
		return gradiant;
	}
	
	public void update() {
		if(robinet) {
			float r = 0.01f;
			for(int i = 0; i < nbSPhereMore; i++) {
				float x = r*4*(float)Math.random()-r*2;
				float y = r*(float)Math.random()-1f;
				float z = r*4*(float)Math.random()-r*2;
				
				addSpheres(x, y, z);
				potentielComputer.addSphere(x, y, z);
			}
		}
		
		
		int x_, y_, z_;
		float x, y, z, vx, vy, vz;
		float[] p;
		float d;
		for(int i = 0; i < subDiv; i++) {
			System.out.println(i + "%");
			for(int j = 0; j < subDiv; j++) {
				for(int k = 0; k < subDiv; k++) {
					for(int l = 0; l < spheres[i][j][k][0].getNbValues(); l++) {
						x = spheres[i][j][k][0].get(l);
						y = spheres[i][j][k][1].get(l);
						z = spheres[i][j][k][2].get(l);
						vx = spheres[i][j][k][3].get(l);
						vy = spheres[i][j][k][4].get(l);
						vz = spheres[i][j][k][5].get(l);
						
						p = pressurGradian(x, y, z);
						
						vx += -dt*p[0]*h3/m;
						vy += -dt*(p[1]*h3/m - g/100f);
						vz += -dt*p[2]*h3/m;
						
						
						vx*=friction;
						vy*=friction;
						vz*=friction;
						x+= vx*dt;
						y+= vy*dt;
						z+= vz*dt;
						
						
						if(x < p1x) {
							x = 2*p1x-x;
							vx *= -wallFriction;
						}
						if(x >= p2x) {
							x = 2*p2x-x;
							vx *= -wallFriction;
						}
						
						if(y< p1y) {
							y = 2*p1y-y;
							vy *= -wallFriction;
						}
						if(y >= p2y) {
							y = 2*p2y-y;
							vy *= -wallFriction;
						}
						
						if(z < p1z) {
							z = 2*p1z-z;
							vz *= -wallFriction;
						}
						if(z >= p2z) {
							z = 2*p2z-z;
							vz *= -wallFriction;
						}
					
						
						x_ = Math.max(0, Math.min((int)(subDiv*(x-p1x)/nx), subDiv-1));
						y_ = Math.max(0, Math.min((int)(subDiv*(y-p1y)/ny), subDiv-1));
						z_ = Math.max(0, Math.min((int)(subDiv*(z-p1z)/nz), subDiv-1));
						spheresTemp[x_][y_][z_][0].add(x);
						spheresTemp[x_][y_][z_][1].add(y);
						spheresTemp[x_][y_][z_][2].add(z);
						spheresTemp[x_][y_][z_][3].add(vx);
						spheresTemp[x_][y_][z_][4].add(vy);
						spheresTemp[x_][y_][z_][5].add(vz);	
					}
				}
			}
		}
		switchTemp();
	}
	private void switchTemp() {
		for(int x = 0; x < subDiv; x++) {
			for(int y = 0; y < subDiv; y++) {
				for(int z = 0; z < subDiv; z++) {
					spheres[x][y][z][0] = spheresTemp[x][y][z][0];
					spheres[x][y][z][1] = spheresTemp[x][y][z][1];
					spheres[x][y][z][2] = spheresTemp[x][y][z][2];
					spheres[x][y][z][3] = spheresTemp[x][y][z][3];
					spheres[x][y][z][4] = spheresTemp[x][y][z][4];
					spheres[x][y][z][5] = spheresTemp[x][y][z][5];
					spheresTemp[x][y][z][0] = new Vect(5, 5);
					spheresTemp[x][y][z][1] = new Vect(5, 5);
					spheresTemp[x][y][z][2] = new Vect(5, 5);
					spheresTemp[x][y][z][3] = new Vect(5, 5);
					spheresTemp[x][y][z][4] = new Vect(5, 5);
					spheresTemp[x][y][z][5] = new Vect(5, 5);
					isComputeOutAirSphere[x][y][z] = false;
				}
			}
		}
		
		int maxSpheres = 0;
		for(int x = 0; x < subDiv; x++) {
			for(int y = 0; y < subDiv; y++) {
				for(int z = 0; z < subDiv; z++) {
					if(maxSpheres < spheres[x][y][z][0].getNbValues()) {
						maxSpheres = spheres[x][y][z][0].getNbValues();
					}
				}
			}
		}
		System.out.println("sphere max par division : " + maxSpheres);
	}
	private void addSpheres(float x, float y, float z) {
		int x_ = (int)(subDiv*(x-p1x)/nx);
		int y_ = (int)(subDiv*(y-p1y)/ny);
		int z_ = (int)(subDiv*(z-p1z)/nz);
		if(x_ >= 0 && x_ < subDiv && y_ >= 0 && y_ < subDiv && z_ >= 0 && z_ < subDiv) {
			spheres[x_][y_][z_][0].add(x);
			spheres[x_][y_][z_][1].add(y);
			spheres[x_][y_][z_][2].add(z);
			spheres[x_][y_][z_][3].add(0f);
			spheres[x_][y_][z_][4].add(0f);
			spheres[x_][y_][z_][5].add(0f);
		}
	}
	
	public void initSpheres(int nbSphere) {

		boolean randomSphere = true;
		boolean airSphere = false;
		boolean perlin = false;
		boolean goutlette = false;	
		boolean smallcube = false;
		boolean flat = false;
		
		
		int nb = clouds.length;
		for(int c = 0; c < nb; c++) {
			clouds[c].resetData();
		}
		
		
		//generation des spheres de manière aléatoire
		if(randomSphere) {
			if(airSphere) {
				for(int i = 0; i < nbSphere; i++) {
					float x = 0.999f*(float)Math.random();
					float y = 0.65f*(float)Math.random();
					float z = 0.999f*(float)Math.random();
					
					if(y+z-x < 0.7 && ((z-0.5)*(z-0.5) + (y-0.6)*(y-0.6) + (x-0.5)*(x-0.5) > 0.3)) {
						addSpheres(p1x + x*nx, p1y + y*ny, p1z + z*nz);
					}
				}
			}
			else if(perlin) {
				PerlinNoise noise = new PerlinNoise(0.200987f);
				Randomizer rd = new Randomizer(-5f);
				
				int nb_ = 30;
				int[] test = new int[nb_];
				
				for(int i = 0; i < nb_; i++) {
					test[i] = 0;
				}
				
				for(int i = 0; i < 1000000; i++) {
					test[(int)(nb_*rd.rand())]++;
				}
				
				for(int i = 0; i < nb_; i++) {
					System.out.println(test[i]);
				}

				
				
				
				for(int i = 0; i < nbSphere; i++) {
					float x = 0.999f*(float)Math.random();
					float z = 0.999f*(float)Math.random();
					float y = 0.15f*rd.rand();
					
					if(noise.getNoise(x, z)/8+0.1 > y) {
					
						addSpheres(p1x + x*nx, p1y + y*ny, p1z + z*nz);
					}
				}
			}
			else if(smallcube) {
				for(int i = 0; i < nbSphere; i++) {
					float x = 0.5f*(float)Math.random();
					float z = 0.5f*(float)Math.random();
					float y = 0.5f*(float)Math.random();
					
					
					
						addSpheres(p1x + x*nx, p1y + y*ny, p1z + z*nz);
					}
			}
			else if(flat) {
				Randomizer rd = new Randomizer(-5f);
				for(int i = 0; i < nbSphere; i++) {
					
					float x = 0.999f*(float)Math.random();
					float z = 0.999f*(float)Math.random();
					float y = 0.15f*rd.rand();
					
					
					addSpheres(p1x + x*nx, p1y + y*ny, p1z + z*nz);
				}
			}
			else {
				for(int i = 0; i < nbSphere; i++) {
					float x = 0.999f*(float)Math.random();
					float y = 0.15f*(float)Math.random();
					float z = 0.999f*(float)Math.random();
					
					addSpheres(p1x + x*nx, p1y + y*ny, p1z + z*nz);
				}
			}
			
		}
		
		
		
		if(goutlette) {
			for (int i = 0; i < 10; i++) {
				float x = 0.1f*(float)Math.random()+0.5f;
				float y = 0.1f*(float)Math.random() + 0.9f;
				float z = 0.1f*(float)Math.random()+0.5f;
				
				addSpheres(p1x + x*nx, p1y + y*ny, p1z + z*nz);
			}
		}
		refreshClouds();
		
		int maxSpheres = 0;
		for(int x = 0; x < subDiv; x++) {
			for(int y = 0; y < subDiv; y++) {
				for(int z = 0; z < subDiv; z++) {
					if(maxSpheres < spheres[x][y][z][0].getNbValues()) {
						maxSpheres = spheres[x][y][z][0].getNbValues();
					}
				}
			}
		}
		System.out.println("sphere max par division : " + maxSpheres);
		
	}
	
	public void refreshClouds() {
		for(int l = 0; l < clouds.length; l++) {
			clouds[l].resetData();
		}
		for(int x = 0; x < subDiv; x++) {
			for(int y = 0; y < subDiv; y++) {
				for(int z = 0; z < subDiv; z++) {
					for(int s = 0; s < spheres[x][y][z][0].getNbValues(); s++) {
						for(int l = 0; l < clouds.length; l++) {
							clouds[l].addSphere(spheres[x][y][z][0].get(s), spheres[x][y][z][1].get(s), spheres[x][y][z][2].get(s));
						}
					}					
				}
			}
		}
		for(int l = 0; l < clouds.length; l++) {
			clouds[l].averageComputing();
		}
	}

}
