package main.object3D;

import main.Main;
import main.Material;
import main.Materials;
import main.PerlinNoise;
import main.Scene;
import main.Timer;
import main.Vect;
import main.Vector3D;

public class SphereCloud extends Object3D{
	private Vect[][][][] spheres;
	
	private float p1x, p1y, p1z, p2x, p2y, p2z;//bordure interne cube
	private float nx, ny, nz, r;//diemension du cube et rayon des sphere/potentiel
	private float p1xr, p1yr, p1zr, p2xr, p2yr, p2zr;// bordure externe pour les sphere
	private float p1xe, p1ye, p1ze, p2xe, p2ye, p2ze;//bordure externe pour les metaball
	private float ox, oy, oz;
	private int subDiv = 100;
	private float div;//subDiv, mais en float
	private float distanceMax;
	private float distanceMaxSquare;
	private float radius_square;
	private float inverse_radius_square;

	
	
	//POTENTIEL DATA
	private float radius;
	private float threshold = 0.9f;
	
	
	
	private boolean showSphere;
	
	
	
	
	public SphereCloud(Vector3D p1, Vector3D p2, int subDivision, float rSphere, float rPotentiel, boolean showSphere,  Scene scene) {
		
		//interaction avec la lumiere
		super.opaque = false;
		super.refract = true;
		super.nRefraction = 1.33f;
		super.haveReflexion = true;
		super.reflexionLevel = 0.9f;
		super.haveReflect = true;
		super.material = new Material(0.7f, 0.5f, 0.05f, 5f, 10f, 20f);
		if(showSphere) {
			super.haveReflect = false;
			super.refract = false;
			super.haveReflexion = false;
			
		}
		

		
		p1x = Math.min(p1.getX(), p2.getX());
		p1y = Math.min(p1.getY(), p2.getY());
		p1z = Math.min(p1.getZ(), p2.getZ());
		p2x = Math.max(p1.getX(), p2.getX());
		p2y = Math.max(p1.getY(), p2.getY());
		p2z = Math.max(p1.getZ(), p2.getZ());
		
		p1xr = p1x - rSphere;
		p1yr = p1y - rSphere;
		p1zr = p1z - rSphere;
		p2xr = p2x + rSphere;
		p2yr = p2y + rSphere;
		p2zr = p2z + rSphere;
		
		p1xe = p1x - Main.epsilon*2;
		p1ye = p1y - Main.epsilon*2;
		p1ze = p1z - Main.epsilon*2;
		p2xe = p2x + Main.epsilon*2;
		p2ye = p2y + Main.epsilon*2;
		p2ze = p2z + Main.epsilon*2;
		
		nx = p2x - p1x;
		ny = p2y - p1y;
		nz = p2z - p1z;
		
		ox = (p1x + p2x)/2;
		oy = (p1y + p2y)/2;
		oz = (p1z + p2z)/2;
		
		if(showSphere) {
			subDiv = subDivision;
		}
		else {
			subDiv = (int)(Math.min(Math.min(nx, ny), nz)/rPotentiel);
		}
		div = (float)subDiv;
		
		this.showSphere = showSphere;
		//precalcul des constante importantes pour le potentiel
		radius = rPotentiel;		
		radius_square = radius*radius;
		inverse_radius_square = 1/radius_square;
		//precalcule des constante importante pour les spheres		
		this.r = rSphere;
		distanceMax = Math.min(Math.min(nx/div, ny/div), nz/div)*1.5f;
		distanceMax -= r;
		distanceMaxSquare = distanceMax*distanceMax;
		
		
		
		
		
		
		System.out.println("Nombre de divisions : " + subDiv);

		spheres = new Vect[subDiv][subDiv][subDiv][3];
		for(int i = 0; i < subDiv; i++) {
			for(int j = 0; j < subDiv; j++) {
				for(int k = 0; k < subDiv; k++) {
					spheres[i][j][k][0] = new Vect(5, 5);
					spheres[i][j][k][1] = new Vect(5, 5);
					spheres[i][j][k][2] = new Vect(5, 5);
				}
			}
		}
		
		
		
		

		
		boolean renderDivisions = false;
		if(renderDivisions) {
			float radiusCylinders = 0.02f;

			for(int i = 0; i < subDiv+1; i++) {
				float t = p1z + i*nz/div;
				scene.addObject(new Cylinder(new Vector3D(p1x-Main.epsilon, p1y, t), new Vector3D(p1x-Main.epsilon, p2y, t), radiusCylinders, Materials.red()));

			}
			for(int i = 0; i < subDiv+1; i++) {
				float t = p1y + i*ny/div;
				scene.addObject(new Cylinder(new Vector3D(p1x-Main.epsilon, t, p1z), new Vector3D(p1x-Main.epsilon, t, p2z), radiusCylinders, Materials.green()));

			}			
		}
		
		if(showSphere) {
			System.out.print("distanceMax : ");
			System.out.println(distanceMax-r);
			System.out.print("rayon : ");
			System.out.println(r);
			System.out.print("dimension division : ");
			System.out.println(distanceMax);
			System.out.print("rayon du potentiel : ");
			System.out.println(radius);
			
			if(Math.min(Math.min(nx/div, ny/div), nz/div) < r) {
				System.out.println("ATTENTION Trop de division pour un rayon trop grand");
			}
			if (r > distanceMax) {
				System.out.println("ATTENTION, le rayson choisit pour le nuage de sphere est trop grand");
			}
			
			//pour eviter des calcul de racine carré lor de la comparaison des distance
			if(distanceMax <= 0) {
				System.out.println("ATTENTION, Le nombre de subdivision est trop grand dans le nuage de sphere, DANGER!!! distance négative...");
			}
		}		

	}
	
	public void resetData() {
		for(int i = 0; i < subDiv; i++) {
			for(int j = 0; j < subDiv; j++) {
				for(int k = 0; k < subDiv; k++) {
					spheres[i][j][k][0].reset();
					spheres[i][j][k][1].reset();
					spheres[i][j][k][2].reset();
				}
			}
		}
	}
	public void averageComputing() {
		for(int i = 0; i < subDiv; i++) {
			for(int j = 0; j < subDiv; j++) {
				for(int k = 0; k < subDiv; k++) {
					spheres[i][j][k][0].compute_moyenne();
					spheres[i][j][k][1].compute_moyenne();
					spheres[i][j][k][2].compute_moyenne();
				}
			}
		}
	}
	public void addSphere(float x, float y, float z) {
		int x_ = (int)(div*(x-p1x)/nx);
		int y_ = (int)(div*(y-p1y)/ny);
		int z_ = (int)(div*(z-p1z)/nz);
		if(x_ >= 0 && x_ < subDiv && y_ >= 0 && y_ < subDiv && z_ >= 0 && z_ < subDiv) {
			spheres[x_][y_][z_][0].add(x);
			spheres[x_][y_][z_][1].add(y);
			spheres[x_][y_][z_][2].add(z);
		}
	}
	
	public float potentiel(float x, float y, float z) {
		//renvoit le potentiel non contraint a un cube
		float s = 0;
		float tx, ty, tz, temp, t, t1, t2, t3;
		
		int x_ = (int)(div*(x-p1x)/nx);
		int y_ = (int)(div*(y-p1y)/ny);
		int z_ = (int)(div*(z-p1z)/nz);
		
		int i_min = Math.max(0, x_ - 1);
		int i_max = Math.min(subDiv, x_ + 2);
		int j_min = Math.max(0, y_ - 1);
		int j_max = Math.min(subDiv, y_ + 2);
		int k_min = Math.max(0, z_ - 1);
		int k_max = Math.min(subDiv, z_ + 2);
		
		
		for(int i = i_min; i < i_max; i++) {
			for(int j = j_min; j < j_max; j++) {
				for(int k = k_min; k < k_max; k++) {
					for(int l = 0; l < spheres[i][j][k][0].getNbValues(); l++) {
						tx = spheres[i][j][k][0].get(l) - x;
						ty = spheres[i][j][k][1].get(l) - y;
						tz = spheres[i][j][k][2].get(l) - z;
						temp = tx*tx + ty*ty + tz*tz;
						if(temp < radius_square) {
							t = 1-temp*inverse_radius_square;
							t1 = t*t;
							t2 = t1*t1;
							s+=t2*t;
							
							//le meilleur semble etre pour
							//un thresold = 0.9 et s+=t*t*t*t*t (5 t)
						}
					}
				}
			}
		}
		return s - threshold;
		
		
	}
	
	public float distanceEnveloppe(float x, float y, float z) {
		float x_ = Math.max(0, Math.abs(x - ox) - nx/2);
		float y_ = Math.max(0, Math.abs(y - oy) - ny/2);
		float z_ = Math.max(0, Math.abs(z - oz) - nz/2);
		return (float)Math.sqrt(x_*x_ + y_*y_ + z_*z_);
	}
	public Vector3D getNormalPotentiel(float x, float y, float z) {
		if (x < p1x|| x > p2x|| y < p1y|| y > p2y|| z < p1z|| z > p2z) {
			float d = distanceEnveloppe(x, y, z);
			float dx = distanceEnveloppe(x+epsilon10, y, z);
			float dy = distanceEnveloppe(x, y+epsilon10, z);
			float dz = distanceEnveloppe(x, y, z+epsilon10);
			return new Vector3D(dx-d, dy-d, dz-d);
		}
		float v = potentiel(x, y, z);
		return new Vector3D(v-potentiel(x+epsilon10, y, z), v-potentiel(x, y+epsilon10, z), v-potentiel(x, y, z+epsilon10));
	}

	
	public void intersect(float[] a_, float[] b_, float[] inter) {
		//cherche le point de potentiel nul de maniere dichotomique
		//a est un point dee potentiel positif et b un point de potentiel negatif
		//intersct aura les coordné d'un point situé a moins de epsilon du point de potentiel null.
		float[] a = {a_[0], a_[1], a_[2]};
		float[] b = {b_[0], b_[1], b_[2]};
		
		if(potentiel(a_[0], a_[1], a_[2]) > 0f && potentiel(b_[0], b_[1], b_[2]) < 0f) {
			a[0] = b_[0];
			a[1] = b_[1];
			a[2] = b_[2];
			b[0] = a_[0];
			b[1] = a_[1];
			b[2] = a_[2];
		}
		
		float d = Scene.norme(a[0]-b[0], a[1]-b[1], a[2]-b[2]);
		
		int k = (int)Main.log(2*d/(Main.epsilon), 2f);
		for(int i = 0; i <= k; i++) {
			inter[0] = (a[0]+b[0])/2f;
			inter[1] = (a[1]+b[1])/2f;
			inter[2] = (a[2]+b[2])/2f;
			if(potentiel(inter[0], inter[1], inter[2]) > 0) {
				b[0] = inter[0];
				b[1] = inter[1];
				b[2] = inter[2];
			}
			else {
				a[0] = inter[0];
				a[1] = inter[1];
				a[2] = inter[2];
			}
		}
		inter[0] = a[0];
		inter[1] = a[1];
		inter[2] = a[2];
	}
	
	
	private static void projection(float[] o, float[] m, float[] d, float[] p) {
		//projette m sur la droite de direction d passant par o. la projection est placer dans p
		p[0] = m[0] - o[0];
		p[1] = m[1] - o[1];
		p[2] = m[2] - o[2];
		float s = Scene.scalar(p,  d);
		//norme du vecteur projeter
		p[0] = o[0] + s*d[0];
		p[1] = o[1] + s*d[1];
		p[2] = o[2] + s*d[2];
	}
	
	public float distanceAdvancedPotentiel(float x, float y, float z, Vector3D direction) {
		if (x < p1xe || x > p2xe || y < p1ye || y > p2ye || z < p1ze || z > p2ze) {
			return distanceEnveloppe(x, y, z) - epsilon10;
		}
		
		boolean fastComputing = true;

		float[] o = {x, y, z};
		float[] m = new float[3];
		float[] d = {direction.getX(), direction.getY(), direction.getZ()};
		float[] p = new float[3];

		float potO = potentiel(o[0], o[1], o[2]);
		//Si jamais on est trés proche de l'enveloppe interne
		//si on est deja dans les potentiel positif, il faut restraindre le liquide
		//autrement, on traverssera le liquide entierement
		if (potO > 0 && (x < p1x || x > p2x || y < p1y || y > p2y || z < p1z || z > p2z)) {
			return distanceEnveloppe(x, y, z) - epsilon10;
		}
		
		int i = (int)(div*(x-p1x)/nx);
		int j = (int)(div*(y-p1y)/ny);
		int k = (int)(div*(z-p1z)/nz);

		if(i < 0) i = 0;
		if(i >= subDiv) i = subDiv - 1;
		if(j < 0) j = 0;
		if(j >= subDiv) j = subDiv - 1;
		if(k < 0) k = 0;
		if(k >= subDiv) k = subDiv - 1;

		
		int offsetI = 1;
		int offsetJ = 1;
		int offsetK = 1;
		float dx = p1x + (i+1)*nx/div - x;
		float dy = p1y + (j+1)*ny/div - y;
		float dz = p1z + (k+1)*nz/div - z;
		
		float sx = nx/div;
		float sy = ny/div;
		float sz = nz/div;
		
		
		if(direction.getX() < 0) {
			offsetI = -1;
			dx = nx/div-dx;
		}
		if(direction.getY() < 0) {
			offsetJ = -1;
			dy = ny/div-dy;
		}
		if(direction.getZ() < 0) {
			offsetK = -1;
			dz = nz/div-dz;
		}
		
		float dirX = Math.abs(direction.getX());
		float dirY = Math.abs(direction.getY());
		float dirZ = Math.abs(direction.getZ());
		int pas = 1;
		int pasX = pas;
		int pasY = pas;
		int pasZ = pas;
		float v;
		
		while (i >= 0 && i < subDiv && j >= 0 && j < subDiv && k >= 0 && k < subDiv){			
			for(int i_ = Math.max(0, i - pasX); i_ < Math.min(subDiv, i + 1 + pasX); i_++) {
				for(int j_ = Math.max(0, j - pasY); j_ < Math.min(subDiv, j + 1 + pasY); j_++) {
					for(int k_ = Math.max(0, k - pasZ); k_ < Math.min(subDiv, k + 1 + pasZ); k_++) {
						if(!fastComputing){
							for(int l = 0; l < spheres[i_][j_][k_][0].getNbValues(); l++) {
								m[0] = spheres[i_][j_][k_][0].get(l);
								m[1] = spheres[i_][j_][k_][1].get(l);
								m[2] = spheres[i_][j_][k_][2].get(l);
								projection(o, m, d, p);
								v = potentiel(p[0], p[1], p[2]);
								if(v > 0) {
									
									//on calcul le point d'intersection
									//m prend la valeur du point de potentiel quasi null
									intersect(o, p, m);
									if (m[0] < p1x || m[0] > p2x || m[1] < p1y || m[1] > p2y || m[2] < p1z || m[2] > p2z) {
										if((m[0] < p1x && x < p1x) || (m[0] > p2x && x > p2x) || (m[1] < p1y && y < p1y) || (m[1] > p2y && y > p2y) || (m[2] < p1z && z < p1z) || (m[2] > p2z && z > p2z)) {
											return distanceEnveloppe(x, y, z);
										}
										
										else {
											return 10f;
										}
										
									}
									
									m[0]-=o[0];
									m[1]-=o[1];
									m[2]-=o[2];
									return Scene.norme(m);
								}
							}	
						}
						else {
							if(spheres[i_][j_][k_][0].getNbValues() != 0) {
								m[0] = spheres[i_][j_][k_][0].get_moyenne();
								m[1] = spheres[i_][j_][k_][1].get_moyenne();
								m[2] = spheres[i_][j_][k_][2].get_moyenne();
								projection(o, m, d, p);
								v = potentiel(p[0], p[1], p[2]);
								if(v > 0) {
									
									//on calcul le point d'intersection
									//m prend la valeur du point de potentiel quasi null
									intersect(o, p, m);
									if (m[0] < p1x || m[0] > p2x || m[1] < p1y || m[1] > p2y || m[2] < p1z || m[2] > p2z) {
										if((m[0] < p1x && x < p1x) || (m[0] > p2x && x > p2x) || (m[1] < p1y && y < p1y) || (m[1] > p2y && y > p2y) || (m[2] < p1z && z < p1z) || (m[2] > p2z && z > p2z)) {
											return distanceEnveloppe(x, y, z);
										}
										
										else {
											return 10f;
										}
										
									}
									
									m[0]-=o[0];
									m[1]-=o[1];
									m[2]-=o[2];
									return Scene.norme(m);
							}
							}
						}	
					}
				}
			}
			
			//calcul du prechain cube a visiter
			if(dy*dirX < dx*dirY) {
				if(dy*dirZ < dz*dirY) {	
					pasX = pas;
					pasY = 0;					
					pasZ = pas;
					
					j+=offsetJ;
					dy+=sy;
				}
				else {
					pasX = pas;
					pasY = pas;
					pasZ = 0;
					
					k+=offsetK;
					dz+=sz;
				}
			}
			else {
				if(dx*dirZ < dz*dirX) {						
					pasX = 0;					
					pasY = pas;
					pasZ = pas;
					
					i+=offsetI;
					dx+=sx;
				}
				else {
					pasX = pas;
					pasY = pas;
					pasZ = 0;
					
					
					k+=offsetK;
						dz+=sz;
				}
			}			
		}
	return 10f;
	}
	

	
	
//	fonction avec le potentiel a support infini
	public Vector3D getNormal(float x, float y, float z) {	
		if(showSphere) {
			float d = distanceMin(x, y, z);
			return new Vector3D(d-distanceMin(x-epsilon10, y, z), d-distanceMin(x, y-epsilon10, z), d-distanceMin(x, y, z-epsilon10));
		
		}
		return getNormalPotentiel(x, y, z);
	}
	
	@Override
	public float distance(float x, float y, float z) {
		float d = distanceMin(x, y, z);
		return d;
	}
	
	@Override
	public float distanceAdvanced(float x, float y, float z, Vector3D direction) {
		if(showSphere) {
			return distanceMin(x, y, z);
		}
		return distanceAdvancedPotentiel(x, y, z, direction);
	}
	
	
	public float distanceMin(float x, float y, float z) {
		//on verifie que le pointb est dans le cube etendu par le rayon des spheres
		if (x < p1xr || x > p2xr || y < p1yr || y > p2yr || z < p1zr || z > p2zr) {
			return distanceEnveloppe(x, y, z);
		}
		// si le point est effectivement dans le cube:
		int x_ = (int)(div*(x-p1x)/nx);
		int y_ = (int)(div*(y-p1y)/ny);
		int z_ = (int)(div*(z-p1z)/nz);
				
		float d = distanceMaxSquare;
		float temp;
		float tx, ty, tz;

		int pas = 1;
		//la compléxité du programme est donné par ces boucles for :
		for(int i = Math.max(0, x_ - pas); i < Math.min(subDiv, x_ + 1 + pas); i++) {
			for(int j = Math.max(0, y_ - pas); j < Math.min(subDiv, y_ + 1 + pas); j++) {
				for(int k = Math.max(0, z_ - pas); k < Math.min(subDiv, z_ + 1 + pas); k++) {
					for(int l = 0; l < spheres[i][j][k][0].getNbValues(); l++) {
						tx = spheres[i][j][k][0].get(l) - x;
						ty = spheres[i][j][k][1].get(l) - y;
						tz = spheres[i][j][k][2].get(l) - z;
						temp = tx*tx + ty*ty + tz*tz;
						
						if(temp < d) {
							d = temp;
						}
					}
				}
			}
		}
		return (float)Math.sqrt(d) - r;
	}
	

}
