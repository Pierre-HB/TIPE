package main;

public class Randomizer {
	private float coef, offset, contreoffset;//contreoffset est la valeur max entre [0, 1]
	private float inf, sup, intervalle;

	public Randomizer(float c) {
		//c est le coeficiant directeur de la courbe
		this.coef = c;
		
		if(c <= 2f && c >= -2f) {
			//surface du triangle rectangle de coté (hors hypothénuse) 1 et c
			this.offset = 1 - c/2f;//offset determiner pour avoir une integral de 0 a 1 de la courbe valant 1
			this.inf = 0;
			this.sup = 1f;
			this.intervalle = 1f;
			this.contreoffset = Math.max(coef, coef+offset);
		}
		else if(c >2f) {
			
			
			this.sup = 1;
			this.offset = -coef +coef*(float)Math.sqrt(2/coef);
			this.inf = -offset/coef;
			this.intervalle = sup - inf;
			this.contreoffset = coef+offset;
			
			
		}else {
			this.inf = 0;
			this.offset = (float)Math.sqrt(-2*coef);
			this.sup = -offset/coef;
			this.intervalle = sup - inf;
			this.contreoffset = offset;
			
		}
		
		System.out.println(coef);
		System.out.println(offset + " " + contreoffset);
		System.out.println(inf + " " + sup + " " + intervalle);
		
		
		
	}
	
	private float randomX() {
		return intervalle*(float)Math.random() + inf;
	}
	private float randomY() {
		return contreoffset*(float)Math.random();
		}
	
	public float rand() {
		float x = randomX();
		float y = randomY();
		
		if(coef*x + offset > y) {
			//le point est sous la courbe, pas de probleme
			if(x == 1) return rand();
			return x;
		}
		if(sup-(x-inf) == 1) return rand();
		return sup-(x-inf);

		
	}

}
