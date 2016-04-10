package euler;

import java.util.Random;

/**
 * World's Worst Attempt
 * @author pete23
 */
public class Euler205BruteForce {
	public static void main(String[] args) {
		final long range = 12230590464L;
		Random random = new Random();	
		
		long accum = 0;
		for (long i = 0; i < 10000000000L; i++) {
			long test = (long)(random.nextDouble()*range);
			if (nineD4(test) > sixD6(test)) {
				accum++;
			}
		}
		System.err.println(accum);
	}
	
	static int nineD4(long x) {
		int accum = 0;
		for (int i = 0; i < 9; i++) {
			accum += x & 3;
			x = x >> 2;
		}
		return accum;
	}
	
	static int sixD6(long x) {
		x = x >> 18; // magic number because 9d4 = 18 bits
		int accum = 0;
		// what if we declare 7,8 either 1,2 3,4 5,6 depending on which iteration we're on?
		// i suspect even distribution but slightly skewed results
		// not worth it to avoid the div/mod
		for (int i = 0; i < 6; i++) {
			accum += x % 6;
			x /= 6;
		}
		return accum;
	}
}
