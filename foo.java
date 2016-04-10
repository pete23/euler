public class foo {


    public static class Triple {
        int a, b, c;
        Triple(int m, int n) {
            a = (m * m - n * n); b = 2 * m * n; c = m * m + n * n;
        }
        int length() {
            return a + b + c;
        }
    };

    public static int gcd(int a, int b) {
        while (b > 0) {
            int c = a % b;
            a = b;
            b = c;
        }
        return a;
    }

    public static void main(String[] args) {
        int[] counts = new int[1500001];
        int maxM = (int)Math.sqrt(counts.length/2);

        for (int m = 2; m < maxM; m++) {
            for (int n = 1; n < m; n++) {
                if (((m + n) % 2) == 1 && gcd(m, n) == 1) {
                    Triple triple = new Triple(m, n);
                    int primitiveLength = triple.length();
                    int length = primitiveLength;
                    // need to deal with the non-primitive case
                    while (length < counts.length) {
                        counts[length]++;
                        length += primitiveLength;
                    }
                }
            }
        }

        int oneCount = 0;
        for (int i = 0; i < counts.length; i++) {
            if (counts[i] == 1)
                oneCount++;
        }

        System.err.println(oneCount);
    }
}
