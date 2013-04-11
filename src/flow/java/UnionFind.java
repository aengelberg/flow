package flow.java;

public class UnionFind {
    private int N;
    public int[] array;
    private int[] sizes;
    public UnionFind(int capacity) {
        N = capacity;
        array = new int[N];
        sizes = new int[N];
        for (int i = 0; i < capacity; i++) {
            array[i] = i;
            sizes[i] = 1;
        }
    }
    public int findRoot(int i) {
        int n = i;
        while (array[n] != n)
            n = array[n];
        return n;
    }
    public void connect(int i, int j) {
        int m = findRoot(i);
        int n = findRoot(j);
        if (m == n)
            return;
        int mSize = sizes[m];
        int nSize = sizes[n];
        if (mSize < nSize) {
            array[m] = n;
            sizes[n] = mSize + nSize;
        }
        else {
            array[n] = m;
            sizes[m] = mSize + nSize;
        }
    }
    public boolean isConnected(int i, int j) {
        return findRoot(i) == findRoot(j);
    }
    
}