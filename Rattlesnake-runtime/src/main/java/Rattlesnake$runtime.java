import it.unimi.dsi.fastutil.ints.IntOpenHashSet;
import it.unimi.dsi.fastutil.ints.IntSet;

import java.util.WeakHashMap;

public final class Rattlesnake$runtime {

    private Rattlesnake$runtime() {
    }

    private static final WeakHashMap<Object, Integer> objToRegion = new WeakHashMap<>();

    public static void saveObjectInRegion(Object obj, int region){
        objToRegion.put(obj, region);
    }

    // NOTE: This implementation assumes single-threaded execution.
    // A thread-safe runtime should include per-thread capability stacks.

    private static DynamicEnvironment currentEnvir = null;
    private static DynamicEnvironment inPrepEnvir = null;

    private static final class DynamicEnvironment {
        private boolean fileSystem = false;
        private final IntSet regions = new IntOpenHashSet();
        private DynamicEnvironment next = null;
    }

    public static void startPreparingEnvir() {
        inPrepEnvir = new DynamicEnvironment();
    }

    public static void allowFilesystem() {
        inPrepEnvir.fileSystem = true;
    }

    public static void allowRegion(int region){
        inPrepEnvir.regions.add(region);
    }

    public static void pushEnvir() {
        inPrepEnvir.next = currentEnvir;
        currentEnvir = inPrepEnvir;
        inPrepEnvir = null;
    }

    public static void popEnvir() {
        currentEnvir = currentEnvir.next;
    }

    public static void assertFileSystemAllowed() {
        if (currentEnvir != null && !currentEnvir.fileSystem) {
            throw new Rattlesnake$IllegalCapabilityUseError(
                    "illegal use of the file system in a dynamically restricted environment");
        }
    }

    private static void assertRegionAllowed(int region){
        if (currentEnvir != null && !currentEnvir.regions.contains(region)){
            throw new Rattlesnake$IllegalCapabilityUseError(
                    "illegal modification of a region region in a dynamically restricted environment");
        }
    }

    public static void assertRegionAllowed(Object obj){
        var region = objToRegion.get(obj);
        if (region != null){
            assertRegionAllowed(region);
        }
    }

    public static void monitoredArrayStore(int[] array, int idx, int value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(long[] array, int idx, long value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(float[] array, int idx, float value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(double[] array, int idx, double value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(Object[] array, int idx, Object value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(byte[] array, int idx, byte value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(char[] array, int idx, char value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    public static void monitoredArrayStore(short[] array, int idx, short value){
        assertRegionAllowed(array);
        array[idx] = value;
    }

    //// REGIONS ////

    private static int lastRegion = 0;

    public static int newRegion() {
        return ++lastRegion;
    }

}
