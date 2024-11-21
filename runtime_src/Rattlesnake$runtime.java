import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

final class Rattlesnake$runtime {

    // NOTE: This implementation assumes single-threaded execution.
    // A thread-safe runtime should include per-thread capability stacks.

    // TODO what to do when too many regions are created? Can we reuse region ids that are not used anymore?

    public static final int FILE_SYSTEM_ID = Integer.MIN_VALUE;


    //////////////// Region allocation ////////////////

    // intentionally initialized with a weird number to ease debugging (i.e. number close to this one is probably a region)
    private static int nextRegionId = -700_000;

    public static int newRegion(){
        return nextRegionId++;
    }


    ///////////////// Mutable objects /////////////////

    private static final WeakHashMap<Object, Integer> regionOf = new WeakHashMap<>();

    public static void saveNewObjectInRegion(Object o, int region){
        regionOf.put(o, region);
    }


    ////////// Dynamic authority enforcement //////////

    private static final Deque<Set<Integer>> allowedCapabilities = new LinkedList<>();
    private static Set<Integer> nextFrame = new HashSet<Integer>();

    public static void addAllowedResource(int resource){
        assertAllowed(resource);
        nextFrame.add(resource);
    }

    public static void pushFrame(){
        allowedCapabilities.addLast(nextFrame);
        nextFrame = new HashSet<>();
    }

    public static void popFrame(){
        allowedCapabilities.remove();
    }

    public static void assertAllowed(int resource){
        // TODO possibly optimize by caching the information of whether the stack is empty or not
        if (!allowedCapabilities.isEmpty() && !allowedCapabilities.getLast().contains(resource)){
            throw new RuntimeException("dynamic resource access policy violated");
        }
    }

}
