import java.util.concurrent.atomic.AtomicInteger;

final class Rattlesnake$runtime {
    private static final AtomicInteger nextRegionId = new AtomicInteger(Integer.MIN_VALUE);

    public static int newRegion(){
        return nextRegionId.incrementAndGet();
    }

}
