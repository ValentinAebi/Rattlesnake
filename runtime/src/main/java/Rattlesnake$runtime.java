import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public final class Rattlesnake$runtime {

    private Rattlesnake$runtime() {
    }

    // NOTE: This implementation assumes single-threaded execution.
    // A thread-safe runtime should include per-thread capability stacks.

    private static DynamicEnvironment currentEnvir = null;
    private static DynamicEnvironment inPrepEnvir = null;

    private static final class DynamicEnvironment {
        private boolean fileSystem = false;
        private DynamicEnvironment next = null;
    }

    public static void startPreparingEnvir() {
        inPrepEnvir = new DynamicEnvironment();
    }

    public static void allowFilesystem() {
        inPrepEnvir.fileSystem = true;
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
            throw new Rattlesnake$IllegalCapabilityUseError("file system");
        }
    }

    private static int lastRegion = 0;

    public static int newRegion() {
        return ++lastRegion;
    }

}
