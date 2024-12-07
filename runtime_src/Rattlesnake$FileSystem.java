import java.util.concurrent.atomic.AtomicLong;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.Map;
import java.util.HashMap;


public final class Rattlesnake$FileSystem {
    private final AtomicLong idGen = new AtomicLong();
    private final Map<Long, FileReader> readers = new HashMap<>();
    private final Map<Long, FileWriter> writers = new HashMap<>();

    public long openForRead(String path) {
        var id = idGen.incrementAndGet();
        // TODO
        return id;
    }

    public long openForWrite(String path) {
        var id = idGen.incrementAndGet();
        // TODO
        return id;
    }

    public void write(long fileId, String s) {
        // TODO
    }

    public void readLine(long fileId) {
        // TODO
    }

    public void closeFile(long fileId) {
        readers.remove(fileId);
        writers.remove(fileId);
    }

}

