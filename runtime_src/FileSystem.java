import java.util.concurrent.atomic.AtomicInteger;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;


public final class FileSystem {
    private final AtomicInteger idGen = new AtomicInteger();
    private final Map<Integer, FileReader> readers = new HashMap<>();
    private final Map<Integer, FileWriter> writers = new HashMap<>();

    public int openR(String path) throws IOException {
        var id = idGen.incrementAndGet();
        var reader = new FileReader(path);
        readers.put(id, reader);
        return id;
    }

    public int openW(String path) throws IOException {
        var id = idGen.incrementAndGet();
        var writer = new FileWriter(path);
        writers.put(id, writer);
        return id;
    }

    public void write(int fileId, String s) throws IOException {
        var writer = writers.get(fileId);
        if (writer == null) {
            throw new IllegalArgumentException("no file with the given id is currently open write mode");
        }
        writer.write(s);
    }

    public int read(int fileId) throws IOException {
        var reader = readers.get(fileId);
        if (reader == null) {
            throw new IllegalArgumentException("no file with the given id is currently open write mode");
        }
        return reader.read();
    }

    public void close(int fileId) throws IOException {
        var reader = readers.remove(fileId);
        if (reader != null) {
            reader.close();
        }
        var writer = writers.remove(fileId);
        if (writer != null) {
            writer.close();
        }
    }

    public boolean createDir(String path){
        var dir = new File(path);
        var alreadyExists = dir.exists();
        if (!alreadyExists){
            dir.mkdirs();
        }
        return alreadyExists;
    }

    public boolean delete(String path){
        var file = new File(path);
        return file.delete();
    }

}

