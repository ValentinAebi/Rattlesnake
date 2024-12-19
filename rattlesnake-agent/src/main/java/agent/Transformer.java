package agent;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.instrument.ClassFileTransformer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.ProtectionDomain;

public final class Transformer implements ClassFileTransformer {

    @Override
    public byte[] transform(
            ClassLoader loader,
            String className,
            Class<?> classBeingRedefined,
            ProtectionDomain protectionDomain,
            byte[] classfileBuffer
    ) {
        var isProgramClass = !className.contains("/") && !className.contains("$") && !className.equals("FileSystem");
        if (isProgramClass) {
            try {
                System.err.println("[DEBUG] Instrumenting " + className);   // TODO remove (debug)
                var reader = new ClassReader(classfileBuffer);
                var writer = new ClassWriter(reader, ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
                var transformer = new ClassTransformer(writer);
                reader.accept(transformer, ClassReader.EXPAND_FRAMES);
                return writer.toByteArray();
            } catch (Throwable t) {
                System.err.println("Instrumentation failed");
                var logFileName = "instrumentation-failed-" + className + "-" + System.nanoTime() + ".log";
                var logFilePath = Path.of(".").resolve(logFileName);
                try (var writer = new FileWriter(logFilePath.toFile())){
                    t.printStackTrace(new PrintWriter(writer));
                    System.err.println("Stacktrace of the error can be found in " + logFileName);
                } catch (IOException e) {
                    System.err.println("Log file could not be written. Stacktrace:");
                    t.printStackTrace(System.err);
                    throw new RuntimeException(e);
                }
                System.exit(-1);
                return null;
            }
        } else {
            return null;
        }
    }

}
