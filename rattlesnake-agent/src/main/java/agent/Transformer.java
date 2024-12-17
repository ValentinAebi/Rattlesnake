package agent;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;

import java.lang.instrument.ClassFileTransformer;
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
        var isProgramClass = !className.contains("/");
        if (isProgramClass) {
            System.err.println("[DEBUG] Instrumenting " + className);   // TODO remove (debug)
            var reader = new ClassReader(classfileBuffer);
            var writer = new ClassWriter(reader, ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS);
            var transformer = new ClassTransformer(writer);
            reader.accept(transformer, ClassReader.EXPAND_FRAMES);
            return writer.toByteArray();
        } else {
            return classfileBuffer;
        }
    }

}
