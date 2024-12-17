package agent;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;

import static agent.Config.ASM_VERSION;

public final class ClassTransformer extends ClassVisitor {

    public ClassTransformer(ClassVisitor classVisitor) {
        super(ASM_VERSION, classVisitor);
    }

    @Override
    public MethodVisitor visitMethod(int access, String name, String descriptor, String signature, String[] exceptions) {
        return new MethodTransformer(super.visitMethod(access, name, descriptor, signature, exceptions));
    }

}
