package agent;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import static agent.Config.ASM_VERSION;

public final class MethodTransformer extends MethodVisitor {

    public MethodTransformer(MethodVisitor methodVisitor) {
        super(ASM_VERSION, methodVisitor);
    }

    @Override
    public void visitInsn(int opcode) {
        if (Opcodes.IASTORE <= opcode && opcode <= Opcodes.SASTORE) {
            var usesTwoSlots = opcode == Opcodes.LASTORE || opcode == Opcodes.DASTORE;
            // TODO
        }
        super.visitInsn(opcode);
    }

}
