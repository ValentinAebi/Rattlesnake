package agent;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import static agent.Config.ASM_VERSION;

public final class MethodTransformer extends MethodVisitor {

    private static final String RUNTIME_CLASS_NAME = "Rattlesnake$runtime";

    public MethodTransformer(MethodVisitor methodVisitor) {
        super(ASM_VERSION, methodVisitor);
    }

    @Override
    public void visitInsn(int opcode) {
        if (Opcodes.IASTORE <= opcode && opcode <= Opcodes.SASTORE) {
            // TODO test where we instrument stores to array of boolean
            var arrayElemDescr = arrayElemTypeForStoreInsn(opcode).getDescriptor();
            var arrayDescr = "[" + arrayElemDescr;
            var methodDescr = "(" + arrayDescr + "I" + arrayElemDescr + ")V";
            super.visitMethodInsn(Opcodes.INVOKESTATIC, RUNTIME_CLASS_NAME, "monitoredArrayStore", methodDescr, false);
        } else {
            super.visitInsn(opcode);
        }
    }

    private Type arrayElemTypeForStoreInsn(int storeInsnOpcode){
        return switch (storeInsnOpcode){
            case Opcodes.IASTORE -> Type.INT_TYPE;
            case Opcodes.LASTORE -> Type.LONG_TYPE;
            case Opcodes.FASTORE -> Type.FLOAT_TYPE;
            case Opcodes.DASTORE -> Type.DOUBLE_TYPE;
            case Opcodes.AASTORE -> Type.getType(Object.class);
            case Opcodes.BASTORE -> Type.BYTE_TYPE;
            case Opcodes.CASTORE -> Type.CHAR_TYPE;
            case Opcodes.SASTORE -> Type.SHORT_TYPE;
            default -> throw new AssertionError("unexpected opcode: " + storeInsnOpcode);
        };
    }

}
