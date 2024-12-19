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

    @Override
    public void visitFieldInsn(int opcode, String owner, String name, String descriptor) {
        if (opcode == Opcodes.PUTFIELD){
            var fieldType = Type.getType(descriptor);
            if (fieldType.getSize() == 2){
                super.visitInsn(Opcodes.DUP2_X1);
                super.visitInsn(Opcodes.POP2);
                super.visitInsn(Opcodes.DUP_X2);
            } else {
                super.visitInsn(Opcodes.DUP_X1);
                super.visitInsn(Opcodes.POP);
                super.visitInsn(Opcodes.DUP_X1);
            }
            super.visitMethodInsn(Opcodes.INVOKESTATIC, RUNTIME_CLASS_NAME, "assertRegionAllowed",
                    "(Ljava/lang/Object;)V", false);
        }
        super.visitFieldInsn(opcode, owner, name, descriptor);
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
