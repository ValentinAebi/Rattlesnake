
public final class IllegalCapabilityUse extends RuntimeException {

    public IllegalCapabilityUse(String capabilityName){
        super("illegal use of " + capabilityName + " capability in a dynamically restricted environment");
    }

}
