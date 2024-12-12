
public final class Rattlesnake$IllegalCapabilityUseError extends Error {

    public Rattlesnake$IllegalCapabilityUseError(String capabilityName){
        super("illegal use of " + capabilityName + " capability in a dynamically restricted environment");
    }

}
