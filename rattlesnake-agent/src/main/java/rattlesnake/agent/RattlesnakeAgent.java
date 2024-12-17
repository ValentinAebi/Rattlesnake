package rattlesnake.agent;

import java.lang.instrument.Instrumentation;

// command: java -javaagent:..\Rattlesnake-agent-0.1.0-jar-with-dependencies.jar Main

public final class RattlesnakeAgent {

    public static void premain(String agentArgs, Instrumentation inst) {
        inst.addTransformer(new Transformer());
    }

}
