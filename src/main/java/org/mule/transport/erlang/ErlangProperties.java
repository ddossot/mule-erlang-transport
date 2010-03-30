package org.mule.transport.erlang;

public abstract class ErlangProperties {
    private ErlangProperties() {
        throw new UnsupportedOperationException("do not instantiate");
    }

    public static final String NODE_PROPERTY = "node";
    public static final String PROCESS_NAME_PROPERTY = "processName";
}
