package org.mule.transport.erlang;

public abstract class ErlangProperties {
    private static final String ERLANG_PROPERTY_PREFIX = "erlang.";

    public static final String NODE_ATTRIBUTE = "node";
    public static final String PROCESS_NAME_ATTRIBUTE = "processName";
    public static final String MODULE_FUNCTION_ATTRIBUTE = "moduleFunction";

    public static final String PROCESS_NAME_PROPERTY = ERLANG_PROPERTY_PREFIX + PROCESS_NAME_ATTRIBUTE;
    public static final String MODULE_FUNCTION_PROPERTY = ERLANG_PROPERTY_PREFIX + MODULE_FUNCTION_ATTRIBUTE;

    private ErlangProperties() {
        throw new UnsupportedOperationException("do not instantiate");
    }
}
