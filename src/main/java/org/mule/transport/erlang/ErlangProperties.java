package org.mule.transport.erlang;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class ErlangProperties {
    private ErlangProperties() {
        throw new UnsupportedOperationException("do not instantiate");
    }

    public static enum InvocationType {
        RAW {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                return invocationContext.getMessage();
            }
        },
        PID_WRAPPED {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                return new OtpErlangTuple(new OtpErlangObject[] { invocationContext.getErlangPidWrapper().getPid(),
                        invocationContext.getMessage() });
            }
        },
        GS_CALL {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                // FIXME code me ;-)
                throw new UnsupportedOperationException("implement me!");
            }
        },
        GS_CAST {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                // FIXME code me ;-)
                throw new UnsupportedOperationException("implement me!");
            }
        };

        abstract OtpErlangObject makeInvocation(InvocationContext invocationContext);
    };

    public interface InvocationContext {
        OtpErlangObject getMessage();

        ErlangPidWrapper getErlangPidWrapper();

        ErlangReferenceFactory getErlangReferenceFactory();
    }

    public static final String NODE_NAME_PROPERTY = "nodeName";
    public static final String PROCESS_NAME_PROPERTY = "processName";
}
