package org.mule.transport.erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class ErlangProperties {
    private ErlangProperties() {
        throw new UnsupportedOperationException("do not instantiate");
    }

    public static enum InvocationType {
        // Msg
        RAW {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                return invocationContext.getMessage();
            }
        },
        // {Pid, Msg}
        PID_WRAPPED {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                return ErlangUtils.makeTuple(invocationContext.getErlangPidWrapper().getPid(), invocationContext.getMessage());
            }
        },
        // {'$gen_call',{<Pid Sender>,Ref},Msg}
        GS_CALL {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                return ErlangUtils.makeTuple(new OtpErlangAtom("gen_call"), ErlangUtils.makeTuple(invocationContext
                        .getErlangPidWrapper().getPid(), invocationContext.getErlangReferenceFactory().createRef()),
                        invocationContext.getMessage());
            }
        },
        // {'$gen_cast',hello}
        GS_CAST {
            @Override
            OtpErlangObject makeInvocation(final InvocationContext invocationContext) {
                return ErlangUtils.makeTuple(new OtpErlangAtom("$gen_cast"), invocationContext.getMessage());
            }
        };

        abstract OtpErlangObject makeInvocation(InvocationContext invocationContext);
        // FIXME add parseResult, mainly for gen_server call response
    };

    public interface InvocationContext {
        OtpErlangObject getMessage();

        ErlangPidWrapper getErlangPidWrapper();

        ErlangReferenceFactory getErlangReferenceFactory();
    }

    public static final String NODE_NAME_PROPERTY = "nodeName";
    public static final String PROCESS_NAME_PROPERTY = "processName";
}
