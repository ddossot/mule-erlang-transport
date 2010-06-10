package org.mule.transport.erlang;

import java.util.concurrent.Callable;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mule.api.MessagingException;
import org.mule.api.MuleEvent;
import org.mule.api.endpoint.ImmutableEndpoint;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

public class ErlangInvocation implements Callable<OtpErlangObject> {

    private static final Log LOGGER = LogFactory.getLog(ErlangInvocation.class);

    private static final OtpErlangAtom GS_CAST_SIGNATURE = new OtpErlangAtom("$gen_cast");

    private static final OtpErlangAtom GEN_CALL_SIGNATURE = new OtpErlangAtom("$gen_call");

    public static enum InvocationType {
        // Msg
        RAW,

        // {Pid, Msg}
        PID_WRAPPED {
            @Override
            OtpErlangObject preProcess(final ErlangInvocation invocation, final OtpErlangObject payload) {
                return ErlangUtils.makeTuple(invocation.senderMbox.self(), payload);
            }

            @Override
            OtpErlangObject postProcess(final ErlangInvocation invocation, final OtpErlangObject result) {
                // we're a little lax with Pid wrapping
                if (!(result instanceof OtpErlangTuple)) {
                    LOGGER.warn(ErlangMessages.badResponseFormat(this).getMessage());
                    return result;
                }

                final OtpErlangTuple resultTuple = (OtpErlangTuple) result;
                if (resultTuple.arity() != 2) {
                    LOGGER.warn(ErlangMessages.badResponseFormat(this).getMessage());
                    return result;
                }

                return resultTuple.elementAt(1);
            }
        },

        // {'$gen_call',{<Pid Sender>,Ref},Msg}
        GS_CALL {
            @Override
            OtpErlangObject preProcess(final ErlangInvocation invocation, final OtpErlangObject payload) {
                return ErlangUtils.makeTuple(GEN_CALL_SIGNATURE, ErlangUtils.makeTuple(invocation.senderMbox.self(),
                        invocation.connector.createRef()), payload);
            }

            @Override
            boolean isResponseExpected(final ErlangInvocation ignored) {
                return true;
            }

            @Override
            OtpErlangObject postProcess(final ErlangInvocation invocation, final OtpErlangObject result) {
                if (!(result instanceof OtpErlangTuple)) {
                    throw new IllegalArgumentException(ErlangMessages.badResponseFormat(this).getMessage());
                }

                final OtpErlangTuple resultTuple = (OtpErlangTuple) result;
                if (resultTuple.arity() != 2) {
                    throw new IllegalArgumentException(ErlangMessages.badResponseFormat(this).getMessage());
                }

                // TODO check REF is what expected (ideally should pattern match on inbox for this ref)
                return resultTuple.elementAt(1);
            }
        },

        // {'$gen_cast',Msg}
        GS_CAST {
            @Override
            OtpErlangObject preProcess(final ErlangInvocation invocation, final OtpErlangObject payload) {
                return ErlangUtils.makeTuple(GS_CAST_SIGNATURE, payload);
            }

            @Override
            boolean isResponseExpected(final ErlangInvocation ignored) {
                return false;
            }
        };

        OtpErlangObject process(final ErlangInvocation invocation) throws Exception {
            final OtpErlangObject payload = (OtpErlangObject) invocation.muleEvent.transformMessage();

            final OtpErlangObject preProcessedPayload = preProcess(invocation, payload);

            invocation.senderMbox.send(invocation.invocationTargetProcessName, invocation.erlangNodeName, preProcessedPayload);

            if (isResponseExpected(invocation)) {
                final OtpErlangObject result = invocation.senderMbox.receive(invocation.muleEvent.getTimeout());

                if (result != null) {
                    return postProcess(invocation, result);
                }

                if (invocation.failIfTimeout) {
                    throw new MessagingException(ErlangMessages.responseTimeOut(), invocation.muleEvent.getMessage());
                }
            }

            return null;
        }

        OtpErlangObject preProcess(final ErlangInvocation invocation, final OtpErlangObject payload) {
            return payload;
        }

        boolean isResponseExpected(final ErlangInvocation invocation) {
            return invocation.muleEvent.isSynchronous();
        }

        OtpErlangObject postProcess(final ErlangInvocation invocation, final OtpErlangObject result) {
            return result;
        }

    };

    // TODO replace with a builder
    private final MuleEvent muleEvent;
    private final ErlangConnector connector;
    private final String erlangNodeName;
    private final OtpMbox senderMbox;
    private final String invocationTargetProcessName;
    private final InvocationType invocationType;
    private final boolean failIfTimeout;

    public ErlangInvocation(final MuleEvent muleEvent, final OtpMbox senderMbox, final String invocationTargetProcessName,
            final InvocationType invocationType, final boolean failIfTimeout) {

        this.muleEvent = muleEvent;
        final ImmutableEndpoint endpoint = muleEvent.getEndpoint();
        connector = (ErlangConnector) endpoint.getConnector();
        erlangNodeName = ErlangUtils.getErlangNodeName(endpoint.getEndpointURI());

        this.senderMbox = senderMbox;
        this.invocationTargetProcessName = invocationTargetProcessName;
        this.invocationType = invocationType;
        this.failIfTimeout = failIfTimeout;
    }

    public OtpErlangObject call() throws Exception {
        return invocationType.process(this);
    }

}
