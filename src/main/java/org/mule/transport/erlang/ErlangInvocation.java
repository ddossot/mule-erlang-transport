package org.mule.transport.erlang;

import java.util.concurrent.Callable;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mule.api.MessagingException;
import org.mule.api.MuleEvent;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

public class ErlangInvocation implements Callable<OtpErlangObject> {

    private static final Log LOGGER = LogFactory.getLog(ErlangInvocation.class);

    private static final OtpErlangAtom OK_ATOM = new OtpErlangAtom("ok");

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
                return ErlangUtils.makeTuple(new OtpErlangAtom("$gen_call"), ErlangUtils.makeTuple(
                        invocation.senderMbox.self(), invocation.connector.createRef()), payload);
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

        // {'$gen_cast',hello}
        GS_CAST {
            @Override
            OtpErlangObject preProcess(final ErlangInvocation invocation, final OtpErlangObject payload) {
                return ErlangUtils.makeTuple(new OtpErlangAtom("$gen_cast"), payload);
            }

            @Override
            OtpErlangObject getResponse(final ErlangInvocation arg0) throws OtpErlangExit, OtpErlangDecodeException,
                    MessagingException {
                return OK_ATOM;
            }
        };

        OtpErlangObject process(final ErlangInvocation invocation) throws Exception {
            final OtpErlangObject payload = (OtpErlangObject) invocation.muleEvent.transformMessage();
            final OtpErlangObject preProcessedPayload = preProcess(invocation, payload);
            // FIXME take node name from endpoint
            invocation.senderMbox.send(invocation.invocationTargetProcessName, "mule_test_server_node@ddossot-laptop",
                    preProcessedPayload);

            if (invocation.muleEvent.isSynchronous()) {
                return getResponse(invocation);
            }

            return null;
        }

        OtpErlangObject getResponse(final ErlangInvocation invocation) throws OtpErlangExit, OtpErlangDecodeException,
                MessagingException {

            final OtpErlangObject result = invocation.senderMbox.receive(invocation.muleEvent.getTimeout());

            if (result != null) {
                return postProcess(invocation, result);
            }

            if (invocation.failIfTimeout) {
                throw new MessagingException(ErlangMessages.responseTimeOut(), invocation.muleEvent.getMessage());
            }

            return null;
        }

        OtpErlangObject preProcess(final ErlangInvocation invocation, final OtpErlangObject payload) {
            return payload;
        }

        OtpErlangObject postProcess(final ErlangInvocation invocation, final OtpErlangObject result) {
            return result;
        }

    };

    private final ErlangConnector connector;
    private final OtpMbox senderMbox;
    private final String invocationTargetProcessName;
    private final InvocationType invocationType;
    private final boolean failIfTimeout;
    private final MuleEvent muleEvent;

    public ErlangInvocation(final ErlangConnector connector, final OtpMbox senderMbox,
            final String invocationTargetProcessName, final InvocationType invocationType, final boolean failIfTimeout,
            final MuleEvent muleEvent) {
        this.connector = connector;
        this.senderMbox = senderMbox;
        this.invocationTargetProcessName = invocationTargetProcessName;
        this.invocationType = invocationType;
        this.failIfTimeout = failIfTimeout;
        this.muleEvent = muleEvent;
    }

    public OtpErlangObject call() throws Exception {
        return invocationType.process(this);
    }

}
