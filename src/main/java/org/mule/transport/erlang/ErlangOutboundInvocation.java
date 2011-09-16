package org.mule.transport.erlang;

import java.util.concurrent.Callable;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mule.MessageExchangePattern;
import org.mule.api.MessagingException;
import org.mule.api.MuleEvent;
import org.mule.api.MuleMessage;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.transport.erlang.i18n.ErlangMessages;
import org.mule.transport.erlang.transformers.ErlangConversionUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

public class ErlangOutboundInvocation implements Callable<OtpErlangObject> {

    private static final Log LOGGER = LogFactory.getLog(ErlangOutboundInvocation.class);

    public static enum InvocationType {
        // Msg
        RAW,

        // {Pid, Msg}
        PID_WRAPPED {
            @Override
            OtpErlangObject preProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject payload) {
                return ErlangUtils.makeTuple(invocation.senderMbox.self(), payload);
            }

            @Override
            OtpErlangObject postProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject result) {
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
        GEN_CALL {
            @Override
            OtpErlangObject preProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject payload) {
                return ErlangUtils.makeTuple(ErlangUtils.GEN_CALL_SIGNATURE,
                        ErlangUtils.makeTuple(invocation.senderMbox.self(), invocation.connector.createRef()), payload);
            }

            @Override
            boolean isResponseExpected(final ErlangOutboundInvocation ignored) {
                return true;
            }

            @Override
            OtpErlangObject postProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject result) {
                if (!(result instanceof OtpErlangTuple)) {
                    throw new IllegalArgumentException(ErlangMessages.badResponseFormat(this).getMessage());
                }

                final OtpErlangTuple resultTuple = (OtpErlangTuple) result;
                if (resultTuple.arity() != 2) {
                    throw new IllegalArgumentException(ErlangMessages.badResponseFormat(this).getMessage());
                }

                // LATER check REF is what expected (ideally should pattern match on
                // inbox for this
                // ref)
                return resultTuple.elementAt(1);
            }
        },

        // {'$gen_cast',Msg}
        GEN_CAST {
            @Override
            OtpErlangObject preProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject payload) {
                return ErlangUtils.makeTuple(ErlangUtils.GEN_CAST_SIGNATURE, payload);
            }

            @Override
            boolean isResponseExpected(final ErlangOutboundInvocation ignored) {
                return false;
            }
        },

        // { self, { call, Mod, Fun, Args, user } } to process rex
        RPC {
            @Override
            String getTargetProcessName(final ErlangOutboundInvocation arg0) {
                return "rex";
            }

            @Override
            OtpErlangObject preProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject payload) {
                final String invocationTargetModuleFunction = lookupStringProperty(invocation.muleEvent.getMessage(),
                        ErlangProperties.MODULE_FUNCTION_PROPERTY,
                        ErlangUtils.getModuleFunction(invocation.outboundEndpoint.getEndpointURI().getUri()));

                final String[] invocationTargetModuleFunctionParts = invocationTargetModuleFunction.split(":");
                if (invocationTargetModuleFunctionParts.length != 2) {
                    throw new IllegalArgumentException(ErlangMessages.badModuleFunctionFormat(invocationTargetModuleFunction).toString());
                }

                final String module = invocationTargetModuleFunctionParts[0];
                final String function = invocationTargetModuleFunctionParts[1];

                final OtpErlangList arguments = (payload instanceof OtpErlangList) ? (OtpErlangList) payload : new OtpErlangList(payload);

                return ErlangUtils.makeTuple(invocation.senderMbox.self(), ErlangUtils.makeTuple(new OtpErlangAtom("call"),
                        new OtpErlangAtom(module), new OtpErlangAtom(function), arguments, new OtpErlangAtom("user")));
            }

            @Override
            boolean isResponseExpected(final ErlangOutboundInvocation ignored) {
                return true;
            }

            @Override
            OtpErlangObject postProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject result) {
                if (!(result instanceof OtpErlangTuple)) {
                    throw new IllegalArgumentException(ErlangMessages.badResponseFormat(this).getMessage());
                }

                final OtpErlangTuple resultTuple = (OtpErlangTuple) result;
                if (resultTuple.arity() != 2) {
                    throw new IllegalArgumentException(ErlangMessages.badResponseFormat(this).getMessage());
                }

                // LATER check element 0 is 'rex'
                return resultTuple.elementAt(1);
            }

        };

        final OtpErlangObject process(final ErlangOutboundInvocation invocation) throws Exception {
            final Object transformedPayload = invocation.muleEvent.transformMessage(Object.class);

            final OtpErlangObject payload = transformedPayload instanceof OtpErlangObject ? (OtpErlangObject) transformedPayload
                    : ErlangConversionUtils.javaToErlang(transformedPayload);

            final OtpErlangObject preProcessedPayload = preProcess(invocation, payload);

            invocation.senderMbox.send(getTargetProcessName(invocation), invocation.erlangNodeName, preProcessedPayload);

            if (isResponseExpected(invocation)) {
                final OtpErlangObject result = invocation.senderMbox.receive(invocation.muleEvent.getTimeout());

                if (result != null) {
                    return postProcess(invocation, result);
                }

                if (invocation.failIfTimeout) {
                    throw new MessagingException(ErlangMessages.responseTimeOut(), invocation.muleEvent);
                }
            }

            return null;
        }

        String getTargetProcessName(final ErlangOutboundInvocation invocation) {
            // TODO document this dynamic support
            return lookupStringProperty(invocation.muleEvent.getMessage(), ErlangProperties.PROCESS_NAME_PROPERTY,
                    ErlangUtils.getProcessName(invocation.outboundEndpoint.getEndpointURI().getUri()));
        }

        OtpErlangObject preProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject payload) {
            return payload;
        }

        boolean isResponseExpected(final ErlangOutboundInvocation invocation) {
            return invocation.muleEvent.getExchangePattern().equals(MessageExchangePattern.REQUEST_RESPONSE);
        }

        OtpErlangObject postProcess(final ErlangOutboundInvocation invocation, final OtpErlangObject result) {
            return result;
        }

    };

    private final MuleEvent muleEvent;
    private final OutboundEndpoint outboundEndpoint;
    private final ErlangConnector connector;
    private final String erlangNodeName;
    private final OtpMbox senderMbox;
    private final InvocationType invocationType;
    private final boolean failIfTimeout;

    public ErlangOutboundInvocation(final OutboundEndpoint outboundEndpoint, final MuleEvent muleEvent, final OtpMbox senderMbox,
            final InvocationType invocationType, final boolean failIfTimeout) {

        this.muleEvent = muleEvent;
        this.outboundEndpoint = outboundEndpoint;
        this.connector = (ErlangConnector) outboundEndpoint.getConnector();
        erlangNodeName = ErlangUtils.getErlangNodeName(outboundEndpoint.getEndpointURI().getUri());

        this.senderMbox = senderMbox;
        this.invocationType = invocationType;
        this.failIfTimeout = failIfTimeout;
    }

    public OtpErlangObject call() throws Exception {
        return invocationType.process(this);
    }

    private static String lookupStringProperty(final MuleMessage message, final String propName, final String defaultValue) {
        return message.findPropertyInAnyScope(propName, defaultValue);
    }
}
