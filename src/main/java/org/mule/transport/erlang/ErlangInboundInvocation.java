
package org.mule.transport.erlang;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mule.api.MuleEvent;
import org.mule.transport.NullPayload;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangInboundInvocation
{

    // LATER consider support inbound RPC (security risk: can invoke
    // java.lang.System.exit())

    private static final Log LOGGER = LogFactory.getLog(ErlangInboundInvocation.class);

    public static enum InvocationType
    {
        // Msg
        RAW,

        // {Pid, Msg}
        PID_WRAPPED
        {
            @Override
            public void respondIfNecessaryAndPossible(final ErlangInboundInvocation invocation,
                                                      final MuleEvent event)
            {
                if (noResponseAvailable(event))
                {
                    return;
                }

                final OtpErlangObject responseMessage = (OtpErlangObject) event.getMessage().getPayload();
                invocation.messageReceiver.respondToErlangProcess(invocation.callerPid, responseMessage);
            }
        },

        // {'$gen_call',{<Pid Sender>,Ref},Msg}
        GS_CALL
        {
            @Override
            public void respondIfNecessaryAndPossible(final ErlangInboundInvocation invocation,
                                                      final MuleEvent event)
            {
                if (noResponseAvailable(event))
                {
                    LOGGER.warn("Impossible to reply to caller for invocation type " + this
                                + " - No message or null payload in event: " + event);
                    return;
                }

                final OtpErlangObject responseMessage = ErlangUtils.makeTuple(invocation.callerRef,
                    (OtpErlangObject) event.getMessage().getPayload());

                invocation.messageReceiver.respondToErlangProcess(invocation.callerPid, responseMessage);
            }
        },

        // {'$gen_cast',Msg}
        GS_CAST;

        public void respondIfNecessaryAndPossible(final ErlangInboundInvocation invocation,
                                                  final MuleEvent event)
        {
            if (noResponseAvailable(event))
            {
                // nothing to respond and the invocation type doesn't call for a
                // response -> life is peachy
                return;
            }

            LOGGER.warn("Impossible to reply to caller for invocation type " + this + ": dropping event: "
                        + event);
        }
    };

    private final ErlangMessageReceiver messageReceiver;

    private InvocationType invocationType;
    private OtpErlangPid callerPid;
    private OtpErlangRef callerRef;
    private OtpErlangObject payloadToRoute;

    public ErlangInboundInvocation(final ErlangMessageReceiver messageReceiver,
                                   final OtpErlangObject receivedErlangObject)
    {
        this.messageReceiver = messageReceiver;

        invocationType = InvocationType.RAW;
        payloadToRoute = receivedErlangObject;

        if (receivedErlangObject instanceof OtpErlangTuple)
        {
            final OtpErlangTuple receivedTuple = (OtpErlangTuple) receivedErlangObject;
            if (receivedTuple.arity() == 3
                && receivedTuple.elementAt(0).equals(ErlangUtils.GEN_CALL_SIGNATURE))
            {
                invocationType = InvocationType.GS_CALL;
                final OtpErlangTuple callPidAndRef = (OtpErlangTuple) receivedTuple.elementAt(1);
                callerPid = (OtpErlangPid) callPidAndRef.elementAt(0);
                callerRef = (OtpErlangRef) callPidAndRef.elementAt(1);
                payloadToRoute = receivedTuple.elementAt(2);
            }
            else if (receivedTuple.arity() == 2
                     && receivedTuple.elementAt(0).equals(ErlangUtils.GEN_CAST_SIGNATURE))
            {
                invocationType = InvocationType.GS_CAST;
                payloadToRoute = receivedTuple.elementAt(1);
            }
            else if (receivedTuple.arity() == 2 && receivedTuple.elementAt(0) instanceof OtpErlangPid)
            {
                invocationType = InvocationType.PID_WRAPPED;
                callerPid = (OtpErlangPid) receivedTuple.elementAt(0);
                payloadToRoute = receivedTuple.elementAt(1);
            }
        }
    }

    public OtpErlangObject getPayloadToRoute()
    {
        return payloadToRoute;
    }

    public void respondIfNecessaryAndPossible(final MuleEvent event)
    {
        invocationType.respondIfNecessaryAndPossible(this, event);
    }

    @Override
    public String toString()
    {
        return ToStringBuilder.reflectionToString(this, ToStringStyle.SIMPLE_STYLE);
    }

    private static boolean noResponseAvailable(final MuleEvent event)
    {
        return (event == null) || (event.getMessage().getPayload() == null)
               || (event.getMessage().getPayload() instanceof NullPayload);
    }
}
