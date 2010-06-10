package org.mule.transport.erlang;

import javax.resource.spi.work.Work;

import org.mule.api.MuleException;
import org.mule.api.MuleMessage;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;

class ErlangMessageRouterWorker implements Work {

    private final ErlangMessageReceiver messageReceiver;
    private final OtpErlangObject receivedErlangObject;

    public ErlangMessageRouterWorker(final ErlangMessageReceiver messageReceiver, final OtpErlangObject receivedErlangObject) {
        this.messageReceiver = messageReceiver;
        this.receivedErlangObject = receivedErlangObject;
    }

    public void run() {
        try {
            OtpErlangObject dispatchedPayload = receivedErlangObject;

            // FIXME support other invocation styles
            if (receivedErlangObject instanceof OtpErlangTuple) {
                final OtpErlangTuple oet = (OtpErlangTuple) receivedErlangObject;
                if (oet.arity() == 3 && oet.elementAt(0).equals(ErlangUtils.GEN_CALL_SIGNATURE)) {
                    dispatchedPayload = oet.elementAt(2);
                }
            }

            // FIXME add properties: erlang.invocationType(RAW,PID_WRAPPED,GS_CALL,GS_CAST),erlang.ref,erlang.remotePid
            final MuleMessage routedMessage = messageReceiver.createMuleMessage(dispatchedPayload);
            final MuleMessage result = messageReceiver.routeMessage(routedMessage);

            if (result != null && result.getPayload() instanceof OtpErlangObject) {
                // FIXME respond to caller if possible - if result is present but can't send response back to Erlang -->
                // WARN!

                // FIXME support other invocation styles
                if (receivedErlangObject instanceof OtpErlangTuple) {
                    final OtpErlangTuple oet = (OtpErlangTuple) receivedErlangObject;
                    if (oet.arity() == 3 && oet.elementAt(0).equals(ErlangUtils.GEN_CALL_SIGNATURE)) {
                        final OtpErlangTuple callPidAndRef = (OtpErlangTuple) oet.elementAt(1);
                        final OtpErlangPid callPid = (OtpErlangPid) callPidAndRef.elementAt(0);
                        final OtpErlangRef callRef = (OtpErlangRef) callPidAndRef.elementAt(1);

                        messageReceiver.respondToErlangProcess(callPid, ErlangUtils.makeTuple(callRef, (OtpErlangObject) result
                                .getPayload()));
                    }
                }
            }

        } catch (final MuleException me) {
            messageReceiver.handleException(me);
        }
    }

    public void release() {
        // noop
    }
}