/*
 * $Id: MessageReceiver.vm 11079 2008-02-27 15:52:01Z tcarlson $
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang;

import javax.resource.spi.work.Work;
import javax.resource.spi.work.WorkManager;

import org.mule.DefaultMuleMessage;
import org.mule.api.MuleException;
import org.mule.api.MuleMessage;
import org.mule.api.endpoint.InboundEndpoint;
import org.mule.api.lifecycle.CreateException;
import org.mule.api.service.Service;
import org.mule.api.transport.Connector;
import org.mule.transport.AbstractPollingMessageReceiver;
import org.mule.transport.ConnectException;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * <code>ErlangMessageReceiver</code> TODO document
 */
public class ErlangMessageReceiver extends AbstractPollingMessageReceiver {

    private final ErlangConnector connector;
    private OtpMbox otpMbox;

    public ErlangMessageReceiver(final Connector connector, final Service service, final InboundEndpoint endpoint)
            throws CreateException {
        super(connector, service, endpoint);
        this.connector = (ErlangConnector) connector;
    }

    @Override
    public void doConnect() throws ConnectException {
        final String processName = ErlangUtils.getProcessName(endpoint.getEndpointURI());
        otpMbox = connector.createMailBox();
        if (!otpMbox.registerName(processName)) {
            throw new ConnectException(ErlangMessages.mailboxNameRegistrationFailed(processName), this);
        }
    }

    @Override
    public void doDisconnect() throws ConnectException {
        otpMbox.registerName(null);
        otpMbox.close();
    }

    @Override
    public void doDispose() {
        otpMbox = null;
    }

    @Override
    public void poll() throws Exception {
        final OtpErlangObject oeo = otpMbox.receive(getFrequency() / 2);

        if (oeo != null) {
            getWorkManager().scheduleWork(new ErlangMessageReceiverWorker(oeo), WorkManager.INDEFINITE, null, null);
        }
    }

    private final class ErlangMessageReceiverWorker implements Work {

        private final OtpErlangObject oeo;

        public ErlangMessageReceiverWorker(final OtpErlangObject oeo) {
            this.oeo = oeo;
        }

        public void release() {
            // noop
        }

        public void run() {
            try {
                OtpErlangObject dispatchedPayload = oeo;

                // FIXME this code must go elsewhere
                if (oeo instanceof OtpErlangTuple) {
                    final OtpErlangTuple oet = (OtpErlangTuple) oeo;
                    if (oet.arity() == 3 && oet.elementAt(0).equals(new OtpErlangAtom("$gen_call"))) {
                        dispatchedPayload = oet.elementAt(2);
                    }
                }

                // FIXME add properties: erlang.invocationType(RAW,PID_WRAPPED,GS_CALL,GS_CAST),erlang.ref,erlang.remotePid
                final MuleMessage result = routeMessage(new DefaultMuleMessage(dispatchedPayload, connector.getMuleContext()));

                if (result != null && result.getPayload() instanceof OtpErlangObject) {
                    // FIXME respond to caller if possible - if result is present but can't send response back to Erlang -->
                    // WARN!

                    // FIXME this code must go elsewhere
                    if (oeo instanceof OtpErlangTuple) {
                        final OtpErlangTuple oet = (OtpErlangTuple) oeo;
                        if (oet.arity() == 3 && oet.elementAt(0).equals(new OtpErlangAtom("$gen_call"))) {
                            final OtpErlangTuple callPidAndRef = (OtpErlangTuple) oet.elementAt(1);
                            final OtpErlangPid callPid = (OtpErlangPid) callPidAndRef.elementAt(0);
                            final OtpErlangRef callRef = (OtpErlangRef) callPidAndRef.elementAt(1);

                            otpMbox.send(callPid, ErlangUtils.makeTuple(callRef, (OtpErlangObject) result.getPayload()));
                        }
                    }
                }

            } catch (final MuleException me) {
                connector.handleException(me);
            }
        }
    }

}
