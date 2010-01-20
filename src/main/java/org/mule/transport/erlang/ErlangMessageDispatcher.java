/*
 * $Id$
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang;

import org.mule.api.MuleEvent;
import org.mule.api.MuleMessage;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.transport.AbstractMessageDispatcher;

import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpPeer;

/**
 * <code>ErlangMessageDispatcher</code> TODO document
 */
public class ErlangMessageDispatcher extends AbstractMessageDispatcher {

    // FIXME support message dispatching of the form {Pid, Msg}, waiting for
    // {Pid, Msg} when sending
    // FIXME support calling MFA
    // FIXME support gen_server call and cast
    // LATER allow disabling {Pid, _} wrapping/unwrapping

    private final ErlangConnector connector;
    private final OtpPeer otpPeer;
    private OtpConnection otpConnection;

    public ErlangMessageDispatcher(final OutboundEndpoint endpoint) {
        super(endpoint);
        connector = (ErlangConnector) endpoint.getConnector();
        // FIXME wrong! get value of nodeName attribute on endpoint
        otpPeer = new OtpPeer(endpoint.getEndpointURI().getAddress());
    }

    @Override
    public void doConnect() throws Exception {
        otpConnection = connector.connectToPeer(otpPeer);
    }

    @Override
    public void doDisconnect() throws Exception {
        otpConnection.close();
    }

    @Override
    public void doDispose() {
        // NOOP
    }

    @Override
    public void doDispatch(final MuleEvent event) throws Exception {
        /*
         * IMPLEMENTATION NOTE: This is invoked when the endpoint is
         * asynchronous. It should invoke the transport but not return any
         * result. If a result is returned it should be ignorred, but if the
         * underlying transport does have a notion of asynchronous processing,
         * that should be invoked. This method is executed in a different thread
         * to the request thread.
         */

        /*
         * IMPLEMENTATION NOTE: The event message needs to be transformed for
         * the outbound transformers to take effect. This isn't done
         * automatically in case the dispatcher needs to modify the message
         * before apllying transformers. To get the transformed outbound message
         * call - event.transformMessage();
         */

        // TODO Write the client code here to dispatch the event over this
        // transport
        throw new UnsupportedOperationException("doDispatch");
    }

    @Override
    public MuleMessage doSend(final MuleEvent event) throws Exception {
        /*
         * IMPLEMENTATION NOTE: Should send the event payload over the
         * transport. If there is a response from the transport it shuold be
         * returned from this method. The sendEvent method is called when the
         * endpoint is running synchronously and any response returned will
         * ultimately be passed back to the callee. This method is executed in
         * the same thread as the request thread.
         */

        /*
         * IMPLEMENTATION NOTE: The event message needs to be transformed for
         * the outbound transformers to take effect. This isn't done
         * automatically in case the dispatcher needs to modify the message
         * before apllying transformers. To get the transformed outbound message
         * call - event.transformMessage();
         */

        // TODO Write the client code here to send the event over this
        // transport (or to dispatch the event to a store or repository)
        // TODO Once the event has been sent, return the result (if any)
        // wrapped in a MuleMessage object
        throw new UnsupportedOperationException("doSend");
    }

}
