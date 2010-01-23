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

import org.mule.DefaultMuleMessage;
import org.mule.api.MuleEvent;
import org.mule.api.MuleMessage;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.transport.AbstractMessageDispatcher;
import org.mule.transport.ConnectException;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * <code>ErlangMessageDispatcher</code> TODO document
 */
public class ErlangMessageDispatcher extends AbstractMessageDispatcher {

    // FIXME support message dispatching of the form {Pid, Msg}, waiting for
    // {Pid, Msg} when sending
    // FIXME support gen_server call and cast
    // LATER allow disabling {Pid, _} wrapping/unwrapping

    private final ErlangConnector connector;
    private final String targetNodeName; // contains node or node@host
    private final String targetProcessName;

    private OtpMbox otpMbox;

    public ErlangMessageDispatcher(final OutboundEndpoint endpoint) {
        super(endpoint);
        connector = (ErlangConnector) endpoint.getConnector();

        // FIXME wrong! get value of nodeName attribute on endpoint
        targetNodeName = endpoint.getEndpointURI().getAddress();
        targetProcessName = endpoint.getEndpointURI().getPath();
    }

    @Override
    public void doConnect() throws Exception {
        otpMbox = connector.createMailBox();

        if (!otpMbox.ping(targetNodeName, 1000L)) {
            throw new ConnectException(ErlangMessages.nodeUnreachable(targetNodeName), this);
        }
    }

    @Override
    public void doDisconnect() throws Exception {
        otpMbox.close();
    }

    @Override
    public void doDispose() {
        otpMbox = null;
    }

    @Override
    public void doDispatch(final MuleEvent event) throws Exception {
        final String targetMailBox = event.getMessage().getStringProperty(ErlangProperties.PROCESS_NAME_PROPERTY, targetProcessName);
        final OtpErlangObject payload = (OtpErlangObject) event.transformMessage();
        otpMbox.send(targetMailBox, payload);
    }

    @Override
    public MuleMessage doSend(final MuleEvent event) throws Exception {
        doDispatch(event);
        final OtpErlangObject result = otpMbox.receive(event.getTimeout());
        return new DefaultMuleMessage(result, connector.getMuleContext());
    }
}
