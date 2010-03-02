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

import org.apache.commons.lang.Validate;
import org.mule.DefaultMuleMessage;
import org.mule.api.MuleEvent;
import org.mule.api.MuleMessage;
import org.mule.api.endpoint.EndpointURI;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.api.lifecycle.InitialisationException;
import org.mule.transport.AbstractMessageDispatcher;
import org.mule.transport.ConnectException;
import org.mule.transport.erlang.ErlangInvocation.InvocationType;
import org.mule.transport.erlang.i18n.ErlangMessages;
import org.mule.transport.erlang.transformers.ErlangConversionUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * <code>ErlangMessageDispatcher</code> TODO document
 */
public class ErlangMessageDispatcher extends AbstractMessageDispatcher {

    private final ErlangConnector connector;
    private final InvocationType invocationType;
    private final boolean failIfTimeout;
    private final String targetNodeName; // contains node or node@host
    private final String targetProcessName;

    private OtpMbox otpMbox;

    public ErlangMessageDispatcher(final OutboundEndpoint endpoint) {
        super(endpoint);
        connector = (ErlangConnector) endpoint.getConnector();

        invocationType = ErlangUtils.getInvocationType(endpoint);
        failIfTimeout = ErlangUtils.isFailIfTimeout(endpoint);

        final EndpointURI endpointURI = endpoint.getEndpointURI();
        targetNodeName = ErlangUtils.getErlangNodeName(endpointURI);
        targetProcessName = ErlangUtils.getProcessName(endpointURI);
    }

    @Override
    protected void doInitialise() throws InitialisationException {
        Validate.notEmpty(targetNodeName, ErlangMessages.missingEndpointProperty("targetNodeName").getMessage());
        Validate.notEmpty(targetProcessName, ErlangMessages.missingEndpointProperty("targetProcessName").getMessage());
        super.doInitialise();
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
        doInvokeRemote(event);
    }

    @Override
    public MuleMessage doSend(final MuleEvent event) throws Exception {
        final OtpErlangObject result = doInvokeRemote(event);
        return new DefaultMuleMessage(ErlangConversionUtils.erlangToJava(result), connector.getMuleContext());
    }

    private OtpErlangObject doInvokeRemote(final MuleEvent event) throws Exception {
        // supports message level override of the target process (for dynamic dispatch)
        final String invocationTargetProcessName = event.getMessage().getStringProperty(ErlangProperties.PROCESS_NAME_PROPERTY,
                targetProcessName);

        return new ErlangInvocation(event, otpMbox, invocationTargetProcessName, invocationType, failIfTimeout).call();
    }

    public OtpErlangPid getPid() {
        return otpMbox.self();
    }
}
