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

import java.io.IOException;

import org.apache.commons.lang.Validate;
import org.mule.api.MuleException;
import org.mule.api.lifecycle.InitialisationException;
import org.mule.api.lifecycle.StartException;
import org.mule.api.lifecycle.StopException;
import org.mule.api.transport.ConnectorException;
import org.mule.transport.AbstractConnector;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

/**
 * <code>ErlangConnector</code> TODO document
 */
public class ErlangConnector extends AbstractConnector {
    /* This constant defines the main transport protocol identifier */
    public static final String ERLANG = "erlang";

    private static final Integer DEFAULT_PORT = 0;

    private String nodeName;
    private String cookie;
    private Integer port;
    // TODO add an option for attempting to start EPMD if not running

    private OtpSelf otpSelf;

    @Override
    protected void doInitialise() throws InitialisationException {
        Validate.notEmpty(ErlangMessages.missingNodeName().getMessage(), nodeName);
    }

    @Override
    public void doConnect() throws Exception {
        if (port != null && !DEFAULT_PORT.equals(port)) {
            if (cookie == null) {
                throw new ConnectorException(ErlangMessages.missingCookieWithPort(), this);
            }
            otpSelf = new OtpSelf(nodeName, cookie, port);

        } else {
            if (cookie != null) {
                otpSelf = new OtpSelf(nodeName, cookie);
            } else {
                otpSelf = new OtpSelf(nodeName);
            }
        }

        logger.info("OTP Node " + otpSelf.alive() + "@" + otpSelf.node() + ":" + otpSelf.port() + " is ready.");
    }

    @Override
    protected void doStart() throws MuleException {
        try {
            otpSelf.publishPort();
        } catch (final IOException ioe) {
            throw new StartException(ioe, this);
        }
    }

    @Override
    protected void doStop() throws MuleException {
        otpSelf.unPublishPort();
    }

    @Override
    public void doDisconnect() throws Exception {
        // NOOP
    }

    @Override
    protected void doDispose() {
        otpSelf = null;
    }

    public OtpConnection connectToPeer(final OtpPeer otpPeer) throws OtpAuthException, IOException {
        return otpSelf.connect(otpPeer);
    }

    public String getProtocol() {
        return ERLANG;
    }

    public String getCookie() {
        return cookie;
    }

    public void setCookie(final String cookie) {
        this.cookie = cookie;
    }

    public String getNodeName() {
        return nodeName;
    }

    public void setNodeName(final String nodeName) {
        this.nodeName = nodeName;
    }

    public Integer getPort() {
        return port;
    }

    public void setPort(final Integer port) {
        this.port = port;
    }

}
