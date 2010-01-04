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
import org.mule.api.MuleException;
import org.mule.api.config.ConfigurationException;
import org.mule.api.lifecycle.InitialisationException;
import org.mule.transport.AbstractConnector;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpNode;

/**
 * <code>ErlangConnector</code> TODO document
 */
public class ErlangConnector extends AbstractConnector {
    /* This constant defines the main transport protocol identifier */
    public static final String MULETRANSPORTERLANG = "erlang";

    private static final Integer DEFAULT_PORT = 0;

    private String nodeName;
    private String cookie;
    private Integer port;
    // TODO add an option for attempting to start EPMD if not running

    private OtpNode otpNode;

    @Override
    protected void doInitialise() throws InitialisationException {
        // NOOP
    }

    @Override
    public void doConnect() throws Exception {
        Validate.notEmpty(ErlangMessages.missingNodeName().getMessage(), nodeName);

        if (port != null && !DEFAULT_PORT.equals(port)) {
            if (cookie == null) {
                throw new ConfigurationException(ErlangMessages.missingCookieWithPort());
            }
            otpNode = new OtpNode(nodeName, cookie, port);

        } else {
            if (cookie != null) {
                otpNode = new OtpNode(nodeName, cookie);
            } else {
                otpNode = new OtpNode(nodeName);
            }
        }

        logger.info("OTP Node " + otpNode.alive() + "@" + otpNode.node() + ":" + otpNode.port() + " is ready.");
    }

    @Override
    protected void doStart() throws MuleException {
        // NOOP
    }

    @Override
    protected void doStop() throws MuleException {
        // NOOP
    }

    @Override
    public void doDisconnect() throws Exception {
        otpNode.close();
    }

    @Override
    protected void doDispose() {
        // NOOP
    }

    public String getProtocol() {
        return MULETRANSPORTERLANG;
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
