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

import org.mule.api.MuleException;
import org.mule.api.lifecycle.InitialisationException;
import org.mule.transport.AbstractConnector;

/**
 * <code>ErlangConnector</code> TODO document
 */
public class ErlangConnector extends AbstractConnector {
    /* This constant defines the main transport protocol identifier */
    public static final String MULETRANSPORTERLANG = "erlang";

    private String cookie;
    private String nodeName;
    private Integer port;

    @Override
    public void doInitialise() throws InitialisationException {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Is called once all bean properties have been set
         * on the connector and can be used to validate and initialise the
         * connectors state.
         */
    }

    @Override
    public void doConnect() throws Exception {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Makes a connection to the underlying resource.
         * When connections are managed at the receiver/dispatcher level, this
         * method may do nothing
         */
    }

    @Override
    public void doDisconnect() throws Exception {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Disconnects any connections made in the connect
         * method If the connect method did not do anything then this method
         * shouldn't do anything either.
         */
    }

    @Override
    public void doStart() throws MuleException {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: If there is a single server instance or
         * connection associated with the connector i.e. AxisServer or a Jms
         * Connection or Jdbc Connection, this method should put the resource in
         * a started state here.
         */
    }

    @Override
    public void doStop() throws MuleException {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Should put any associated resources into a
         * stopped state. Mule will automatically call the stop() method.
         */
    }

    @Override
    public void doDispose() {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Should clean up any open resources associated
         * with the connector.
         */
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
