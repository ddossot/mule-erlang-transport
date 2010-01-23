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
import org.mule.api.endpoint.EndpointFactory;
import org.mule.api.endpoint.EndpointURI;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.tck.FunctionalTestCase;
import org.mule.transport.erlang.transformers.ErlangMessageToObject;
import org.mule.transport.erlang.transformers.ObjectToErlangMessage;

public class ErlangNamespaceHandlerTestCase extends FunctionalTestCase {

    public ErlangNamespaceHandlerTestCase() {
        super();
        setStartContext(false);
    }

    @Override
    protected String getConfigResources() {
        return "erlang-namespace-config.xml";
    }

    public void testConnectorConfigurations() throws Exception {
        testConnectorConfiguration("erlangConnector1", "muleErlang1", null, Integer.valueOf(0));
        testConnectorConfiguration("erlangConnector2", "muleErlang2", "xyz2", Integer.valueOf(0));
        testConnectorConfiguration("erlangConnector3", "muleErlang3", "xyz3", Integer.valueOf(30103));
    }

    private void testConnectorConfiguration(final String connectorName, final String nodeName, final String cookie,
            final Integer port) throws Exception {

        final ErlangConnector c = (ErlangConnector) muleContext.getRegistry().lookupConnector(connectorName);
        assertNotNull(c);

        assertEquals(connectorName, c.getName());
        assertEquals(nodeName, c.getNodeName());
        assertEquals(cookie, c.getCookie());
        assertEquals(port, c.getPort());
    }

    public void testTransformersConfiguration() throws Exception {
        assertTrue("ErlangMessageToObject configured",
                muleContext.getRegistry().lookupTransformer("e2oTransformer") instanceof ErlangMessageToObject);

        assertTrue("ObjectToErlangMessage configured",
                muleContext.getRegistry().lookupTransformer("o2eTransformer") instanceof ObjectToErlangMessage);
    }

    public void testOutboundEndpointConfiguration() throws Exception {
        final EndpointFactory endpointFactory = muleContext.getRegistry().lookupEndpointFactory();
        testEndpointConfiguration(endpointFactory, "erlangEndpoint1");
        testEndpointConfiguration(endpointFactory, "erlangEndpoint2");
        testEndpointConfiguration(endpointFactory, "erlangEndpoint3");
    }

    private void testEndpointConfiguration(final EndpointFactory endpointFactory, final String endpointName)
            throws MuleException {
        // FIXME assert stuff
        final OutboundEndpoint outboundEndpoint = endpointFactory.getOutboundEndpoint(endpointName);
        System.out.println(outboundEndpoint);
        final EndpointURI endpointURI = outboundEndpoint.getEndpointURI();
        System.out.println(endpointURI);
        System.out.println(endpointURI.getHost());
        System.out.println(endpointURI.getPath());
        System.out.println(endpointURI.getParams());
    }
}
