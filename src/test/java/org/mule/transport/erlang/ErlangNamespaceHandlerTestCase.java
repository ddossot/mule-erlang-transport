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
import org.mule.routing.outbound.OutboundPassThroughRouter;
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
        testEndpointConfiguration(endpointFactory, "erlangEndpoint1", "localNode1", "process1",
                ErlangInvocation.InvocationType.PID_WRAPPED, false);
        testEndpointConfiguration(endpointFactory, "erlangEndpoint2", "remoteNode2@hostName", "process2",
                ErlangInvocation.InvocationType.PID_WRAPPED, false);
        testEndpointConfiguration(endpointFactory, "erlangEndpoint3", "remoteNode3@hostName", "process3",
                ErlangInvocation.InvocationType.GS_CALL, true);
    }

    public void testEndpointsInService() throws Exception {
        assertEquals("erlang://connector.nodeName/test", muleContext.getRegistry().lookupService("test").getInboundRouter()
                .getEndpoint("erlin").getEndpointURI().toString());

        final OutboundPassThroughRouter optr = (OutboundPassThroughRouter) muleContext.getRegistry().lookupService("test")
                .getOutboundRouter().getRouters().get(0);

        assertEquals("erlang://remoteNode4@hostName/process4", optr.getEndpoint("erlout").getEndpointURI().toString());
    }

    private void testEndpointConfiguration(final EndpointFactory endpointFactory, final String endpointName,
            final String expectedErlangNodeName, final String exectedProcessName,
            final ErlangInvocation.InvocationType expectedInvocationType, final boolean expectedIsFailIfTimeout)
            throws MuleException {

        final OutboundEndpoint outboundEndpoint = endpointFactory.getOutboundEndpoint(endpointName);
        assertEquals(expectedInvocationType, ErlangUtils.getInvocationType(outboundEndpoint));
        assertEquals(expectedIsFailIfTimeout, ErlangUtils.isFailIfTimeout(outboundEndpoint));

        final EndpointURI endpointURI = outboundEndpoint.getEndpointURI();
        assertEquals(expectedErlangNodeName, ErlangUtils.getErlangNodeName(endpointURI));
        assertEquals(exectedProcessName, ErlangUtils.getProcessName(endpointURI));
    }

}
