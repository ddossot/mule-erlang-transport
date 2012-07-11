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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.mule.api.MuleException;
import org.mule.api.endpoint.EndpointFactory;
import org.mule.api.endpoint.EndpointURI;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.api.service.Service;
import org.mule.routing.outbound.DefaultOutboundRouterCollection;
import org.mule.routing.outbound.OutboundPassThroughRouter;
import org.mule.service.ServiceCompositeMessageSource;
import org.mule.tck.junit4.FunctionalTestCase;
import org.mule.transport.erlang.transformers.ErlangMessageToObject;
import org.mule.transport.erlang.transformers.ObjectToErlangMessage;

public class ErlangNamespaceHandlerTestCase extends FunctionalTestCase
{
    public ErlangNamespaceHandlerTestCase()
    {
        super();
        setStartContext(false);
    }

    @Override
    protected String getConfigResources()
    {
        return "erlang-namespace-config.xml";
    }

    @Test
    public void testConnectorConfigurations() throws Exception
    {
        testConnectorConfiguration("erlangConnector1", "muleErlang1", null, Integer.valueOf(0));
        testConnectorConfiguration("erlangConnector2", "muleErlang2", "xyz2", Integer.valueOf(0));
        testConnectorConfiguration("erlangConnector3", "muleErlang3", "xyz3", Integer.valueOf(30103));
    }

    private void testConnectorConfiguration(final String connectorName,
                                            final String nodeName,
                                            final String cookie,
                                            final Integer port) throws Exception
    {

        final ErlangConnector c = (ErlangConnector) muleContext.getRegistry().lookupConnector(connectorName);
        assertNotNull(c);

        assertEquals(connectorName, c.getName());
        assertEquals(nodeName, c.getNodeName());
        assertEquals(cookie, c.getCookie());
        assertEquals(port, c.getPort());
    }

    @Test
    public void testTransformersConfiguration() throws Exception
    {
        assertTrue("ErlangMessageToObject configured",
            muleContext.getRegistry().lookupTransformer("e2oTransformer") instanceof ErlangMessageToObject);

        assertTrue("ObjectToErlangMessage configured",
            muleContext.getRegistry().lookupTransformer("o2eTransformer") instanceof ObjectToErlangMessage);
    }

    @Test
    public void testOutboundEndpointConfiguration() throws Exception
    {
        final EndpointFactory endpointFactory = muleContext.getEndpointFactory();
        testEndpointConfiguration(endpointFactory, "erlangEndpoint1", "localNode1", "process1",
            ErlangOutboundInvocation.InvocationType.PID_WRAPPED, false);
        testEndpointConfiguration(endpointFactory, "erlangEndpoint2", "remoteNode2@hostName", "process2",
            ErlangOutboundInvocation.InvocationType.PID_WRAPPED, false);
        testEndpointConfiguration(endpointFactory, "erlangEndpoint3", "remoteNode3@hostName", "process3",
            ErlangOutboundInvocation.InvocationType.GEN_CALL, true);
        testEndpointConfiguration(endpointFactory, "erlangEndpoint4", "remoteNode4@hostName", "erlang:node",
            ErlangOutboundInvocation.InvocationType.RPC, true);
    }

    @Test
    public void testEndpointsInService() throws Exception
    {
        final Service testService = muleContext.getRegistry().lookupService("test-service");
        final EndpointURI inboundEndpointURI = ((ServiceCompositeMessageSource) testService.getMessageSource()).getEndpoint(
            "erlin")
            .getEndpointURI();
        assertEquals("erlang://connector.nodeName/process4", inboundEndpointURI.toString());
        assertEquals("process4", ErlangUtils.getProcessName(inboundEndpointURI.getUri()));

        final OutboundPassThroughRouter optr = (OutboundPassThroughRouter) ((DefaultOutboundRouterCollection) testService.getOutboundMessageProcessor()).getRoutes()
            .get(0);

        final EndpointURI outboundEndpointURI = ((OutboundEndpoint) optr.getRoute("erlout")).getEndpointURI();
        assertEquals("erlang://remoteNode4@hostName/process5", outboundEndpointURI.toString());
        assertEquals("process5", ErlangUtils.getProcessName(outboundEndpointURI.getUri()));
    }

    private void testEndpointConfiguration(final EndpointFactory endpointFactory,
                                           final String endpointName,
                                           final String expectedErlangNodeName,
                                           final String exectedProcessName,
                                           final ErlangOutboundInvocation.InvocationType expectedInvocationType,
                                           final boolean expectedIsFailIfTimeout) throws MuleException
    {

        final OutboundEndpoint outboundEndpoint = endpointFactory.getOutboundEndpoint(endpointName);
        assertEquals(expectedInvocationType, ErlangUtils.getInvocationType(outboundEndpoint));
        assertEquals(expectedIsFailIfTimeout, ErlangUtils.isFailIfTimeout(outboundEndpoint));

        final EndpointURI endpointURI = outboundEndpoint.getEndpointURI();
        assertEquals(expectedErlangNodeName, ErlangUtils.getErlangNodeName(endpointURI.getUri()));
        assertEquals(exectedProcessName, ErlangUtils.getProcessName(endpointURI.getUri()));
    }
}
