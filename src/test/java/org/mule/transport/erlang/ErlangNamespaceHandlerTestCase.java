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

    public void testMinimalConnectorConfiguration() throws Exception {
        testConnectorConfiguration("erlangConnector1", "muleErlang1", null, Integer.valueOf(0));
    }

    public void testIntermediateConnectorConfiguration() throws Exception {
        testConnectorConfiguration("erlangConnector2", "muleErlang2", "xyz2", Integer.valueOf(0));
    }

    public void testFullConnectorConfiguration() throws Exception {
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
}
