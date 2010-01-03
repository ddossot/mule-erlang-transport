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

import org.mule.api.transport.Connector;
import org.mule.transport.AbstractConnectorTestCase;
import org.mule.transport.erlang.transformers.ErlangConversionUtils;

public class ErlangConnectorTestCase extends AbstractConnectorTestCase {

    @Override
    public Connector createConnector() throws Exception {
        final ErlangConnector c = new ErlangConnector();
        c.setName("Test");
        c.setNodeName("TestErlangNode");
        c.setCookie("TestCookie");
        return c;
    }

    @Override
    public String getTestEndpointURI() {
        return "erlang://hostName:5432/nodeName/processName";
    }

    @Override
    public Object getValidMessage() throws Exception {
        return ErlangConversionUtils.javaToErlang("test");
    }

    public void testProperties() throws Exception {
        final ErlangConnector erlangConnector = (ErlangConnector) getConnector();
        assertEquals("TestErlangNode", erlangConnector.getNodeName());
        assertEquals("TestCookie", erlangConnector.getCookie());
    }

}
