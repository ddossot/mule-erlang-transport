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

import org.mule.api.endpoint.EndpointURI;
import org.mule.endpoint.MuleEndpointURI;
import org.mule.tck.AbstractMuleTestCase;

public class ErlangEndpointTestCase extends AbstractMuleTestCase {

    public void testValidEndpointUri() throws Exception {
        final EndpointURI endpointUri = new MuleEndpointURI("erlang://hostName/nodeName/processName", muleContext);
        endpointUri.initialise();
        assertEquals("erlang://hostName/nodeName/processName", endpointUri.getAddress());
        assertEquals("erlang", endpointUri.getScheme());
        assertEquals("hostName", endpointUri.getHost());
        assertEquals(0, endpointUri.getParams().size());
    }

    public void testValidEndpointUriWithPort() throws Exception {
        final EndpointURI endpointUri = new MuleEndpointURI("erlang://hostName:30103/nodeName/processName", muleContext);
        endpointUri.initialise();
        assertEquals("erlang://hostName:30103/nodeName/processName", endpointUri.getAddress());
        assertEquals("erlang", endpointUri.getScheme());
        assertEquals(30103, endpointUri.getPort());
        assertEquals("hostName", endpointUri.getHost());
        assertEquals(0, endpointUri.getParams().size());
    }

    public void testValidEndpointUriWithMF() throws Exception {
        final EndpointURI endpointUri = new MuleEndpointURI("erlang://hostName:30103/nodeName/module:function", muleContext);
        endpointUri.initialise();
        assertEquals("erlang://hostName:30103/nodeName/module:function", endpointUri.getAddress());
        assertEquals("erlang", endpointUri.getScheme());
        assertEquals(30103, endpointUri.getPort());
        assertEquals("hostName", endpointUri.getHost());
        assertEquals(0, endpointUri.getParams().size());
    }

    public void testValidEndpointUriWithGS() throws Exception {
        final EndpointURI endpointUri = new MuleEndpointURI("erlang://hostName:30103/nodeName/gen_server:call:serverName",
                muleContext);
        endpointUri.initialise();
        assertEquals("erlang://hostName:30103/nodeName/gen_server:call:serverName", endpointUri.getAddress());
        assertEquals("erlang", endpointUri.getScheme());
        assertEquals(30103, endpointUri.getPort());
        assertEquals("hostName", endpointUri.getHost());
        assertEquals(0, endpointUri.getParams().size());
    }

}
