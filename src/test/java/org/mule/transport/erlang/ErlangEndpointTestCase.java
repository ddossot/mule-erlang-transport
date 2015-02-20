
package org.mule.transport.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.mule.api.endpoint.EndpointURI;
import org.mule.endpoint.MuleEndpointURI;
import org.mule.tck.junit4.AbstractMuleContextTestCase;

public class ErlangEndpointTestCase extends AbstractMuleContextTestCase
{
    @Test
    public void testValidEndpointUriWithHostname() throws Exception
    {
        final EndpointURI endpointUri = new MuleEndpointURI("erlang://nodeName@hostName/processName",
            muleContext);
        endpointUri.initialise();
        assertEquals("erlang://hostName/processName", endpointUri.getAddress());
        assertEquals("erlang", endpointUri.getScheme());
        assertEquals("hostName", endpointUri.getHost());
        assertEquals("nodeName", endpointUri.getUser());
        assertEquals("/processName", endpointUri.getPath());
    }

    @Test
    public void testValidLocalEndpointUri() throws Exception
    {
        final EndpointURI endpointUri = new MuleEndpointURI("erlang://nodeName/processName", muleContext);
        endpointUri.initialise();
        assertEquals("erlang://nodeName/processName", endpointUri.getAddress());
        assertEquals("erlang", endpointUri.getScheme());
        assertEquals("nodeName", endpointUri.getHost());
        assertNull(endpointUri.getUser());
        assertEquals("/processName", endpointUri.getPath());
    }
}
