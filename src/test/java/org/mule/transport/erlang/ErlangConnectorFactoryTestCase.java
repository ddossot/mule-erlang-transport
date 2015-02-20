
package org.mule.transport.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.mule.api.endpoint.InboundEndpoint;
import org.mule.tck.junit4.AbstractMuleContextTestCase;

public class ErlangConnectorFactoryTestCase extends AbstractMuleContextTestCase
{
    @Test
    public void testCreateFromFactory() throws Exception
    {
        final InboundEndpoint endpoint = muleContext.getEndpointFactory()
            .getInboundEndpoint(getEndpointURI());
        assertNotNull(endpoint);
        assertNotNull(endpoint.getConnector());
        assertTrue(endpoint.getConnector() instanceof ErlangConnector);
        assertEquals(getEndpointURI(), endpoint.getEndpointURI().getAddress());
    }

    public String getEndpointURI()
    {
        return "erlang://hostName/nodeName/processName";
    }
}
