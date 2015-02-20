
package org.mule.transport.erlang;

import junit.framework.Assert;

import org.junit.Test;
import org.mule.api.transport.Connector;
import org.mule.transport.AbstractConnectorTestCase;
import org.mule.transport.erlang.transformers.ErlangConversionUtils;

public class ErlangConnectorTestCase extends AbstractConnectorTestCase
{

    public ErlangConnectorTestCase()
    {
        super();
        setStartContext(false);
    }

    @Override
    public Connector createConnector() throws Exception
    {
        final ErlangConnector c = new ErlangConnector(muleContext);
        c.setName("Test");
        c.setNodeName("TestErlangNode");
        c.setCookie("TestCookie");
        return c;
    }

    @Override
    public String getTestEndpointURI()
    {
        return "erlang://hostName:30103/nodeName/processName";
    }

    @Override
    public Object getValidMessage() throws Exception
    {
        return ErlangConversionUtils.javaToErlang("test");
    }

    @Test
    public void testProperties() throws Exception
    {
        final ErlangConnector erlangConnector = (ErlangConnector) getConnector();
        Assert.assertEquals("TestErlangNode", erlangConnector.getNodeName());
        Assert.assertEquals("TestCookie", erlangConnector.getCookie());
    }

    @Override
    public void testConnectorLifecycle() throws Exception
    {
        // Deactivated: this pertains to integration testing more than unit
        // testing
    }

    @Override
    public void testConnectorMessageRequesterFactory() throws Exception
    {
        // Deactivated: this connector doesn't support requesting.
    }

}
