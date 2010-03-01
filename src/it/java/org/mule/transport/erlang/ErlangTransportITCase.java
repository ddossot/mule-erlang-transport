package org.mule.transport.erlang;

import org.apache.commons.lang.RandomStringUtils;
import org.mule.api.MuleMessage;
import org.mule.module.client.MuleClient;
import org.mule.tck.FunctionalTestCase;

public class ErlangTransportITCase extends FunctionalTestCase {

    private MuleClient muleClient;

    @Override
    protected void doSetUp() throws Exception {
        super.doSetUp();
        muleClient = new MuleClient(muleContext);
    }

    @Override
    protected String getConfigResources() {
        return "erlang-it-config.xml";
    }

    public void testGenServerCallTest() throws Exception {
        final String testPayload = RandomStringUtils.randomAlphanumeric(20);

        final MuleMessage result = muleClient.send("vm://DispatchTest.IN", testPayload, null);

        final Object payload = result.getPayload();
        assertTrue(payload.getClass().isArray());

        final Object[] responseArray = (Object[]) payload;
        assertEquals("ack", responseArray[0]);
        assertEquals(testPayload, responseArray[1]);
        assertEquals("undefined", responseArray[2]);
    }

}
