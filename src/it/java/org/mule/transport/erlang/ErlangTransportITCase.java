package org.mule.transport.erlang;

import java.net.InetAddress;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang.math.RandomUtils;
import org.mule.api.MuleException;
import org.mule.api.MuleMessage;
import org.mule.module.client.MuleClient;
import org.mule.tck.FunctionalTestCase;

public class ErlangTransportITCase extends FunctionalTestCase {

    private MuleClient muleClient;

    @Override
    protected void suitePreSetUp() throws Exception {
        super.suitePreSetUp();
        System.setProperty("host.name", InetAddress.getLocalHost().getHostName());
    }

    @Override
    protected void doSetUp() throws Exception {
        super.doSetUp();
        muleClient = new MuleClient(muleContext);
    }

    @Override
    protected String getConfigResources() {
        return "erlang-it-config.xml";
    }

    public void testGenServerCastAndCallTest() throws Exception {
        final Object expectedState = doGenServerCast();
        doGenServerCall(expectedState);
    }

    private Object doGenServerCast() throws Exception {
        final Long testPayload = RandomUtils.nextLong();

        final MuleMessage result = muleClient.send("vm://GenServerCastTest.IN", testPayload, null);
        assertEquals("ok", result.getPayloadAsString());
        return testPayload;
    }

    private void doGenServerCall(final Object expectedState) throws MuleException {
        final String testPayload = RandomStringUtils.randomAlphanumeric(20);

        final MuleMessage result = muleClient.send("vm://GenServerCallTest.IN", testPayload, null);

        final Object payload = result.getPayload();
        assertTrue(payload.getClass().isArray());

        final Object[] responseArray = (Object[]) payload;
        assertEquals("ack", responseArray[0]);
        assertEquals(testPayload, responseArray[1]);
        assertEquals(expectedState, responseArray[2]);
    }

    // TODO test dynamic dispatch (property in msg)

}
