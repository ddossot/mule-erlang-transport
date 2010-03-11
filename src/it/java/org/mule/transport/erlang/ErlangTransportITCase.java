package org.mule.transport.erlang;

import java.net.InetAddress;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang.math.RandomUtils;
import org.mule.api.MuleException;
import org.mule.api.MuleMessage;
import org.mule.module.client.MuleClient;
import org.mule.tck.FunctionalTestCase;
import org.mule.transport.NullPayload;

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

    public void testRawAndPidWrapped() throws Exception {
        final Object expectedState = doRaw();

        // this hacky sleep lives time for raw message to hit erlang server
        // TODO replace this with a loop back when inbound will be supported
        Thread.sleep(1000L);

        doPidWrapped(expectedState);
    }

    private Object doRaw() throws Exception {
        final Long testPayload = RandomUtils.nextLong();

        muleClient.dispatch("vm://RawTest.IN", testPayload, null);
        return testPayload;
    }

    private void doPidWrapped(final Object expectedState) throws Exception {
        final String testPayload = RandomStringUtils.randomAlphanumeric(20);

        final MuleMessage result = muleClient.send("vm://PidWrapped.IN", testPayload, null);

        final Object payload = result.getPayload();
        assertTrue(payload.getClass().isArray());

        final Object[] responseArray = (Object[]) payload;
        assertEquals("raw_ack", responseArray[0]);
        assertEquals(testPayload, responseArray[1]);
        assertEquals(expectedState, responseArray[2]);
    }

    public void testGenServerCastAndCall() throws Exception {
        final Object expectedState = doGenServerCast();
        doGenServerCall(expectedState);
    }

    private Object doGenServerCast() throws Exception {
        final Long testPayload = RandomUtils.nextLong();

        final MuleMessage result = muleClient.send("vm://GenServerCastTest.IN", testPayload, null);
        assertEquals(NullPayload.getInstance(), result.getPayload());
        return testPayload;
    }

    private void doGenServerCall(final Object expectedState) throws MuleException {
        final String testPayload = RandomStringUtils.randomAlphanumeric(20);

        final MuleMessage result = muleClient.send("vm://GenServerCallTest.IN", testPayload, null);

        final Object payload = result.getPayload();
        assertTrue(payload.getClass().isArray());

        final Object[] responseArray = (Object[]) payload;
        assertEquals("gs_ack", responseArray[0]);
        assertEquals(testPayload, responseArray[1]);
        assertEquals(expectedState, responseArray[2]);
    }

    // TODO test dynamic dispatch (property in msg)

}
