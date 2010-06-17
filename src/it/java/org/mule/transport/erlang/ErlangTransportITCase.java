package org.mule.transport.erlang;

import java.net.InetAddress;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
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

    public void testRaw() throws Exception {
        final String testPayload = RandomStringUtils.randomAlphabetic(5) + " " + RandomStringUtils.randomAlphabetic(5);

        muleClient.dispatch("vm://RawTest.IN", testPayload, null);

        while (getFunctionalTestComponent("JmsDrain").getReceivedMessagesCount() < 3) {
            Thread.sleep(500L);
        }

        final List<Object> receivedMessages = Arrays.asList(getFunctionalTestComponent("JmsDrain").getReceivedMessage(1),
                getFunctionalTestComponent("JmsDrain").getReceivedMessage(2),getFunctionalTestComponent("JmsDrain").getReceivedMessage(3));
        final String capitalizedTestPayload = WordUtils.capitalizeFully(testPayload);
        assertTrue(receivedMessages.contains(capitalizedTestPayload));
        assertTrue(receivedMessages.contains(StringUtils.reverse(capitalizedTestPayload)));
        assertTrue(receivedMessages.contains(StringUtils.upperCase(testPayload)));
    }

    public void testPidWrapped() throws Exception {
        final String testPayload = RandomStringUtils.randomAlphanumeric(20);

        final MuleMessage result = muleClient.send("vm://PidWrapped.IN", testPayload, null);

        final Object payload = result.getPayload();
        assertTrue(payload.getClass().isArray());

        final Object[] responseArray = (Object[]) payload;
        assertEquals("raw_ack", responseArray[0]);
        assertEquals(testPayload, responseArray[1]);
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

}
