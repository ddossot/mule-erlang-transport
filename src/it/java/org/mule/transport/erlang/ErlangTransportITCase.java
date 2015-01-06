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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.apache.commons.lang.math.RandomUtils;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mule.api.MuleException;
import org.mule.api.MuleMessage;
import org.mule.module.client.MuleClient;
import org.mule.tck.junit4.FunctionalTestCase;
import org.mule.transport.NullPayload;

public class ErlangTransportITCase extends FunctionalTestCase
{
    private MuleClient muleClient;

    @BeforeClass
    public static void initializeSystem() throws UnknownHostException
    {
        System.setProperty("host.name", InetAddress.getLocalHost().getHostName());
    }

    public ErlangTransportITCase()
    {
        setDisposeContextPerClass(true);
    }

    @Override
    protected void doSetUp() throws Exception
    {
        super.doSetUp();
        muleClient = new MuleClient(muleContext);
    }

    @Override
    protected String getConfigResources()
    {
        return "erlang-it-config.xml";
    }

    @Test
    public void testRaw() throws Exception
    {
        final String testPayload = RandomStringUtils.randomAlphabetic(5) + " "
                                   + RandomStringUtils.randomAlphabetic(5);

        muleClient.dispatch("vm://RawTest.IN", testPayload, null);

        while (getFunctionalTestComponent("JmsDrain").getReceivedMessagesCount() < 3)
        {
            Thread.sleep(500L);
        }

        final List<Object> receivedMessages = Arrays.asList(
            getFunctionalTestComponent("JmsDrain").getReceivedMessage(1),
            getFunctionalTestComponent("JmsDrain").getReceivedMessage(2),
            getFunctionalTestComponent("JmsDrain").getReceivedMessage(3));
        final String capitalizedTestPayload = WordUtils.capitalizeFully(testPayload);
        assertTrue(receivedMessages.contains(capitalizedTestPayload));
        assertTrue(receivedMessages.contains(StringUtils.reverse(capitalizedTestPayload)));
        assertTrue(receivedMessages.contains(StringUtils.upperCase(testPayload)));
    }

    @Test
    public void testPidWrapped() throws Exception
    {
        final String testPayload = RandomStringUtils.randomAlphanumeric(20);

        final MuleMessage result = muleClient.send("vm://PidWrapped.IN", testPayload, null);

        final Object payload = result.getPayload();
        assertTrue(payload.getClass().isArray());

        final Object[] responseArray = (Object[]) payload;
        assertEquals("raw_ack", responseArray[0]);
        assertEquals(testPayload, responseArray[1]);
    }

    @Test
    public void testRPC() throws Exception
    {
        final String testPayload = RandomStringUtils.randomAlphabetic(20);
        final MuleMessage result = muleClient.send("vm://RpcCallTest.IN", testPayload, null);
        assertEquals(testPayload.toUpperCase(), result.getPayload());
    }

    @Test
    public void testRPCWithDynamicModuleFunction() throws Exception
    {
        final Map<String, Object> props = new HashMap<String, Object>();
        props.put("erlang.moduleFunction", "erlang:node");
        final MuleMessage result = muleClient.send("vm://RpcCallTest.IN", new ArrayList<Object>(), props);
        assertEquals("mule_test_server_node@" + InetAddress.getLocalHost().getHostName(), result.getPayload());
    }

    @Test
    public void testGenServerCastAndCall() throws Exception
    {
        final Object expectedState = doGenServerCast();
        doGenServerCall(expectedState);
    }

    private Object doGenServerCast() throws Exception
    {
        final Long testPayload = RandomUtils.nextLong();

        final MuleMessage result = muleClient.send("vm://GenServerCastTest.IN", testPayload, null);
        assertEquals(NullPayload.getInstance(), result.getPayload());
        return testPayload;
    }

    private void doGenServerCall(final Object expectedState) throws MuleException
    {
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
