/*
 * $Id: MessageReceiver.vm 11079 2008-02-27 15:52:01Z tcarlson $
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang;

import javax.resource.spi.work.Work;
import javax.resource.spi.work.WorkException;

import org.mule.api.MuleEvent;
import org.mule.api.MuleException;
import org.mule.api.MuleMessage;
import org.mule.api.MuleRuntimeException;
import org.mule.api.construct.FlowConstruct;
import org.mule.api.endpoint.InboundEndpoint;
import org.mule.api.lifecycle.CreateException;
import org.mule.api.lifecycle.StartException;
import org.mule.api.transport.Connector;
import org.mule.transport.AbstractMessageReceiver;
import org.mule.transport.ConnectException;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * The <code>ErlangMessageReceiver</code> creates a single worker listening to an OtpMbox (which can
 * only be consumed by a single thread) but delegates the routing of incoming messages to workers.
 */
public class ErlangMessageReceiver extends AbstractMessageReceiver
{

    private static final long MBOX_RECEIVE_TIMEOUT = 1000L;

    private final ErlangConnector connector;
    private OtpMbox otpMbox;
    private ErlangMessageReceiverWorker erlangMessageReceiverWorker;

    public ErlangMessageReceiver(final Connector connector,
                                 final FlowConstruct flowConstruct,
                                 final InboundEndpoint endpoint) throws CreateException
    {
        super(connector, flowConstruct, endpoint);
        this.connector = (ErlangConnector) connector;
    }

    @Override
    public void doConnect() throws ConnectException
    {
        final String processName = ErlangUtils.getProcessName(endpoint.getEndpointURI().getUri());
        otpMbox = connector.createMailBox();
        if (!otpMbox.registerName(processName))
        {
            throw new ConnectException(ErlangMessages.mailboxNameRegistrationFailed(processName), this);
        }
    }

    @Override
    protected void doStart() throws MuleException
    {
        erlangMessageReceiverWorker = new ErlangMessageReceiverWorker();

        try
        {
            getWorkManager().scheduleWork(erlangMessageReceiverWorker);
        }
        catch (final WorkException we)
        {
            throw new StartException(we, this);
        }
    }

    @Override
    protected void doStop() throws MuleException
    {
        erlangMessageReceiverWorker.release();
    }

    @Override
    public void doDisconnect() throws ConnectException
    {
        otpMbox.registerName(null);
        otpMbox.close();
    }

    @Override
    public void doDispose()
    {
        otpMbox = null;
    }

    public void respondToErlangProcess(final OtpErlangPid pid, final OtpErlangObject message)
    {
        otpMbox.send(pid, message);
    }

    private final class ErlangMessageReceiverWorker implements Work
    {
        private volatile boolean running = true;

        @Override
        public void run()
        {
            while (running)
            {
                try
                {
                    final OtpErlangObject receivedErlangObject = otpMbox.receive(MBOX_RECEIVE_TIMEOUT);

                    if (receivedErlangObject != null)
                    {
                        getWorkManager().scheduleWork(new ErlangMessageRouterWorker(receivedErlangObject));
                    }
                }
                catch (final WorkException we)
                {
                    throwAsRuntimeException(we);
                }
                catch (final OtpErlangException oee)
                {
                    throwAsRuntimeException(oee);
                }
            }
        }

        @Override
        public void release()
        {
            running = false;
        }
    }

    private final class ErlangMessageRouterWorker implements Work
    {
        private final OtpErlangObject receivedErlangObject;

        public ErlangMessageRouterWorker(final OtpErlangObject receivedErlangObject)
        {
            this.receivedErlangObject = receivedErlangObject;
        }

        @Override
        public void run()
        {
            try
            {
                final ErlangInboundInvocation invocation = new ErlangInboundInvocation(
                    ErlangMessageReceiver.this, receivedErlangObject);

                // TODO consider adding inbound properties:
                // erlang.invocationType,erlang.ref,erlang.remotePid
                final MuleMessage routedMessage = createMuleMessage(invocation.getPayloadToRoute());
                final MuleEvent result = routeMessage(routedMessage);
                invocation.respondIfNecessaryAndPossible(result);

                if (logger.isDebugEnabled())
                {
                    logger.debug("Invocation: " + invocation + " tried to respond: " + result);
                }
            }
            catch (final MuleException me)
            {
                throwAsRuntimeException(me);
            }
        }

        @Override
        public void release()
        {
            // NOOP
        }
    }

    private static void throwAsRuntimeException(final Exception e)
    {
        throw new MuleRuntimeException(e);
    }

}
