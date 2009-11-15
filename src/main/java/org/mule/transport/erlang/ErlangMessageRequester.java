/*
 * $Id: MessageRequester.vm 13767 2009-01-20 14:16:54Z dfeist $
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang;

import org.mule.api.MuleMessage;
import org.mule.api.endpoint.InboundEndpoint;
import org.mule.transport.AbstractMessageRequester;

/**
 * <code>ErlangMessageRequester</code> TODO document
 */
public class ErlangMessageRequester extends AbstractMessageRequester
{

    /* For general guidelines on writing transports see
       http://mule.mulesource.org/display/MULE/Writing+Transports

             */

    public ErlangMessageRequester(InboundEndpoint endpoint)
    {
        super(endpoint);
    }

    protected MuleMessage doRequest(long timeout) throws Exception
    {
        /* IMPLEMENTATION NOTE: This method should make a request to the underlying
           transport.  */

        /* IMPLEMENTATION NOTE: If you need to spawn any threads such as
           worker threads for this receiver you can schedule a worker thread
           with the work manager i.e.

             getWorkManager().scheduleWork(worker, WorkManager.INDEFINITE, null, null);
           Where 'worker' implemments javax.resource.spi.work.Work */

        /* IMPLEMENTATION NOTE: When throwing an exception from this method
           you need to throw an ConnectException that accepts a Message, a
           cause exception and a reference to this MessageReceiver i.e.

             throw new ConnectException(new Message(Messages.FAILED_TO_SCHEDULE_WORK), e, this);
        */

        // TODO the code necessay to Connect to the underlying resource
        return null;
    }

    public void doConnect() throws Exception
    {
        /* IMPLEMENTATION NOTE: This method should make a connection to the underlying
           transport i.e. connect to a socket or register a soap service. When
           there is no connection to be made this method should be used to
           check that resources are available. For example the
           FileMessageReceiver checks that the directories it will be using
           are available and readable. The MessageReceiver should remain in a
           'stopped' state even after the doConnect() method is called. This
           means that a connection has been made but no events will be
           received until the start() method is called.

           Calling start() on the MessageReceiver will call doConnect() if the receiver
           hasn't connected already. */

        /* IMPLEMENTATION NOTE: If you need to spawn any threads such as
           worker threads for this receiver you can schedule a worker thread
           with the work manager i.e.

             getWorkManager().scheduleWork(worker, WorkManager.INDEFINITE, null, null);
           Where 'worker' implemments javax.resource.spi.work.Work */

        /* IMPLEMENTATION NOTE: When throwing an exception from this method
           you need to throw an ConnectException that accepts a Message, a
           cause exception and a reference to this MessageReceiver i.e.

             throw new ConnectException(new Message(Messages.FAILED_TO_SCHEDULE_WORK), e, this);
        */

        // TODO the code necessay to Connect to the underlying resource
    }

    public void doDisconnect() throws Exception
    {
        /* IMPLEMENTATION NOTE: Disconnects and tidies up any rources allocted
           using the doConnect() method. This method should return the
           MessageRequester into a disconnected state so that it can be
           connected again using the doConnect() method. */

        // TODO release any resources here
    }

    public void doDispose()
    {
        /* IMPLEMENTATION NOTE: Is called when the Requester is being dispoed
           and should clean up any resources. The doStop() and doDisconnect()
           methods will be called implicitly when this method is called. */
    }

}

