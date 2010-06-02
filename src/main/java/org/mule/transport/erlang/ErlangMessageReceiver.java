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

import org.mule.api.endpoint.InboundEndpoint;
import org.mule.api.lifecycle.CreateException;
import org.mule.api.service.Service;
import org.mule.api.transport.Connector;
import org.mule.transport.AbstractPollingMessageReceiver;
import org.mule.transport.ConnectException;

/**
 * <code>ErlangMessageReceiver</code> TODO document
 */
public class ErlangMessageReceiver extends AbstractPollingMessageReceiver {

    /*
     * For general guidelines on writing transports see http://mule.mulesource.org/display/MULE/Writing+Transports
     */

    public ErlangMessageReceiver(final Connector connector, final Service service, final InboundEndpoint endpoint)
            throws CreateException {
        super(connector, service, endpoint);
    }

    @Override
    public void doConnect() throws ConnectException {
        /*
         * IMPLEMENTATION NOTE: This method should make a connection to the underlying transport i.e. connect to a socket or
         * register a soap service. When there is no connection to be made this method should be used to check that resources
         * are available. For example the FileMessageReceiver checks that the directories it will be using are available and
         * readable. The MessageReceiver should remain in a 'stopped' state even after the doConnect() method is called. This
         * means that a connection has been made but no events will be received until the start() method is called.
         * 
         * Calling start() on the MessageReceiver will call doConnect() if the receiver hasn't connected already.
         */

        /*
         * IMPLEMENTATION NOTE: If you need to spawn any threads such as worker threads for this receiver you can schedule a
         * worker thread with the work manager i.e.
         * 
         * getWorkManager().scheduleWork(worker, WorkManager.INDEFINITE, null, null); Where 'worker' implemments
         * javax.resource.spi.work.Work
         */

        /*
         * IMPLEMENTATION NOTE: When throwing an exception from this method you need to throw an ConnectException that accepts a
         * Message, a cause exception and a reference to this MessageReceiver i.e.
         * 
         * throw new ConnectException(new Message(Messages.FAILED_TO_SCHEDULE_WORK), e, this);
         */

        // TODO the code necessary to connect to the underlying resource
    }

    @Override
    public void doStart() {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Should perform any actions necessary to enable the reciever to start reciving events. This is
         * different to the doConnect() method which actually makes a connection to the transport, but leaves the
         * MessageReceiver in a stopped state. For polling-based MessageReceivers the start() method simply starts the polling
         * thread, for the Axis Message receiver the start method on the SOAPService is called. What action is performed here
         * depends on the transport being used. Most of the time a custom provider doesn't need to override this method.
         */
    }

    @Override
    public void doStop() {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Should perform any actions necessary to stop the reciever from receiving events.
         */
    }

    @Override
    public void doDisconnect() throws ConnectException {
        /*
         * IMPLEMENTATION NOTE: Disconnects and tidies up any rources allocted using the doConnect() method. This method should
         * return the MessageReceiver into a disconnected state so that it can be connected again using the doConnect() method.
         */

        // TODO release any resources here
    }

    @Override
    public void doDispose() {
        // Optional; does not need to be implemented. Delete if not required

        /*
         * IMPLEMENTATION NOTE: Is called when the Conector is being dispoed and should clean up any resources. The doStop() and
         * doDisconnect() methods will be called implicitly when this method is called.
         */
    }

    @Override
    public void poll() throws Exception {
        // add properties: erlang.invocationType(RAW,PID_WRAPPED,GS_CALL,GS_CAST),erlang.ref,erlang.remotePid
        // use workers to do the dispatching
        // FIXME call routeMessage and if a non-null response is received try to send it back to Erlang
    }

}
