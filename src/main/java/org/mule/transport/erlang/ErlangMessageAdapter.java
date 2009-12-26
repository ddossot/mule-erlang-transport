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

import org.mule.api.MuleException;
import org.mule.transport.AbstractMessageAdapter;

/**
 * <code>ErlangMessageAdapter</code> TODO document
 */
public class ErlangMessageAdapter extends AbstractMessageAdapter {

    /*
     * For general guidelines on writing transports see
     * http://mule.mulesource.org/display/MULE/Writing+Transports
     */

    /*
     * IMPLEMENTATION NOTE: The MessageAdapter is used to wrap an underlying
     * message. It should store a copy of the underlying message as an instance
     * variable.
     */

    /*
     * IMPLEMENTATION NOTE: If the underlying transport data is available as a
     * stream it is recommended that you pass the stream object into the
     * MessageAdapter as the payload. This will ensure that Mule will use
     * streaming where possible.
     */

    /**
     * 
     */
    private static final long serialVersionUID = -4088199136738488029L;

    public ErlangMessageAdapter(final Object message) throws MuleException {
        /*
         * IMPLEMENTATION NOTE: The constructor should determine that the
         * message is of the correct type or throw an exception i.e.
         * 
         * if (message instanceof byte[]) { this.message = (byte[]) message; }
         * else { throw new MessageTypeNotSupportedException(message,
         * getClass()); }
         */
    }

    public Object getPayload() {
        // TODO return the actual wrapped message
        throw new UnsupportedOperationException("getPayload");
    }

}
