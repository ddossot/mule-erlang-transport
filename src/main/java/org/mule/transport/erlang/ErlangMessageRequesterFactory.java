/*
 * $Id: MessageRequesterFactory.vm 13761 2009-01-20 12:50:38Z dfeist $
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang;

import org.mule.api.MuleException;
import org.mule.api.endpoint.InboundEndpoint;
import org.mule.api.transport.MessageRequester;
import org.mule.transport.AbstractMessageRequesterFactory;

/**
 * <code>ErlangMessageRequester</code> TODO document
 */
public class ErlangMessageRequesterFactory extends AbstractMessageRequesterFactory {

    @Override
    public MessageRequester create(final InboundEndpoint endpoint) throws MuleException {
        return new ErlangMessageRequester(endpoint);
    }

}
