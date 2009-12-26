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

import org.junit.Ignore;
import org.mule.api.MuleException;
import org.mule.api.transport.MessageAdapter;
import org.mule.transport.AbstractMessageAdapterTestCase;

//FIXME reactivate test
@Ignore
public class ErlangMessageAdapterTestCase extends AbstractMessageAdapterTestCase {

    /*
     * For general guidelines on writing transports see
     * http://mule.mulesource.org/display/MULE/Writing+Transports
     */

    @Override
    public Object getValidMessage() throws Exception {
        // TODO Create a valid message for your transport
        throw new UnsupportedOperationException("getValidMessage");
    }

    @Override
    public MessageAdapter createAdapter(final Object payload) throws MuleException {
        return new ErlangMessageAdapter(payload);
    }
}
