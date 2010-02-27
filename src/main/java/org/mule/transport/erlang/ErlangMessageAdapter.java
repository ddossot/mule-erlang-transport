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

public class ErlangMessageAdapter extends AbstractMessageAdapter {

    private final Object payload;

    private static final long serialVersionUID = -4088199136738488029L;

    public ErlangMessageAdapter(final Object payload) throws MuleException {
        this.payload = payload;
    }

    public ErlangMessageAdapter(final ErlangMessageAdapter template) throws MuleException {
        this.payload = template.getPayload();
    }

    public Object getPayload() {
        return payload;
    }

}
