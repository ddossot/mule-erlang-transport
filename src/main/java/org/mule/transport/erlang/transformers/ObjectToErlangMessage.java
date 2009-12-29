/*
 * $Id: OutboundTransformer.vm 10621 2008-01-30 12:15:16Z dirk.olmes $
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang.transformers;

import org.mule.api.MuleMessage;
import org.mule.api.transformer.TransformerException;
import org.mule.transformer.AbstractMessageAwareTransformer;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * <code>ObjectToErlangMessage</code> converts a Java Object to an
 * OtpErlangObject.
 */
public class ObjectToErlangMessage extends AbstractMessageAwareTransformer {

    public ObjectToErlangMessage() {
        registerSourceType(Object.class);
        setReturnClass(OtpErlangObject.class);
    }

    @Override
    public Object transform(final MuleMessage message, final String encoding) throws TransformerException {
        try {
            return ErlangConversionUtils.javaToErlang(message.getPayload());
        } catch (final IllegalArgumentException iae) {
            throw new TransformerException(this, iae);
        }
    }

}
