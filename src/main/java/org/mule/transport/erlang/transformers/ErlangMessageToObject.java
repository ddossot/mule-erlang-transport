/*
 * $Id: InboundTransformer.vm 10621 2008-01-30 12:15:16Z dirk.olmes $
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
 * <code>ErlangMessageToObject</code> converts an OtpErlangObject to a Java Object.
 */
public class ErlangMessageToObject extends AbstractMessageAwareTransformer {

    public ErlangMessageToObject() {
        registerSourceType(OtpErlangObject.class);
        setReturnClass(Object.class);
    }

    @Override
    public Object transform(final MuleMessage message, final String outputEncoding) throws TransformerException {
        try {
            return ErlangConversionUtils.erlangToJava((OtpErlangObject) message.getPayload());
        } catch (final IllegalArgumentException iae) {
            throw new TransformerException(this, iae);
        }
    }

}
