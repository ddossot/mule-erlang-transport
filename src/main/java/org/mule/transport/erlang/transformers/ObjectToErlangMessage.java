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

import org.mule.transformer.AbstractMessageAwareTransformer;
import org.mule.api.transformer.TransformerException;
import org.mule.api.MuleMessage;

/**
 * <code>ObjectToErlangMessage</code> TODO Document
 */
public class ObjectToErlangMessage extends AbstractMessageAwareTransformer
{

    /* For general guidelines on writing transports see
       http://mule.mulesource.org/display/MULE/Writing+Transports */

    public ObjectToErlangMessage()
    {
        /* IMPLEMENTATION NOTE: Here you can set default types that the
           transformer will accept at runtime.  Mule will then validate the
           transformer at runtime. You can register one or more source types.

             registerSourceType(XXX.class.getName());
        */

        /* IMPLEMENTATION NOTE: It's good practice to set the expected return
           type for this transformer here This helps Mule validate event flows
           and Transformer chains

             setReturnClass(YYY.class);
        */
    }

    public Object transform(MuleMessage message, String encoding) throws TransformerException
    {
        // TODO Transformer the message here. See comments in {@link AbstractMessageAwareTransformer}

        // Make sure you return a transfromed object that matches the
        // returnClass type

        throw new UnsupportedOperationException("transform");
    }

}
