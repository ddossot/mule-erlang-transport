/*
 * $Id: TransformersTestCase.vm 10621 2008-01-30 12:15:16Z dirk.olmes $
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang.transformers;

import org.mule.transformer.AbstractTransformerTestCase;
import org.mule.api.transformer.Transformer;


public class ErlangTransformersTestCase extends AbstractTransformerTestCase
{

    /* For general guidelines on writing transports see
       http://mule.mulesource.org/display/MULE/Writing+Transports */

    public Object getTestData()
    {
        // TODO create a test data object that will be passed into the
        // transformer
        throw new UnsupportedOperationException("getResultData");
    }

    public Object getResultData()
    {
        try {
            // TODO Return the result data expected once the getTestData()
            // value has been transformed
            throw new UnsupportedOperationException("getResultData");
        }
        catch (Exception ex) {
            return null;
        }
    }

    public Transformer getTransformer()
    {
        Transformer t = new ErlangMessageToObject();
        // Set the correct return class for this roundtrip test
        t.setReturnClass(this.getResultData().getClass());
        return t;
    }

    public Transformer getRoundTripTransformer()
    {
        Transformer t = new ObjectToErlangMessage();
        // Set the correct return class for this roundtrip test
        t.setReturnClass(this.getTestData().getClass());
        return t;
    }

}
