
package org.mule.transport.erlang.transformers;

import org.mule.api.transformer.Transformer;
import org.mule.transformer.AbstractTransformerTestCase;

import com.ericsson.otp.erlang.OtpErlangString;

public class ErlangTransformersTestCase extends AbstractTransformerTestCase
{

    private final static String TEST_DATA = "foo";

    @Override
    public Object getTestData()
    {
        return TEST_DATA;
    }

    @Override
    public Object getResultData()
    {
        return new OtpErlangString(TEST_DATA);
    }

    @Override
    public Transformer getTransformer()
    {
        final ObjectToErlangMessage t = new ObjectToErlangMessage();
        t.setMuleContext(muleContext);
        return t;
    }

    @Override
    public Transformer getRoundTripTransformer()
    {
        final ErlangMessageToObject t = new ErlangMessageToObject();
        t.setMuleContext(muleContext);
        return t;
    }
}
