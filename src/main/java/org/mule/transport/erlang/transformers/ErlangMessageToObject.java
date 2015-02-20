
package org.mule.transport.erlang.transformers;

import org.mule.api.MuleMessage;
import org.mule.api.transformer.TransformerException;
import org.mule.transformer.AbstractMessageTransformer;
import org.mule.transformer.types.SimpleDataType;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * <code>ErlangMessageToObject</code> converts an OtpErlangObject to a Java Object.
 */
public class ErlangMessageToObject extends AbstractMessageTransformer
{
    public ErlangMessageToObject()
    {
        registerSourceType(new SimpleDataType<OtpErlangObject>(OtpErlangObject.class));
        setReturnDataType(new SimpleDataType<Object>(Object.class));
    }

    @Override
    public Object transformMessage(final MuleMessage message, final String outputEncoding)
        throws TransformerException
    {
        try
        {
            return ErlangConversionUtils.erlangToJava((OtpErlangObject) message.getPayload());
        }
        catch (final IllegalArgumentException iae)
        {
            throw new TransformerException(this, iae);
        }
    }
}
