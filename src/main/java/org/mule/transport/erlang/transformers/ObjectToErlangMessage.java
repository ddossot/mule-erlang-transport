
package org.mule.transport.erlang.transformers;

import org.mule.api.MuleMessage;
import org.mule.api.transformer.TransformerException;
import org.mule.transformer.AbstractMessageTransformer;
import org.mule.transformer.types.SimpleDataType;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * <code>ObjectToErlangMessage</code> converts a Java Object to an OtpErlangObject.
 */
public class ObjectToErlangMessage extends AbstractMessageTransformer
{
    public ObjectToErlangMessage()
    {
        registerSourceType(new SimpleDataType<Object>(Object.class));
        setReturnDataType(new SimpleDataType<OtpErlangObject>(OtpErlangObject.class));
    }

    @Override
    public Object transformMessage(final MuleMessage message, final String outputEncoding)
        throws TransformerException
    {
        try
        {
            return ErlangConversionUtils.javaToErlang(message.getPayload());
        }
        catch (final IllegalArgumentException iae)
        {
            throw new TransformerException(this, iae);
        }
    }
}
