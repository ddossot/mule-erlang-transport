
package org.mule.transport.erlang;

import org.mule.api.MuleException;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.api.transport.MessageDispatcher;
import org.mule.transport.AbstractMessageDispatcherFactory;

/**
 * <code>ErlangMessageDispatcherFactory</code> Todo document
 */

public class ErlangMessageDispatcherFactory extends AbstractMessageDispatcherFactory
{

    /*
     * For general guidelines on writing transports see
     * http://mule.mulesource.org/display/MULE/Writing+Transports
     */

    @Override
    public MessageDispatcher create(final OutboundEndpoint endpoint) throws MuleException
    {
        return new ErlangMessageDispatcher(endpoint);
    }

}
