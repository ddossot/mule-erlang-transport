
package org.mule.transport.erlang;

import org.apache.commons.lang.Validate;
import org.mule.api.MuleEvent;
import org.mule.api.MuleMessage;
import org.mule.api.endpoint.EndpointURI;
import org.mule.api.endpoint.OutboundEndpoint;
import org.mule.api.lifecycle.InitialisationException;
import org.mule.transport.AbstractMessageDispatcher;
import org.mule.transport.ConnectException;
import org.mule.transport.erlang.ErlangOutboundInvocation.InvocationType;
import org.mule.transport.erlang.i18n.ErlangMessages;
import org.mule.transport.erlang.transformers.ErlangConversionUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public class ErlangMessageDispatcher extends AbstractMessageDispatcher
{

    private final ErlangConnector connector;
    private final InvocationType invocationType;
    private final boolean failIfTimeout;
    private final String targetNodeName; // contains node or node@host

    private OtpMbox otpMbox;

    public ErlangMessageDispatcher(final OutboundEndpoint endpoint)
    {
        super(endpoint);
        connector = (ErlangConnector) endpoint.getConnector();

        invocationType = ErlangUtils.getInvocationType(endpoint);
        failIfTimeout = ErlangUtils.isFailIfTimeout(endpoint);

        final EndpointURI endpointURI = endpoint.getEndpointURI();
        targetNodeName = ErlangUtils.getErlangNodeName(endpointURI.getUri());
    }

    @Override
    protected void doInitialise() throws InitialisationException
    {
        Validate.notEmpty(targetNodeName, ErlangMessages.missingEndpointProperty("targetNodeName")
            .getMessage());
        super.doInitialise();
    }

    @Override
    public void doConnect() throws Exception
    {
        otpMbox = connector.createMailBox();

        if (!otpMbox.ping(targetNodeName, 1000L))
        {
            throw new ConnectException(ErlangMessages.nodeUnreachable(targetNodeName), this);
        }
    }

    @Override
    public void doDisconnect() throws Exception
    {
        otpMbox.close();
    }

    @Override
    public void doDispose()
    {
        otpMbox = null;
    }

    @Override
    public void doDispatch(final MuleEvent event) throws Exception
    {
        doInvokeRemote(event);
    }

    @Override
    public MuleMessage doSend(final MuleEvent event) throws Exception
    {
        final OtpErlangObject result = doInvokeRemote(event);
        return createMuleMessage(ErlangConversionUtils.erlangToJava(result));
    }

    private OtpErlangObject doInvokeRemote(final MuleEvent event) throws Exception
    {
        return new ErlangOutboundInvocation((OutboundEndpoint) endpoint, event, otpMbox, invocationType,
            failIfTimeout).call();
    }

    public OtpErlangPid getPid()
    {
        return otpMbox.self();
    }
}
