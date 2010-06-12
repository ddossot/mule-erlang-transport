package org.mule.transport.erlang;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.Validate;
import org.mule.api.endpoint.EndpointURI;
import org.mule.api.endpoint.ImmutableEndpoint;
import org.mule.transport.erlang.i18n.ErlangMessages;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class ErlangUtils {
    public static final OtpErlangAtom GEN_CAST_SIGNATURE = new OtpErlangAtom("$gen_cast");
    public static final OtpErlangAtom GEN_CALL_SIGNATURE = new OtpErlangAtom("$gen_call");

    private ErlangUtils() {
        throw new UnsupportedOperationException("do not instantiate");
    }

    public static String getErlangNodeName(final EndpointURI euri) {
        final String user = euri.getUser();
        final String host = euri.getHost();

        if (user == null) {
            return host;
        }

        return user + "@" + host;
    }

    public static String getProcessName(final EndpointURI euri) {
        return StringUtils.stripStart(euri.getPath(), "/");
    }

    public static ErlangOutboundInvocation.InvocationType getInvocationType(final ImmutableEndpoint ie) {
        final Object invocationTypeProperty = ie.getProperty("invocationType");
        Validate.notNull(invocationTypeProperty, ErlangMessages.missingEndpointProperty("invocationType").getMessage());
        return ErlangOutboundInvocation.InvocationType.valueOf(invocationTypeProperty.toString());
    }

    public static boolean isFailIfTimeout(final ImmutableEndpoint ie) {
        final Object failIfTimeOut = ie.getProperty("failIfTimeout");
        return failIfTimeOut == null ? false : Boolean.parseBoolean(failIfTimeOut.toString());
    }

    public static OtpErlangTuple makeTuple(final OtpErlangObject... otpErlangObjects) {
        return new OtpErlangTuple(otpErlangObjects);
    }

}
