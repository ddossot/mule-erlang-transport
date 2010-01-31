package org.mule.transport.erlang;

import com.ericsson.otp.erlang.OtpErlangRef;

public interface ErlangReferenceFactory {
    OtpErlangRef createRef();
}
