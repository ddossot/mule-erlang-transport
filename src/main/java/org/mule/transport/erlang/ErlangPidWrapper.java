package org.mule.transport.erlang;

import com.ericsson.otp.erlang.OtpErlangPid;

public interface ErlangPidWrapper {
    OtpErlangPid getPid();
}
