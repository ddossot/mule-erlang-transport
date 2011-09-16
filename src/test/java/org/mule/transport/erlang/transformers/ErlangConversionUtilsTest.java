package org.mule.transport.erlang.transformers;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.Arrays;

import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * @author <a href="mailto:david@dossot.net">David Dossot</a>
 */
public class ErlangConversionUtilsTest {

    @Test
    public void roundTrips() {
        final Object[] values = new Object[] { "string", 'a', Byte.MAX_VALUE, Short.MAX_VALUE, Integer.MAX_VALUE, Long.MAX_VALUE,
                BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.TEN), Float.MAX_VALUE, Double.MAX_VALUE, Boolean.TRUE,
                Arrays.asList(new Object[] { 1, "a", Math.PI }) };

        for (int i = 0; i < values.length; i++) {
            assertEquals(values[i].getClass().getName(), values[i],
                    ErlangConversionUtils.erlangToJava(ErlangConversionUtils.javaToErlang(values[i])));
        }
    }

    @Test
    public void byteArrayRoundTrip() {
        final byte[] source = "hello".getBytes();

        assertEquals("hello", new String((byte[]) ErlangConversionUtils.erlangToJava(ErlangConversionUtils.javaToErlang(source))));
    }

    @Test
    public void miscArrayRoundTrip() {
        final Object[] source = new Object[] { 1, "a", Math.PI };

        assertEquals(Arrays.asList(source),
                Arrays.asList((Object[]) ErlangConversionUtils.erlangToJava(ErlangConversionUtils.javaToErlang(source))));
    }

    @Test
    public void erlangToJavaOnly() {
        assertEquals("atom", "atom", ErlangConversionUtils.erlangToJava(new OtpErlangAtom("atom")));
        assertEquals("pid", new OtpErlangPid("test", 1, 2, 3), ErlangConversionUtils.erlangToJava(new OtpErlangPid("test", 1, 2, 3)));
    }

    @Test
    public void javaToErlangIdempotency() {
        final OtpErlangString oeString = new OtpErlangString("test");
        assertEquals("otp object unchanged", oeString, ErlangConversionUtils.javaToErlang(oeString));
    }
}
