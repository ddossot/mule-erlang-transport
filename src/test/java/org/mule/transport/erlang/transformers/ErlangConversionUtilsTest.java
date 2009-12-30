package org.mule.transport.erlang.transformers;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;

import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;

import edu.emory.mathcs.backport.java.util.Arrays;

/**
 * @author <a href="mailto:david@dossot.net">David Dossot</a>
 */
public class ErlangConversionUtilsTest {

    @Test
    public void roundTrips() {
        final Object[] values = new Object[] { "string", 'a', Byte.MAX_VALUE, Short.MAX_VALUE, Integer.MAX_VALUE,
                Long.MAX_VALUE, BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.TEN), Float.MAX_VALUE, Double.MAX_VALUE,
                Boolean.TRUE, Arrays.asList(new Object[] { 1, "a", Math.PI }) };

        for (int i = 0; i < values.length; i++) {
            assertEquals(values[i].getClass().getName(), values[i], ErlangConversionUtils.erlangToJava(ErlangConversionUtils
                    .javaToErlang(values[i])));
        }
    }

    @Test
    public void erlangToJavaOnly() {
        assertEquals("atom", "atom", ErlangConversionUtils.erlangToJava(new OtpErlangAtom("atom")));
    }
}
