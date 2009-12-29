package org.mule.transport.erlang.transformers;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * @author <a href="mailto:david@dossot.net">David Dossot</a>
 */
public class ErlangConversionUtilsTest {

    @Test
    public void roundTrips() {
        assertEquals("string", "string", ErlangConversionUtils.erlangToJava(ErlangConversionUtils.javaToErlang("string")));
        assertEquals("char", 'a', ErlangConversionUtils.erlangToJava(ErlangConversionUtils.javaToErlang('a')));
        assertEquals("byte", (byte) 12, ErlangConversionUtils.erlangToJava(ErlangConversionUtils.javaToErlang((byte) 12)));
    }

}
