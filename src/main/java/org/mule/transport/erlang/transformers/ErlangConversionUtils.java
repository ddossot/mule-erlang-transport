package org.mule.transport.erlang.transformers;

import java.lang.reflect.Array;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.Validate;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBoolean;
import com.ericsson.otp.erlang.OtpErlangByte;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangShort;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Supporting methods for Java to Erlang conversion.
 * 
 * @author <a href="mailto:david@dossot.net">David Dossot</a>
 */
public abstract class ErlangConversionUtils {

    // FIXME add support for Map <-> PropList, stream/byte[] <-> Binary conversions

    private ErlangConversionUtils() {
        throw new UnsupportedOperationException("do not instantiate");
    }

    /**
     * This method comes from Erlide, the awesome Eclipse plug-in for Erlang development.
     * 
     * Copyright (c) 2008 Vlad Dumitrescu and others. All rights reserved. This program and the accompanying materials are made
     * available under the terms of the Eclipse Public License v1.0 which accompanies this distribution, and is available at
     * http://www.eclipse.org/legal/epl-v10.html
     */
    public static OtpErlangObject javaToErlang(final Object obj) throws IllegalArgumentException {
        Validate.notNull(obj, "Can't transform null objects");

        if (obj instanceof OtpErlangObject) {
            return (OtpErlangObject) obj;
        }
        if (obj instanceof String) {
            return new OtpErlangString((String) obj);
        }
        if (obj instanceof Character) {
            return new OtpErlangChar((Character) obj);
        }
        if (obj instanceof Byte) {
            return new OtpErlangByte((Byte) obj);
        }
        if (obj instanceof Short) {
            return new OtpErlangShort((Short) obj);
        }
        if (obj instanceof Integer) {
            return new OtpErlangInt((Integer) obj);
        }
        if (obj instanceof Long) {
            return new OtpErlangLong((Long) obj);
        }
        if (obj instanceof BigInteger) {
            return new OtpErlangLong((BigInteger) obj);
        }
        if (obj instanceof Float) {
            return new OtpErlangFloat((Float) obj);
        }
        if (obj instanceof Double) {
            return new OtpErlangDouble((Double) obj);
        }
        if (obj instanceof Boolean) {
            return new OtpErlangBoolean((Boolean) obj);
        }
        if (obj.getClass().isArray()) {
            final int length = Array.getLength(obj);
            final OtpErlangObject[] erlangObjects = new OtpErlangObject[length];
            for (int i = 0; i < length; i++) {
                erlangObjects[i] = javaToErlang(Array.get(obj, i));
            }
            return new OtpErlangTuple(erlangObjects);
        }
        if (obj instanceof Collection<?>) {
            final int size = ((Collection<?>) obj).size();
            final OtpErlangObject[] erlangObjects = new OtpErlangObject[size];
            int j = 0;
            for (final Iterator<?> i = ((Collection<?>) obj).iterator(); i.hasNext();) {
                erlangObjects[j++] = javaToErlang(i.next());
            }
            return new OtpErlangList(erlangObjects);
        }

        throw new IllegalArgumentException("Unsupported object of type: " + obj.getClass().getName());
    }

    public static Object erlangToJava(final OtpErlangObject erl) throws IllegalArgumentException {
        Validate.notNull(erl, "Can't transform null objects");

        try {
            if (erl instanceof OtpErlangString) {
                return ((OtpErlangString) erl).stringValue();
            }
            if (erl instanceof OtpErlangChar) {
                return ((OtpErlangChar) erl).charValue();
            }
            if (erl instanceof OtpErlangByte) {
                return ((OtpErlangByte) erl).byteValue();
            }
            if (erl instanceof OtpErlangShort) {
                return ((OtpErlangShort) erl).shortValue();
            }
            if (erl instanceof OtpErlangInt) {
                return ((OtpErlangInt) erl).intValue();
            }
            if (erl instanceof OtpErlangLong) {
                final OtpErlangLong otpErlangLong = (OtpErlangLong) erl;
                if (otpErlangLong.isLong()) {
                    return otpErlangLong.longValue();
                }
                return otpErlangLong.bigIntegerValue();
            }
            if (erl instanceof OtpErlangFloat) {
                return ((OtpErlangFloat) erl).floatValue();
            }
            if (erl instanceof OtpErlangDouble) {
                return ((OtpErlangDouble) erl).doubleValue();
            }
            if (erl instanceof OtpErlangBoolean) {
                return ((OtpErlangBoolean) erl).booleanValue();
            }
            if (erl instanceof OtpErlangTuple) {
                return erlangObjectsToJava(((OtpErlangTuple) erl).elements()).toArray();
            }
            if (erl instanceof OtpErlangAtom) {
                return ((OtpErlangAtom) erl).toString();
            }
            if (erl instanceof OtpErlangList) {
                return erlangObjectsToJava(((OtpErlangList) erl).elements());
            }
        } catch (final OtpErlangRangeException oere) {
            throw new IllegalArgumentException("Can't convert Erlang character", oere);
        }

        return erl;
    }

    private static List<Object> erlangObjectsToJava(final OtpErlangObject[] erlangObjects) {
        final List<Object> result = new ArrayList<Object>();
        for (final OtpErlangObject erlangObject : erlangObjects) {
            result.add(erlangToJava(erlangObject));
        }
        return result;
    }
}
