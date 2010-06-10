/*
 * $Id$
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */

package org.mule.transport.erlang.i18n;

import org.mule.config.i18n.Message;
import org.mule.config.i18n.MessageFactory;
import org.mule.transport.erlang.ErlangInvocation.InvocationType;

public class ErlangMessages extends MessageFactory {
    private static final ErlangMessages MF = new ErlangMessages();
    private static final String BUNDLE_PATH = getBundlePath("erlang");

    public static Message missingNodeName() {
        return MF.createMessage(BUNDLE_PATH, 0);
    }

    public static Message missingCookieWithPort() {
        return MF.createMessage(BUNDLE_PATH, 1);
    }

    public static Message nodeUnreachable(final String nodeName) {
        return MF.createMessage(BUNDLE_PATH, 2, nodeName);
    }

    public static Message missingEndpointProperty(final String propertyName) {
        return MF.createMessage(BUNDLE_PATH, 3, propertyName);
    }

    public static Message badResponseFormat(final InvocationType invocationType) {
        return MF.createMessage(BUNDLE_PATH, 4, invocationType.toString());
    }

    public static Message responseTimeOut() {
        return MF.createMessage(BUNDLE_PATH, 5);
    }

    public static Message mailboxNameRegistrationFailed(final String name) {
        return MF.createMessage(BUNDLE_PATH, 6, name);
    }

}
