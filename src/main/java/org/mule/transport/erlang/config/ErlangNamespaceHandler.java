/*
 * $Id: NamespaceHandler.vm 10621 2008-01-30 12:15:16Z dirk.olmes $
 * --------------------------------------------------------------------------------------
 * Copyright (c) MuleSource, Inc.  All rights reserved.  http://www.mulesource.com
 *
 * The software in this package is published under the terms of the CPAL v1.0
 * license, a copy of which has been included with this distribution in the
 * LICENSE.txt file.
 */
package org.mule.transport.erlang.config;

import org.mule.config.spring.handlers.AbstractMuleNamespaceHandler;
import org.mule.config.spring.parsers.specific.TransformerDefinitionParser;
import org.mule.endpoint.URIBuilder;
import org.mule.transport.erlang.ErlangConnector;
import org.mule.transport.erlang.transformers.ErlangMessageToObject;
import org.mule.transport.erlang.transformers.ObjectToErlangMessage;

/**
 * Registers a Bean Definition Parser for handling
 * <code><erlang:connector></code> elements and supporting endpoint elements.
 */
public class ErlangNamespaceHandler extends AbstractMuleNamespaceHandler {
    // TODO cleanup
    public void init() {
        /*
         * This creates handlers for 'endpoint', 'outbound-endpoint' and
         * 'inbound-endpoint' elements. The defaults are sufficient unless you
         * have endpoint styles different from the Mule standard ones The
         * URIBuilder as constants for common required attributes, but you can
         * also pass in a user-defined String[].
         */
        registerStandardTransportEndpoints(ErlangConnector.MULETRANSPORTERLANG, URIBuilder.PATH_ATTRIBUTES);

        /*
         * This will create the handler for your custom 'connector' element. You
         * will need to add handlers for any other xml elements you define. For
         * more information see:
         * http://www.mulesource.org/display/MULE2USER/Creating
         * +a+Custom+XML+Namespace
         */
        registerConnectorDefinitionParser(ErlangConnector.class);

        registerBeanDefinitionParser("erlang-message-to-object-transformer", new TransformerDefinitionParser(
                ErlangMessageToObject.class));

        registerBeanDefinitionParser("object-erlang-message-transformer", new TransformerDefinitionParser(
                ObjectToErlangMessage.class));
    }
}
