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
import org.mule.transport.erlang.ErlangProperties;
import org.mule.transport.erlang.transformers.ErlangMessageToObject;
import org.mule.transport.erlang.transformers.ObjectToErlangMessage;

/**
 * Registers a Bean Definition Parser for handling
 * <code><erlang:connector></code> elements and supporting endpoint elements.
 */
public class ErlangNamespaceHandler extends AbstractMuleNamespaceHandler {

    public void init() {
        // TODO add required attributes here and in schema based on how the
        // receiver and requester will evolve
        registerStandardTransportEndpoints(ErlangConnector.ERLANG,
                new String[] { ErlangProperties.NODE_PROPERTY, ErlangProperties.PROCESS_NAME_PROPERTY }).addAlias(
                ErlangProperties.PROCESS_NAME_PROPERTY, URIBuilder.PATH).addAlias(ErlangProperties.NODE_PROPERTY,
                URIBuilder.HOST);

        registerConnectorDefinitionParser(ErlangConnector.class);

        registerBeanDefinitionParser("erlang-message-to-object-transformer", new TransformerDefinitionParser(
                ErlangMessageToObject.class));

        registerBeanDefinitionParser("object-erlang-message-transformer", new TransformerDefinitionParser(
                ObjectToErlangMessage.class));
    }

}
