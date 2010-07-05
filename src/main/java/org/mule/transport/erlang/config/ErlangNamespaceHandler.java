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

import org.mule.config.spring.factories.InboundEndpointFactoryBean;
import org.mule.config.spring.factories.OutboundEndpointFactoryBean;
import org.mule.config.spring.handlers.AbstractMuleNamespaceHandler;
import org.mule.config.spring.parsers.MuleDefinitionParser;
import org.mule.config.spring.parsers.specific.TransformerDefinitionParser;
import org.mule.config.spring.parsers.specific.endpoint.TransportEndpointDefinitionParser;
import org.mule.config.spring.parsers.specific.endpoint.TransportGlobalEndpointDefinitionParser;
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

    public static final String[][] ERLANG_ATTRIBUTES = new String[][] {
            new String[] { ErlangProperties.NODE_ATTRIBUTE, ErlangProperties.PROCESS_NAME_ATTRIBUTE },
            new String[] { ErlangProperties.NODE_ATTRIBUTE, ErlangProperties.MODULE_FUNCTION_ATTRIBUTE } };

    public void init() {
        registerErlangTransportEndpoints();

        registerConnectorDefinitionParser(ErlangConnector.class);

        registerBeanDefinitionParser("erlang-message-to-object-transformer", new TransformerDefinitionParser(ErlangMessageToObject.class));

        registerBeanDefinitionParser("object-to-erlang-message-transformer", new TransformerDefinitionParser(ObjectToErlangMessage.class));
    }

    /**
     * Need to use the most complex constructors as have mutually exclusive
     * address attributes
     */
    protected void registerErlangTransportEndpoints() {
        registerErlangEndpointDefinitionParser("endpoint", new TransportGlobalEndpointDefinitionParser(ErlangConnector.ERLANG,
                TransportGlobalEndpointDefinitionParser.PROTOCOL, TransportGlobalEndpointDefinitionParser.RESTRICTED_ENDPOINT_ATTRIBUTES,
                ERLANG_ATTRIBUTES, new String[][] {}));

        registerErlangEndpointDefinitionParser("inbound-endpoint", new TransportEndpointDefinitionParser(ErlangConnector.ERLANG,
                TransportEndpointDefinitionParser.PROTOCOL, InboundEndpointFactoryBean.class,
                TransportEndpointDefinitionParser.RESTRICTED_ENDPOINT_ATTRIBUTES, ERLANG_ATTRIBUTES, new String[][] {}));

        registerErlangEndpointDefinitionParser("outbound-endpoint", new TransportEndpointDefinitionParser(ErlangConnector.ERLANG,
                TransportEndpointDefinitionParser.PROTOCOL, OutboundEndpointFactoryBean.class,
                TransportEndpointDefinitionParser.RESTRICTED_ENDPOINT_ATTRIBUTES, ERLANG_ATTRIBUTES, new String[][] {}));
    }

    protected void registerErlangEndpointDefinitionParser(final String element, final MuleDefinitionParser parser) {
        parser.addAlias(ErlangProperties.PROCESS_NAME_ATTRIBUTE, URIBuilder.PATH);
        parser.addAlias(ErlangProperties.MODULE_FUNCTION_ATTRIBUTE, URIBuilder.PATH);
        parser.addAlias(ErlangProperties.NODE_ATTRIBUTE, URIBuilder.HOST);
        registerBeanDefinitionParser(element, parser);
    }

}
