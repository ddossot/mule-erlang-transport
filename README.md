<p align="center">
<img src="https://raw.githubusercontent.com/ddossot/mule-erlang-transport/mule-3.x/mule-erlang.png" alt="Mule Erlang Logo" />
</p>

# Mule Erlang Transport

A transport that can send and receive messages to and from Erlang nodes.

## Pre-requisite

Erlang R13B01 or better must be installed on the host where you want to use this transport.

## Before using the Erlang transport

[epmd](https://web.archive.org/web/20120818125617/http://www.erlang.org/doc/man/epmd.html) must be running before starting the Erlang connector.

If you have an Erlang application already running on the host where Mule will run, `epmd` will be running already. Otherwise, you'll need to start it by hand.
For this, use the startup command that is appropriate to your environment (for example, [for Linux](https://web.archive.org/web/20120818125617/http://linux.die.net/man/1/epmd)).

## Build

Run:

    mvn clean install

Integration tests:

    mvn -Pit clean verify

Erlang Escripts must be executable on your platorm for the integration tests to run.

## Maven Support

Add the following repository:

```xml
<repository>
  <id>muleforge-repo</id>
  <name>MuleForge Repository</name>
  <url>https://repository.mulesoft.org/nexus/content/repositories/releases</url>
  <layout>default</layout>
</repository>
```

To add the Mule Erlang transport to a Maven project add the following dependency:

```xml
<dependency>
  <groupId>org.mule.transports</groupId>
  <artifactId>mule-transport-erlang</artifactId>
  <version>x.y.z</version>
</dependency>
```

Only versions `3.2.0` and `3.3.0` are available there. Currently, snapshots of more recent versions need to be built and installed in your Maven repository.

Future releases will be done to Maven central, since Mulesoft has abandonned Muleforge, the platform where this transport was hosted and built. Please create an issue if you need a release to be done.

# Using the Erlang transport
## Connector configuration
### Basic configuration

```xml
<erlang:connector cookie="mule_test_cookie" name="ErlangConnector" nodename="MuleIT" />
```

With this configuration, Mule will be accessible from remote Erlang nodes as if it was an Erlang node started this way:

    erl -sname MuleIT -setcookie mule_test_cookie

You can host several of such connectors in a single Mule instance.

## Inbound endpoint configuration

Mule takes care of the threading aspect of the services you expose as Erlang processes: each incoming request gets routed in its own thread to enable maximum throughput while using a single process inbox as the main entry point.

###For Request-Response services

```xml
<simple-service name="ErlangRequestResponseService">
  <erlang:inbound-endpoint exchange-pattern="request-response" processname="capitalizer">
    <component>
      <method-entry-point-resolver>
        <include-entry-point method="capitalizeFully" />
        <singleton-object class="org.apache.commons.lang.WordUtils" />
      </method-entry-point-resolver>
    </component>
  </erlang:inbound-endpoint>
</simple-service>
```

The above service can be invoked as if it is a pure OTP gen_server:

```erlang
gen_server:call({capitalizer, 'MuleIT'}, "hello World").

"Hello World"
```

The Erlang transport also understands PID wrapped calls:

```erlang
{capitalizer, 'MuleIT'} ! {self(), "hello World"}.
receive(M) M end.

"Hello World"
```

###For One-Way services
```xml
<bridge exchange-pattern="one-way" name="ErlangOneWayService">
  <erlang:inbound-endpoint processname="jms_bridge">
    <jms:outbound-endpoint queue="erlang.it.queue" />
  </erlang:inbound-endpoint>
</bridge>
```

The above service can be invoked as if it is a pure OTP gen_server:

```erlang
gen_server:cast({jms_bridge, 'MuleIT@ddossot-laptop'}, "Lorem ipsum").

ok
```

The Erlang transport also understands raw calls:

```erlang
{jms_bridge, 'MuleIT@ddossot-laptop'} ! "Lorem ipsum".
```

In both cases, a JMS TextMessage has been generated from the Erlang call!

##Outbound endpoint configuration
###For gen_server calls

```xml
<erlang:outbound-endpoint
        exchange-pattern="request-response"
        failiftimeout="true"
        invocationtype="GEN_CALL"
        node="nodeName@hostName"
        processname="... gen_server name ..." />
```

###For gen_server casts

```xml
<erlang:outbound-endpoint
        invocationtype="GEN_CAST"
        node="nodeName@hostName"
        processname="... gen_server name ..." /> 
```

###For PID wrapped calls

In this kind of calls, the payload is wrapped in a tuple with the PID of the calling process as the first element. This allows the remote process to send a response back.

```xml
<erlang:outbound-endpoint
        exchange-pattern="request-response"
        invocationtype="PID_WRAPPED"
        node="nodeName@hostName"
        processname="...registered process name ..." />
```

### For raw calls

In this kind of calls, the payload is sent as is to the remote process.

```xml
<erlang:outbound-endpoint
        invocationtype="RAW"
        node="nodeName@hostName"
        processname="...registered process name ..." />
```

###For RPC calls

Allows the invocation of a module:function on a target node, using the current message as the arguments.

```xml
<erlang:outbound-endpoint
        exchange-pattern="request-response"
        invocationtype="RPC" 
        modulefunction="module:function"
        node="nodeName@hostName" />
```
