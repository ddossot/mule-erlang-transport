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

    <repository>
      <id>muleforge-repo</id>
      <name>MuleForge Repository</name>
      <url>https://repository.mulesoft.org/nexus/content/repositories/releases</url>
      <layout>default</layout>
    </repository>

To add the Mule Erlang transport to a Maven project add the following dependency:

    <dependency>
      <groupId>org.mule.transports</groupId>
      <artifactId>mule-transport-erlang</artifactId>
      <version>x.y.z</version>
    </dependency>

Only versions `3.2.0` and `3.3.0` are available there. Currently, snapshots of more recent versions need to be built and installed in your Maven repository.

Future releases will be done to Maven central, since Mulesoft has abandonned Muleforge, the platform where this transport was hosted and built. Please create an issue if you need a release to be done.

# Using the Erlang transport
## Connector configuration
### Basic configuration

    <erlang:connector cookie="mule_test_cookie" name="ErlangConnector" nodename="MuleIT" />

With this configuration, Mule will be accessible from remote Erlang nodes as if it was an Erlang node started this way:

    erl -sname MuleIT -setcookie mule_test_cookie

You can host several of such connectors in a single Mule instance.
