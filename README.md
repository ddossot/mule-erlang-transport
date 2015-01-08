# Mule Erlang Transport

A transport that can send and receive messages to and from Erlang nodes.

Read the [documentation](http://www.mulesoft.org/erlang-transport).

## Build

Run:

    mvn clean install


Integration tests:

    mvn -Pit clean verify


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



