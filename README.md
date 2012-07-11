Mule Erlang Transport
=====================

A transport that can send and receive messages to and from Erlang nodes.

Read the [documentation](http://www.mulesoft.org/erlang-transport).

Build
-----

Run:

    mvn clean install


Integration tests:

    mvn -Pit clean verify


Maven Support
-------------

Add the following repository:

    <repository> 
      <id>muleforge</id> 
      <name>MuleForge Repository</name> 
      <url>http://repository.muleforge.org</url> 
    </repository>

To add the Mule Erlang transport to a Maven project add the following dependency:

    <dependency>
      <groupId>org.mule.transports</groupId>
      <artifactId>mule-transport-erlang</artifactId>
      <version>x.y.z</version>
    </dependency>
