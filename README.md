# Jerkson for Scala 2.10 [![Build Status](https://travis-ci.org/masahitojp/jerkson.svg?branch=master)](https://travis-ci.org/masahitojp/jerkson) #

[Jerkson](https://github.com/codahale/jerkson) has only been published for
Scala versions as high as 2.9.1.

## Differences from upstream Jerkson ##

- sbt instead of Maven.
- Minor tweaks to get compilation in 2.10.
- [Streaming iteration patch](https://github.com/ymasory/jerkson/pull/1), if you
  use version 0.6.2+.
- [Streaming field iterator patch](https://github.com/cphylabs/jerkson/commit/85076b9daeeb3ba3c3c3f0fbdd60deaf09fb275b), if you use version 0.6.3+.

## Install ##

- This version of Jerkson is hosted on
  [Maven Central](http://central.maven.org/maven2/com/cloudphysics/jerkson_2.10).
- From sbt:

  ```scala
  libraryDependencies += "com.cloudphysics %% "jerkson" % "0.6.1"
  ```
- From Maven:

  ```xml
  <dependency>
    <groupId>com.cloudphysics</groupId>
    <artifactId>jerkson_2.10</artifactId>
    <version>0.6.1</version>
  </dependency>
  ```

## Build ##

```sh
$ cd jerkson
$ sbt compile
```

## Future plans ##

Jerkson is legacy.
We won't do any further work except providing the 2.10 jars.
We suggest you switch to one of the many excellent Scala Json libraries.

## Contact ##

[opensource@cloudphysics.com](opensource@cloudphysics.com)
