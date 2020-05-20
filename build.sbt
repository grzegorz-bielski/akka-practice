name := "akka-practice"
version := "0.1"
scalaVersion := "2.12.10"

val akkaVersion = "2.6.4"
val akkaHttpVersion = "10.1.11"
// lazy val leveldbVersion = "0.7"
// lazy val leveldbjniVersion = "1.8"
// lazy val postgresVersion = "42.2.2"
// lazy val cassandraVersion = "0.91"
// lazy val json4sVersion = "3.2.11"
// lazy val protobufVersion = "3.6.1"

resolvers += Resolver.jcenterRepo

scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.play" %% "play-json" % "2.8.1",
  "org.scalatest" %% "scalatest" % "3.0.5",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime
  // // local levelDB stores
  // "org.iq80.leveldb" % "leveldb" % leveldbVersion,
  // "org.fusesource.leveldbjni" % "leveldbjni-all" % leveldbjniVersion,
  // // JDBC with PostgreSQL
  // "org.postgresql" % "postgresql" % postgresVersion,
  // "com.github.dnvriend" %% "akka-persistence-jdbc" % "3.4.0",
  // // Cassandra
  // "com.typesafe.akka" %% "akka-persistence-cassandra" % cassandraVersion,
  // "com.typesafe.akka" %% "akka-persistence-cassandra-launcher" % cassandraVersion % Test,
  // // Google Protocol Buffers
  // "com.google.protobuf" % "protobuf-java" % protobufVersion
)
