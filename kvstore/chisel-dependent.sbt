// Provide a managed dependency on chisel if -DchiselVersion="" is
// supplied on the command line.

val chiselVersion_k = System.getProperty("chiselVersion", "None")

// _k a temporary fix until sbt 13.6 https://github.com/sbt/sbt/issues/1465

libraryDependencies ++= ( if (chiselVersion_k != "None" ) (
    "edu.berkeley.cs" %% "chisel" % chiselVersion_k
) :: Nil; else Nil)
