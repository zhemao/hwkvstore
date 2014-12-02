// Provide a managed dependency on chisel if -DchiselVersion="" is
// supplied on the command line.

val chiselVersion_p = System.getProperty("chiselVersion", "None")

// _p a temporary fix until sbt 13.6 https://github.com/sbt/sbt/issues/1465

libraryDependencies ++= ( if (chiselVersion_p != "None" ) (
    "edu.berkeley.cs" %% "chisel" % chiselVersion_p
) :: Nil; else Nil)
