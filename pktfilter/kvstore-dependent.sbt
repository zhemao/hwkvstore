val kvstoreVersion = System.getProperty("kvstoreVersion", "None")

libraryDependencies ++= ( if (kvstoreVersion != "None" ) (
  "edu.berkeley.eecs" %% "kvstore" % kvstoreVersion
) :: Nil; else Nil)
