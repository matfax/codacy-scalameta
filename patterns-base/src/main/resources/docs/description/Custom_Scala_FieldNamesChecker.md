A consistent naming convention for field names can make code easier to read and understand.
You can parameterize the regex that validates your field names. The default regex is "^[a-z][A-Za-z0-9]*$".
There are also options to specify if you want this pattern to check for enum variables (includeEnums),
or object variables (includeObjects) that are ignored by default (complying with the Scala naming conventions).
 
 [More Info](http://docs.scala-lang.org/style/naming-conventions.html)
 