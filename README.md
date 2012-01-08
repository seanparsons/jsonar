# JSONAR

JSONAR is a JSON library for Scala, offering the ability to parse text for JSON into an object graph and similarly turn an object graph into a String.

JSONAR avoids pitfalls of some JSON libraries, like the inbuilt Scala implementation, by offering the following features:

* Parsing preserves the errors without throwing exceptions, making for simpler code as you just need to map or flatMap the result.
* Rigidly typing the JSON objects avoiding types like Map[String, Any], to ensure a List couldn't be used as the key in a JSONObject.
* Sensible printing of JSON from the graph, as stock Scala provides none, with tests to ensure that going forwards and backwards parsing and printing produces the same results.
* Simple easy to follow code.

## Use

To include it in your SBT project add the following to the definition

    resolvers += "JSONAR repo" at "https://github.com/seanparsons/jsonar-repo/raw/master/releases/"
    
    libraryDependencies += "com.github.seanparsons.jsonar" %% "jsonar" % "0.8.2"

The first thing you'll want to do is import the core of jsonar:

```scala
import com.github.seanparsons.jsonar._
```
This includes all the necessary implicits and classes for convenience.
    
To parse some JSON use the Parser object:

```scala
val parseResult: ValidationNEL[String, JSONValue] = Parser.parse("[10]")
// scala> parseResult: ValidationNEL[String, JSONValue] = Success(JSONArray(Vector(JSONInt(10))))
```
JSONAR uses [Scalaz](http://code.google.com/p/scalaz/) for the ValidationNel type which is effectively a pimped version of the Either class.

To produce JSON from an instance of JSONValue use the Printer object:

```scala
val json: String = Printer.print(JSONObject(JSONString("key") -> JSONString("value")))
// scala> json: String = {"key":"value"}
```

A XPath-like API is available which lends itself to for comprehensions very nicely:


```scala
val json = """
{
  "users":
  {
    "1":"Martin",
    "2":"Rich",
    "3":"James"
  },
  "friends":
  {
    "1":[2,3],
    "2":[1,3],
    "3":[1]
  }
}
"""

val friendsOfUser = for {
  parsed <- Parser.parse(json)
  user <- (parsed \ "users" \ "1").asJSONString
  friendIDArray <- (parsed \ "friends" \ "1").asJSONArray
  friendIDs <- friendIDArray.collectElements{case JSONInt(value) => value}
} yield (user.value, friendIDs)
```
    
## Design.

JSONAR attempts to be as tightly typed as possible, even to the point of providing two specific objects for the two values of a boolean:

    scala> println(JSONBoolTrue.value)
    true

The core is all inside the [Types.scala](https://github.com/seanparsons/jsonar/blob/master/src/main/scala/com/github/seanparsons/jsonar/Types.scala) file which is well worth skimming over quickly.