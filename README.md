# JSONAR

JSONAR is a JSON library for Scala, offering the ability to parse text for JSON into an object graph and similarly turn an object graph into a String.

## Use

The first thing you'll want to do is import the bulk of the classes:

```scala
import com.github.seanparsons.jsonar._
```
Optionally also import the contents of the Implicits object for some convenience implicit conversions:

```scala
import com.github.seanparsons.jsonar.Implicits._
```
    
To parse some JSON use the Parser object:

```scala
val parseResult: ValidationNel[Parser.ParserError, JSONValue] = Parser.parse("[10]")
```
JSONAR uses [Scalaz](http://code.google.com/p/scalaz/) for the ValidationNel type which is effectively a pimped version of the Either class.

To produce JSON from an instance of JSONValue use the Printer object:

```scala
val json: String = Printer.print(JSONObject("key" -> "value"))
```
    
## Design.

JSONAR attempts to be as tightly typed as possible, even to the point of providing two specific objects for the two values of a boolean:
    scala> println(JSONBoolTrue.value)
    true

The core is all inside the [Types.scala](https://github.com/seanparsons/jsonar/blob/master/src/main/scala/com/github/seanparsons/jsonar/Types.scala) file which is well worth skimming over quickly.