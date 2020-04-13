


import scala.reflect.runtime.{universe => ru}

val mapName = ru.TermName("map")

val listTpe = ru.typeOf[List[Int]]

listTpe.member(mapName)

// standard term names, e.g "<init>", "package", "_root_"
// standard type names  e.g "<error>", "_", "_*"

// A scope object generaly maps names to symbols avaialbe in
// a corresponding lexical scope.

val overridden = listTpe.members.filter(_.isFinal)
listTpe.decls
listTpe.members

import scala.reflect.runtime.universe._
//
//ClassDef(Modifiers(NoFlags), TypeName("C"), Nil, Template)

val r = reify{
    class C
}.tree

showRaw(r)

Literal(Constant(5))

Constant(true) match {
    case Constant(s: String) => println(s"A string $s")
    case Constant(b: Boolean) => println(s"A Boolean value is $b")
    case Constant(x) => println(s"Something else: $x")
}

def tpe: ru.Type = typeOf[{def x: Int; val y: List[Int]}]

show(tpe)
showRaw(tpe)

showRaw(tpe, printIds = true, printKinds = true)

import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._

//val jann = typeOf[JavaAnnottee].typeSymbol.annotations(0).javaArgs




