import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}

class C[T] {
    def test[U](x: T)(y: U): Int = ???
}

val testMember = ru.typeOf[C[Int]].member(ru.TermName("test"))
testMember.asMethod

ru.typeOf[List[Int]]

type T[X] = (X, X)

def getType[T: ru.TypeTag](obj: T): ru.Type = ru.typeOf[T]

getType(List(1, 2, 3))

class Animal

class Cat extends Animal

val a = new Animal
val c = new Cat

getType(a)
getType(c)

val intType = ru.definitions.IntTpe
val intClass = ru.definitions.IntClass


class A

class B extends A

ru.typeOf[A] <:< ru.typeOf[B]

ru.typeOf[B] <:< ru.typeOf[A]

ru.definitions.IntTpe weak_<:< ru.definitions.LongTpe
ru.definitions.IntTpe <:< ru.definitions.LongTpe
"""
  |In dome situations Scala uses a more general conformance relation.
  |A type S weakly conforms to a type T, written S
  |""".stripMargin

if (true) 1 else 1d

ru.definitions.IntTpe =:= ru.definitions.LongTpe
ru.definitions.IntTpe =:= ru.definitions.IntTpe

val a1 = new A
val a2 = new A
getType(a1) =:= getType(a2)

getType(List(1, 2, 3)) =:= getType(List(1.0, 2.0, 3.0))

getType(List(1, 2, 3)) =:= getType(List(3, 4, 5))


type HIstogram = List[Int]

ru.typeOf[HIstogram] =:= getType(List(3, 4))

ru.typeOf[HIstogram] == getType(List(3, 4))

//import scala.reflect.runtime.universe._

ru.typeOf[List[_]].member(ru.TermName("map"))
ru.typeOf[List[_]].member(ru.TypeName("Self"))

ru.typeOf[List[Int]].members.filter(_.isPrivate).foreach(println)

// Trees

ru.reify(3).tree

ru.reify(
    {
        val x = 3
    }
).tree

import scala.reflect.runtime.universe._

val tree = Apply(Select(Ident(TermName("x")), TermName("$plus")), List(Literal(Constant(2))))

show(tree)
showRaw(tree)

val expr = reify {
    class Flower {
        def name = "Rose"
    }
}

showRaw(expr)


showRaw(tree)

val (fun, arg) = tree match {
    case Apply(fn, a :: Nil) => (fn, a)
}

println(s"fun is $fun arg is $arg")

val tree2 = Apply(Select(Apply(Select(Ident(TermName("x")), TermName("$plus")), List(Literal(Constant(2)))), TermName("$plus")), List(Literal(Constant(3))))

val Apply(fun2, arg2 :: Nil) = tree2

println(s"fun2 is $fun2 arg2 is $arg2")

object traverser extends Traverser {
    val applies = ListBuffer.empty[Apply]
    override def traverse(tree: ru.Tree): Unit = tree match {
        case app @ Apply(fun, args) =>
            applies += app // 相当于对方法做了拦截的操作!!!
            super.traverse(fun)
            super.traverseTrees(args)
        case _ => super.traverse(tree)
    }


}

traverser.traverse(tree2)

traverser.applies.foreach(println)


{
    val tree = reify(println(2)).tree
    showRaw(tree)
}


val x = reify(2)

val fn = reify(println(20))

reify(fn.splice)


import scala.tools.reflect.ToolBox

val tb = ru.runtimeMirror(getClass.getClassLoader).mkToolBox()

showRaw(tb.parse("println(2)"))


import scala.language.experimental.macros

def impl(c: scala.reflect.macros.Context): c.Expr[Unit] = c.Expr[Unit](c.parse("println(2)"))

def test:Unit  = macro impl  // 果然是一个括号的差别
test

import scala.reflect.runtime.universe._
val tree3 = reify{
    "test".length
}.tree

import scala.tools.reflect.ToolBox

val tb2 = ru.runtimeMirror(getClass.getClassLoader).mkToolBox()

val ttree = tb.typecheck(tree3)

ttree.tpe
ttree.symbol

Apply(Ident(TermName("println")), List(Literal(Constant(2))))


















