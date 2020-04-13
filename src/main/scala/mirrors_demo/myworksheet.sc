import scala.reflect.runtime.{universe => ru}
import scala.language.experimental.macros
val classLoaderMirror = ru.runtimeMirror(getClass.getClassLoader)

class C {
    def x = 2
    val y = 3
    var z = 4
}

val instancemirror =  classLoaderMirror.reflect(new C)

val instancemethodsymbol = instancemirror.symbol.info.member(ru.TermName("x")).asMethod

val classmethodsymboal = ru.typeOf[C].decl(ru.TermName("x")).asMethod

println(s"im == cm ${instancemethodsymbol == classmethodsymboal}")

val instaceMethod = instancemirror.reflectMethod(instancemethodsymbol)

instaceMethod()

val fieldY = instancemirror.symbol.info.member(ru.TermName("y")).asTerm

val fmY = instancemirror.reflectField(fieldY)

fmY.get
fmY.set(44)
fmY.get

val fieldZ = instancemirror.symbol.info.member(ru.TermName("z")).asTerm
val fmZ = instancemirror.reflectField(fieldZ)
fmZ.get
fmZ.set(44)
fmZ.get

case class D(x: Int)

val classD = ru.typeOf[D].typeSymbol.asClass

val dm = classLoaderMirror.reflectClass(classD)

val ctorD = dm.symbol.info.member(ru.termNames.CONSTRUCTOR).asMethod

val ctorm = dm.reflectConstructor(ctorD)

ctorm(4)

object E { def x = 3 }

val objectE = ru.typeOf[E.type].termSymbol.asModule
val mm = classLoaderMirror.reflectModule(objectE)

mm.instance


import scala.reflect.macros.Context
case class Location(fileName: String, line: Int, column: Int)

object Macros2 {
    def currentLocation: Location = macro impl

    def impl(c: Context): c.Expr[Location] = {
        import c.universe._
        val pos = c.macroApplication.pos
        val clsLocation = c.mirror.staticModule("Location")
        c.Expr(Apply(Ident(clsLocation), List(Literal(Constant(pos.source.path)), Literal(Constant(pos.line)), Literal(Constant(pos.column)))))

    }
}

//Macros2.currentLocation





