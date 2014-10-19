package com.greencatsoft.angularjs

import scala.reflect.macros.blackbox.Context

protected[angularjs] trait InjectionMacro[A <: InjectionTarget] {

  protected[angularjs] def targets[T <: A](c: Context)(implicit tag: c.WeakTypeTag[T]): Iterable[(String, c.universe.TermSymbol)] = {
    import c.universe._

    val members = tag.tpe.members collect { case s: TermSymbol if s.isVar => s }

    val names = members map { member =>
      val annotations = member.typeSignature.typeSymbol.annotations.filter(_.tree.tpe =:= typeOf[injectable])

      annotations.headOption.map(_.tree.children.tail) collect {
        case List(Literal(Constant(literal: String))) => (literal, member)
      }
    }

    names.map(_.toSeq).flatten
  }

  def inject[T <: A](c: Context)(target: c.Expr[T])(implicit tag: c.WeakTypeTag[T]): c.Expr[ModuleProxy] = {
    import c.universe._

    val dependencies = targets[T](c) collect {
      case (name, _) => name
    }

    val assignments = targets[T](c).zipWithIndex collect {
      case ((_, member), index) =>
        val fullName = List(Ident(c.mirror.staticClass(member.typeSignature.typeSymbol.fullName)))

        val variable = TermName(member.asTerm.name.decodedName.toString.trim)
        val argument = List(Ident(TermName(s"a$index")))

        Assign(
          Select(target.tree, variable),
          TypeApply(Select(q"..$argument", TermName("asInstanceOf")), fullName))
    }

    val handler = q"""{
      import scala.scalajs.js
      import scala.scalajs.js.UndefOr
      import scala.scalajs.js.JSConverters.JSRichOption

      import com.greencatsoft.angularjs.{ ConfigurableTarget, InjectionTarget }
      import com.greencatsoft.angularjs.Module.bindTarget

      val target: InjectionTarget = $target
      val handler = new js.ThisFunction10[js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, UndefOr[js.Any]] {
        override def apply(t: js.Any, a0: js.Any, a1: js.Any, a2: js.Any, a3: js.Any, a4: js.Any,
          a5: js.Any, a6: js.Any, a7: js.Any, a8: js.Any, a9: js.Any): UndefOr[js.Any] = { 
          bindTarget(t, target)

          ..$assignments

          target.initialize()

          val result = target match {
            case configurable: ConfigurableTarget[_] => configurable()
            case _ => null
          }

          Option(result).map(_.asInstanceOf[js.Any]).orUndefined
        }
      }

      val dependencies = js.Array[js.Any](..$dependencies)
      dependencies.push(handler)

      handler.asInstanceOf[js.Dynamic].obj = target.asInstanceOf[js.Object]

      ${register(c)(target)}
      ${c.prefix.tree}
    }"""

    c.Expr[ModuleProxy](handler)
  }

  def register[T <: A](c: Context)(target: c.Expr[T]): c.universe.Tree
}