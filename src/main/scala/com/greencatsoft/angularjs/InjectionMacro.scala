package com.greencatsoft.angularjs

import scala.reflect.macros.blackbox.Context

protected[angularjs] trait InjectionMacro[A <: InjectionTarget] {

  protected[angularjs] def targets[T <: A](c: Context)(implicit tag: c.WeakTypeTag[T]): Iterable[(String, c.universe.TypeSymbol, c.universe.MethodSymbol)] = {
    import c.universe._

    val members = tag.tpe.members collect { case m: MethodSymbol if m.isSetter => m }

    val names = members map { member =>
      val argType = member.paramLists.head.head.typeSignature.dealias.typeSymbol

      val normalizedType = Option(argType.typeSignature) collect {
        case ClassInfoType(_, _, sym) => sym.asType
      }

      val annotations = normalizedType.map(_.annotations).toSeq.flatten.filter(_.tree.tpe =:= typeOf[injectable])

      val members = normalizedType map { n =>
        n.annotations.filter(_.tree.tpe =:= typeOf[injectable]).map(_.tree.children.tail) collect {
          case List(Literal(Constant(literal: String))) => (literal, n.asType, member)
        }
      }

      members.toSeq.flatten
    }

    names.toSeq.flatten
  }

  def inject[T <: A](c: Context)(target: c.Expr[T])(implicit tag: c.WeakTypeTag[T]): c.Expr[ModuleProxy] = {
    import c.universe._

    val dependencies = targets[T](c) collect {
      case (name, _, _) => name
    }

    val assignments = targets[T](c).zipWithIndex collect {
      case ((_, argType, setter), index) =>
        val argument = List(Ident(TermName(s"a$index")))
        val value = TypeApply(
          Select(q"..$argument", TermName("asInstanceOf")),
          List(Ident(c.mirror.staticClass(argType.fullName))))

        Apply(Select(target.tree, setter.asTerm), List(value))
    }

    val handler = q"""{
      import scala.scalajs.js
      import scala.scalajs.js.UndefOr
      import scala.scalajs.js.JSConverters.JSRichOption

      import com.greencatsoft.angularjs.{ ConfigurableTarget, InjectionTarget }
      import com.greencatsoft.angularjs.Module.bindTarget

      val target: InjectionTarget = $target

      val handler: js.ThisFunction10[js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, UndefOr[js.Any]] = 
        (t: js.Any, a0: js.Any, a1: js.Any, a2: js.Any, a3: js.Any, a4: js.Any, a5: js.Any, a6: js.Any, a7: js.Any, a8: js.Any, a9: js.Any) => {

        bindTarget(t, target)

        ..$assignments

        target.initialize()

        val result = target match {
          case configurable: ConfigurableTarget[_] => configurable()
          case _ => null
        }

        Option(result).map(_.asInstanceOf[js.Any]).orUndefined
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