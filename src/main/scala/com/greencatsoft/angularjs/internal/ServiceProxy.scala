package com.greencatsoft.angularjs.internal

import scala.reflect.macros.blackbox.Context
import scala.scalajs.js
import scala.scalajs.js.Any.fromString
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr.{ any2undefOrA, undefOr2ops }

import com.greencatsoft.angularjs.{resolve, Service, inject, injectable}

object ServiceProxy {

  def bind(service: js.Any, target: Service) {
    try {
      service.asInstanceOf[js.Dynamic]._ng_service_proxy = target.asInstanceOf[js.Object]
    } catch {
      case _: Throwable =>
    }
  }

  def unbind[A <: Service](service: js.Any): Option[A] = {
    val target: UndefOr[Any] = service.asInstanceOf[js.Dynamic]._ng_service_proxy
    target.map(_.asInstanceOf[A]).toOption
  }

  def dependencies[A <: Service](c: Context)(implicit tag: c.WeakTypeTag[A]): Iterable[(String, c.universe.TypeSymbol, c.universe.MethodSymbol)] = {
    import c.universe._

//    Should check if @inject annotation is present:
//
//    val members = tag.tpe.members filter {
//      _.annotations.exists(_.tree.tpe =:= typeOf[inject])
//    } collect {
//      case m: MethodSymbol if m.isSetter => m
//    }

//        val hehe = tag.tpe.members filter {
//          _.annotations.exists(_.tree.tpe =:= typeOf[resolve])
//        }
//    println("hehe")
//
//        println(hehe)
//    println(hehe.map(_.typeSignature))
//    println(hehe.map(_.getClass))
    val members = tag.tpe.members collect { case m: MethodSymbol if m.isSetter => m }

//    val membersAndSetters = tag.tpe.members filter {
//      _.annotations.exists(_.tree.tpe =:= typeOf[resolve] || _.tree.tpe =:= typeOf[inject])
//    } map {
//      (_, _.setter)
//    }
    val membersAndSetters = tag.tpe.members collect {
      case m: TermSymbol if m.setter.isInstanceOf[MethodSymbol] => (m, m.setter.asInstanceOf[MethodSymbol])
    } groupBy {_._2} map {_._2.head}

//    System.out.println("!!!====================================================\n")
//    System.out.println(members.toString())
//    System.out.println("----------------------------------------------------\n")

    val names = membersAndSetters map { case (privateMember, member) =>
      println(member)
      println(member.annotations)
      val argType = member.paramLists.head.head.typeSignature.dealias.typeSymbol


      val members = for (
        normalized <- Option(argType.typeSignature) collect {
          case ClassInfoType(_, _, sym) => sym.asType
        }
      ) yield if (privateMember.annotations.exists(_.tree.tpe =:= typeOf[resolve])) {
          println("has resolve")
          privateMember.annotations.filter(_.tree.tpe =:= typeOf[resolve]).map(_.tree.children.tail) collect {
            case List(Literal(Constant(literal: String))) => (literal, normalized.asType, member)
          }
        } else {
          println("no resolve")
          normalized.annotations.filter(_.tree.tpe =:= typeOf[injectable]).map(_.tree.children.tail) collect {
            case List(Literal(Constant(literal: String))) => (literal, normalized.asType, member)
          }
        }

      members.toSeq.flatten
    }

//    System.out.println(names.toString())
//    System.out.println("!!!----------------------------------------------------\n")
    names.toSeq.flatten
  }

  def newInstance[A <: Service](c: Context)(target: c.Expr[A])(implicit tag: c.WeakTypeTag[A]): c.Expr[js.Any] = {
    import c.universe._

    val names = dependencies[A](c) collect {
      case (name, _, _) => name
    }

    val assignments = dependencies[A](c).zipWithIndex collect {
      case ((_, argType, setter), index) =>
        val argument = List(Ident(TermName(s"a$index")))
        val value = TypeApply(
          Select(q"..$argument", TermName("asInstanceOf")),
          List(Ident(c.mirror.staticClass(argType.fullName))))

        Apply(Select(q"target", setter.asTerm), List(value))
    }

    val proxy = q"""{
      import scala.scalajs.js
      import scala.scalajs.js.UndefOr
      import scala.scalajs.js.JSConverters.JSRichOption

      import com.greencatsoft.angularjs.{ Initializable, Service }
      import com.greencatsoft.angularjs.internal.ServiceProxy

      val target: Service = $target

      val handler: js.ThisFunction10[js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, js.Any, UndefOr[js.Any]] =
        (t: js.Any, a0: js.Any, a1: js.Any, a2: js.Any, a3: js.Any, a4: js.Any, a5: js.Any, a6: js.Any, a7: js.Any, a8: js.Any, a9: js.Any) => {

        try {
          ServiceProxy.bind(t, target)

          ..$assignments

          target match {
            case init: Initializable => init.initialize()
            case _ =>
          }

          val result = target match {
            case configurable: Function0[_] => configurable()
            case injected: Injected => injected.run()
            case _ => null
          }

          Option(result).map(_.asInstanceOf[js.Any]).orUndefined
        } catch {
          case e: Exception => js.eval("debugger"); Option(js.eval("undefined")).orUndefined
        }
      }

      val proxy = js.Array[js.Any](..$names)

      proxy.push(handler)
      proxy
    }"""

    c.Expr[js.Any](proxy)
  }
}
