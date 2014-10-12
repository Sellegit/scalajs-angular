package com.greencatsoft.angularjs

import scala.reflect.macros.blackbox.Context
import scala.scalajs.js

trait InjectionTarget {

  def dependencies: Seq[String] = Nil

  def inject(args: Seq[js.Any]): Unit = Unit

  def initialize(): Unit
}

object InjectionTarget {

  def targets[T <: InjectionTarget](c: Context)(implicit tag: c.WeakTypeTag[T]): Iterable[(String, c.universe.TermSymbol)] = {
    import c.universe._

    val members = tag.tpe.members
      .filter(_.isTerm)
      .map(_.asTerm)
      .filter(_.isVar)
      .filter(_.annotations.exists(_.tree.tpe =:= typeOf[Inject]))

    members map {
      member =>
        val name = member.annotations.map(_.tree.children.tail.headOption) collect {
          case Some(Literal(Constant(literal: String))) => literal
        }

        ("$" + name.headOption.getOrElse(member.asTerm.name.decodedName.toString.trim), member)
    }
  }

  def dependenciesImpl[T <: InjectionTarget](c: Context)(implicit tag: c.WeakTypeTag[T]): c.Expr[Seq[String]] = {
    import c.universe._

    val names = targets(c) collect {
      case (name, _) => name
    }

    c.Expr[Seq[String]](q"""Seq(..$names)""")
  }

  def injectImpl[T <: InjectionTarget](c: Context)(args: c.Expr[Seq[js.Any]])(implicit tag: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._

    val expr = targets(c) collect {
      case (name, field) =>
        val key = Literal(Constant(name))

        Assign(
          Ident(field.asTerm.name), TypeApply(Select(q"""$args(dependencies.indexOf($key))""", TermName("asInstanceOf")),
            List(Ident(field.typeSignature.typeSymbol.name.toTypeName))))
    }

    c.Expr[Unit](q"{..$expr}")
  }
}