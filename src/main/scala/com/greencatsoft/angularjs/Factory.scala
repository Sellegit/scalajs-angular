package com.greencatsoft.angularjs

import scala.reflect.macros.blackbox.Context

trait Factory[A] extends NamedTarget with ConfigurableTarget[A] {

  override def initialize(): Unit = Unit
}

protected[angularjs] object Factory extends InjectionMacro[Factory[_]] {

  override def inject[T <: Factory[_]](c: Context)(target: c.Expr[T])(implicit tag: c.WeakTypeTag[T]): c.Expr[ModuleProxy] =
    super.inject[T](c)(target)

  override def register[T <: Factory[_]](c: Context)(target: c.Expr[T]): c.universe.Tree = {
    import c.universe._

    val name = Select(target.tree, TermName("name"))
    q"${c.prefix.tree}.module.factory($name, dependencies)"
  }
}