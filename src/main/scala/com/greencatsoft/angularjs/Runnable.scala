package com.greencatsoft.angularjs

import scala.reflect.macros.blackbox.Context

trait Runnable extends InjectionTarget

protected[angularjs] object Runnable extends InjectionMacro[Runnable] {

  override def inject[T <: Runnable](c: Context)(target: c.Expr[T])(implicit tag: c.WeakTypeTag[T]): c.Expr[ModuleProxy] =
    super.inject[T](c)(target)

  override def register[T <: Runnable](c: Context)(target: c.Expr[T]): c.universe.Tree = {
    import c.universe._
    q"${c.prefix.tree}.module.run(dependencies)"
  }
}