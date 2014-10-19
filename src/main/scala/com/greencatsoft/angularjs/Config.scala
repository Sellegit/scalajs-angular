package com.greencatsoft.angularjs

import scala.reflect.macros.blackbox.Context

trait Config extends InjectionTarget

protected[angularjs] object Config extends InjectionMacro[Config] {

  override def inject[T <: Config](c: Context)(target: c.Expr[T])(implicit tag: c.WeakTypeTag[T]): c.Expr[ModuleProxy] =
    super.inject[T](c)(target)

  override def register[T <: Config](c: Context)(target: c.Expr[T]): c.universe.Tree = {
    import c.universe._
    q"${c.prefix.tree}.module.config(dependencies)"
  }
}