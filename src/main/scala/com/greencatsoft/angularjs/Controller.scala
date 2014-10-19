package com.greencatsoft.angularjs

import scala.reflect.macros.blackbox.Context

import com.greencatsoft.angularjs.core.ScopeAware

trait Controller extends NamedTarget with ScopeAware

abstract class AbstractController(name: String)
  extends AbstractNamedTarget(name) with Controller

trait TemplateController extends Controller {

  val templateUrl: String

  val title: Option[String] = None
}

protected[angularjs] object Controller extends InjectionMacro[Controller] {

  override def inject[T <: Controller](c: Context)(target: c.Expr[T])(implicit tag: c.WeakTypeTag[T]): c.Expr[ModuleProxy] =
    super.inject[T](c)(target)

  override def register[T <: Controller](c: Context)(target: c.Expr[T]): c.universe.Tree = {
    import c.universe._

    val name = Select(target.tree, TermName("name"))
    q"${c.prefix.tree}.module.controller($name, dependencies)"
  }
}