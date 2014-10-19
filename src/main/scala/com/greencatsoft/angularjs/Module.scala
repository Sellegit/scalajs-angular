package com.greencatsoft.angularjs

import scala.language.experimental.macros
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr.{ any2undefOrA, undefOr2ops }

trait Module extends js.Object {

  def factory(name: String, constructor: js.Array[js.Any]): Module = ???

  def controller(name: String, constructor: js.Array[js.Any]): Module = ???

  def config(constructor: js.Array[js.Any]): Module = ???

  def run(constructor: js.Array[js.Any]): Module = ???

  def directive(name: String, directiveFactory: js.Array[js.Any]): Module = ???
}

object Module {

  def apply(module: Module) = new ModuleProxy(module)

  def bindTarget(service: js.Any, target: InjectionTarget) {
    try {
      service.asInstanceOf[js.Dynamic]._serviceTarget = target.asInstanceOf[js.Object]
    } catch {
      case _: Throwable =>
    }
  }

  def unbindTarget[A <: InjectionTarget](service: js.Any): Option[A] = {
    val target: UndefOr[Any] = service.asInstanceOf[js.Dynamic]._serviceTarget

    target.map(_.asInstanceOf[A]).toOption
  }
}

class ModuleProxy(val module: Module) {
  require(module != null, "Missing argument 'module'.")

  def config[A <: Config](target: A): ModuleProxy = macro Config.inject[A]

  def controller[A <: Controller](target: A): ModuleProxy = macro Controller.inject[A]

  def directive[A <: Directive](target: A): ModuleProxy = macro Directive.inject[A]

  def factory[A <: Factory[_]](target: A): ModuleProxy = macro Factory.inject[A]

  def run[A <: Runnable](target: A): ModuleProxy = macro Runnable.inject[A]
}
