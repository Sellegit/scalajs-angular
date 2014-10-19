package com.greencatsoft.angularjs.core

import scala.scalajs.js

import com.greencatsoft.angularjs.{ inject, injectable, InjectionTarget }

@injectable("$scope")
trait Scope extends js.Object {

  def $id: String = ???

  def $apply(exp: js.Any = null): js.Any = ???

  def $broadcast(name: String, args: js.Any*): js.Object = ???

  def $destroy(): Unit = ???

  def $digest(): Unit = ???

  def $emit(name: String, args: js.Any*): js.Object = ???

  def $eval(expression: js.Any = null, locals: js.Object = null): js.Any = ???

  def $evalAsync(expression: js.Any = null): Unit = ???

  def $new(isolate: Boolean): Scope = ???

  def $on(name: String, listener: js.Function): js.Any = ???

  def $watch(watchExpression: js.Any, listener: js.Any = null, objectEquality: Boolean = false): js.Function = ???

  def $watchCollection(obj: js.Any, listener: js.Function): js.Function = ???
}

trait Scoped {

  type ScopeType <: Scope

  implicit class DynamicScope(scope: ScopeType) {

    def dynamic = scope.asInstanceOf[js.Dynamic]
  }
}

trait ScopeAware extends InjectionTarget with Scoped {

  var currentScope: Option[ScopeType] = None

  @inject
  private var scope: ScopeType = _

  override def initialize() {
    scope.dynamic.controller = this.asInstanceOf[js.Object]

    this.currentScope = Some(scope)

    currentScope.foreach(initialize(_))
  }

  def initialize(scope: ScopeType): Unit = Unit
}

@injectable("$rootScope")
trait RootScope extends Scope

trait RootScopeAware extends InjectionTarget with Scoped {

  @inject
  implicit var rootScope: ScopeType = _

  override type ScopeType <: RootScope
}