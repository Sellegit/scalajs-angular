package com.greencatsoft.angularjs

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context

import scala.scalajs.js
import scala.scalajs.js.Any.{ fromFunction4, wrapArray }
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr.undefOr2ops
import org.scalajs.dom.Element
import com.greencatsoft.angularjs.core.Scoped
import scala.scalajs.js.annotation.JSBracketAccess

trait Directive extends NamedTarget with ConfigurableTarget[js.Dictionary[js.Any]]
  with Scoped with Configurable {

  type ControllerType <: Controller

  val controller: Option[ControllerType] = None

  override def initialize(): Unit = Unit

  override def apply(): js.Dictionary[js.Any] = buildConfig()

  override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    def bind(scope: ScopeType): ScopeType = {
      scope.dynamic.directive = this.asInstanceOf[js.Object]
      scope
    }

    config("link") = (scope: ScopeType, elems: js.Array[Element], attrs: Attributes, controllers: UndefOr[js.Any]) => {
      controllers.toOption match {
        case Some(arr) if js.Array.isArray(arr) =>
          val args = arr.asInstanceOf[js.Array[js.Any]].toSeq.map(Module.unbindTarget[Controller](_)).flatten
          link(bind(scope), elems, attrs, args: _*)
        case Some(c) =>
          Module.unbindTarget[Controller](c) match {
            case Some(arg) => link(bind(scope), elems, attrs, arg)
            case _ => link(bind(scope), elems, attrs)
          }
        case None => link(bind(scope), elems, attrs)
      }
    }

//    def inject(controller: ControllerType) = macro Controller.inject[ControllerType]
//
//    controller.foreach {
//      c => Controller.inject[ControllerType]
//    }
//    controller foreach { c =>
//      Module.asService(c) { args: js.Array[js.Any] => config("controller") = args }
//    }

    super.buildConfig(config)
  }

  def link(scope: ScopeType, elems: Seq[Element], attrs: Attributes, controller: Controller*): Unit = Unit
}

protected[angularjs] object Directive extends InjectionMacro[Directive] {

  override def inject[T <: Directive](c: Context)(target: c.Expr[T])(implicit tag: c.WeakTypeTag[T]): c.Expr[ModuleProxy] =
    super.inject[T](c)(target)

  override def register[T <: Directive](c: Context)(target: c.Expr[T]): c.universe.Tree = {
    import c.universe._

    val name = Select(target.tree, TermName("name"))
    q"${c.prefix.tree}.module.directive($name, dependencies)"
  }
}

trait Attributes extends js.Object {

  val $attr: js.Dictionary[String] = ???

  def $addClass(classVal: String): Unit = ???

  def $removeClass(classVal: String): Unit = ???

  def $updateClass(newClasses: String, oldClasses: String): Unit = ???

  @JSBracketAccess
  def apply(name: String): UndefOr[String] = ???

  @JSBracketAccess
  def update(name: String, value: String): Unit = ???

  def $get(name: String): UndefOr[String] = ???

  def $set(name: String, value: String): Unit = ???

  def $observe(key: String, fn: js.Function1[String, Unit]): Unit = ???
}

trait Requires extends Configurable {
  this: Directive =>

  var requirements = Set.empty[Requirement]

  abstract override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    config("require") = js.Array[String](requirements.toSeq.map(_.toString): _*)

    super.buildConfig(config)
  }

  case class Requirement(name: String, lookup: Boolean, optional: Boolean = false) {

    override def toString = (if (lookup) "^" else "") + (if (optional) "?" else "") + name
  }

  def ^(requirement: NamedTarget) = new Requirement(requirement.name, true)

  def ^?(requirement: NamedTarget) = new Requirement(requirement.name, true, true)

  def ?(requirement: NamedTarget) = new Requirement(requirement.name, false, true)

  implicit def ~(requirement: NamedTarget) = new Requirement(requirement.name, false)
}

trait RestrictedDirective extends Directive with Configurable {

  def restrict: Set[String] = Set.empty

  abstract override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    config("restrict") = restrict.mkString

    super.buildConfig(config)
  }
}

trait ElementDirective extends RestrictedDirective {

  override def restrict = super.restrict + "E"

  var transclude = false

  var replace = false

  abstract override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    config("transclude") = transclude
    config("replace") = replace

    super.buildConfig(config)
  }
}

trait AttributeDirective extends RestrictedDirective {

  override def restrict = super.restrict + "A"
}

trait ClassDirective extends RestrictedDirective {

  override def restrict = super.restrict + "C"
}

trait CommentDirective extends RestrictedDirective {

  override def restrict = super.restrict + "M"
}

trait ScopeStrategy extends Configurable {
  this: Directive =>
}

trait InheritParentScope extends ScopeStrategy {
  this: Directive =>

  override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    config("scope") = true

    super.buildConfig(config)
  }
}

trait UseParentScope extends ScopeStrategy {
  this: Directive =>

  override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    config("scope") = false

    super.buildConfig(config)
  }
}

trait IsolatedScope extends ScopeStrategy {
  this: Directive =>

  var bindings = Seq.empty[ScopeBinding]

  override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    val dict = js.Dictionary.empty[String]

    bindings foreach { b =>
      dict(b.name) = s"${b.prefix}${b.attribute}"
    }

    config("scope") = dict

    super.buildConfig(config)
  }

  abstract class ScopeBinding(val prefix: String) {

    val name: String

    val attribute: String
  }

  case class UnidirectionalBinding(name: String, attribute: String = "") extends ScopeBinding("=")

  case class BidirectionalBinding(name: String, attribute: String = "") extends ScopeBinding("@")

  case class BehavioralBinding(name: String, attribute: String = "") extends ScopeBinding("&")

  implicit class BindingBuilder(name: String) {

    def :=(attribute: String = ""): ScopeBinding = UnidirectionalBinding(name, attribute)

    def :@(attribute: String = ""): ScopeBinding = BidirectionalBinding(name, attribute)

    def :&(attribute: String = ""): ScopeBinding = BehavioralBinding(name, attribute)
  }
}

trait TemplateProvider extends Configurable {
  this: Directive =>

  val template: String

  def getTemplate(elems: Seq[Element], attrs: Attributes): String = template

  abstract override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    config("template") = (elems: js.Array[Element], attrs: Attributes) => getTemplate(elems, attrs)

    super.buildConfig(config)
  }
}

trait TemplateUrlProvider extends Configurable {
  this: Directive =>

  val templateUrl: String

  def getTemplateUrl(elems: Seq[Element], attrs: Attributes): String = templateUrl

  abstract override def buildConfig(config: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
    config("templateUrl") = (elems: js.Array[Element], attrs: Attributes) => getTemplateUrl(elems, attrs)

    super.buildConfig(config)
  }
}

