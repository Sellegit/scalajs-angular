package com.greencatsoft.angularjs.core

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.UndefOr.any2undefOrA

import com.greencatsoft.angularjs.{ injectable, NamedTarget, TemplateController }

@injectable("$routeProvider")
trait RouteProvider extends js.Object {

  def when(path: String, route: Route): this.type = ???

  def otherwise(route: Route): this.type = ???
}

trait Route extends js.Object {

  var title: UndefOr[String]

  var templateUrl: UndefOr[String]

  var controller: UndefOr[String]

  var redirectTo: UndefOr[String]
}

object Route {

  def apply(controller: TemplateController): Route = {
    require(controller != null, "Missing argument 'controller'.")
    apply(controller.templateUrl, controller.title, Some(controller))
  }

  def apply(templateUrl: String, title: Option[String], controller: Option[NamedTarget]): Route = {
    require(templateUrl != null, "Missing argument 'templateUrl'.")
    require(title != null, "Missing argument 'title'.")
    require(controller != null, "Missing argument 'controller'.")

    var route = new js.Object().asInstanceOf[Route]

    route.templateUrl = templateUrl

    title.foreach(t => route.title = t)
    controller.foreach(c => route.controller = c.name)

    route
  }

  def apply(redirectTo: String) = {
    require(redirectTo != null, "Missing argument 'redirectTo'.")

    var route = new js.Object().asInstanceOf[Route]
    route.redirectTo = redirectTo

    route
  }
}

trait RouteInfo extends js.Object {

  var $$route: Route

  var loadedTemplateUrl: String

  var params: js.Array[js.Any]

  var pathParams: js.Array[js.Any]

  var scope: Scope
}
