package com.greencatsoft.angularjs

import scala.scalajs.js

trait Injected extends Initializable with Service {

  def run(): js.Any = {
    null
  }

}

case class InjectedTransformed(go: js.Any)

object InjectedImplicits {

  import scala.language.experimental.macros

  import internal.{ Angular => angular }

  implicit def inject[A <: Injected](target: A): InjectedTransformed = macro angular.injectImpl[A]

}


