package com.greencatsoft.angularjs

import scala.language.implicitConversions
import scala.scalajs.js

import org.scalajs.dom.Element

trait Angular extends js.Object {

  def module(name: String): Module = ???

  def module(name: String, require: js.Array[String]): Module = ???

  implicit def element(elem: Element): AngularElement
}
