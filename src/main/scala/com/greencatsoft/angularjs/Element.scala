package com.greencatsoft.angularjs

import scala.language.implicitConversions
import scala.scalajs.js

import org.scalajs.dom.Event

trait Element extends org.scalajs.dom.Element {

  def bind[T <: Event](event: String, handler: js.Function1[T, _]): Unit = ???

  def unbind[T <: Event](event: String, handler: js.Function1[T, _]): Unit = ???
}