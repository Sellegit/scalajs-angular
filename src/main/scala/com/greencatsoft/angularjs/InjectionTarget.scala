package com.greencatsoft.angularjs

trait InjectionTarget {

  def initialize(): Unit
}

trait ConfigurableTarget[A] extends InjectionTarget with Function0[A]
