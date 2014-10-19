package com.greencatsoft.angularjs

import scala.scalajs.js

trait Configurable {

  def buildConfig(config: js.Dictionary[js.Any] = js.Dictionary.empty): js.Dictionary[js.Any] = config
}
