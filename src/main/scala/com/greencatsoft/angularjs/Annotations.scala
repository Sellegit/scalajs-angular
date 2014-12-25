package com.greencatsoft.angularjs

import scala.annotation.StaticAnnotation

class inject extends StaticAnnotation

class injectable(name: String) extends StaticAnnotation

class resolve(name: String) extends StaticAnnotation

