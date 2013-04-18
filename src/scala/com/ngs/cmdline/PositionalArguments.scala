package com.ngs.cmdline

import java.lang.annotation.Documented
import java.lang.annotation.ElementType
import java.lang.annotation.Retention
import java.lang.annotation.RetentionPolicy
import java.lang.annotation.Target

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Documented

trait PositionalArguments {
  def minElements(): Int
  def maxElements(): Int
}
