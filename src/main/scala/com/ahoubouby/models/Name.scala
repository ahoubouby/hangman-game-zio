package com.ahoubouby.models

sealed abstract case class Name private (name: String)
object Name {
  def make(name: String): Option[Name] = if (name.nonEmpty) Some(new Name(name) {}) else None
}
