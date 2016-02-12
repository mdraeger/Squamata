package org.draegisoft.math.field

trait Field[A] { self: A =>
  def +(that: A): A
  def -(that: A): A
  def *(that: A): A
  def /(that: A): A
  def unary_- : A
  def inv : A

  def zero: A
  def one: A
}
