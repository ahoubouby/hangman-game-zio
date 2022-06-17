package com.ahoubouby.models

sealed abstract case class Word private (word: String) {
  def contains(char: Char) = word.contains(char)
  val length: Int          = word.length
  def toList: List[Char]   = word.toList
  def toSet: Set[Char]     = word.toSet
}
object Word {
  def make(word: String): Option[Word] =
    if (word.nonEmpty && word.forall(_.isLetter)) Some(new Word(word.toLowerCase) {})
    else None
}
