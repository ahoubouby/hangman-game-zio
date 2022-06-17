package com.ahoubouby.models

sealed trait GuessResult

object GuessResult {
  case object Won       extends GuessResult
  case object Lost      extends GuessResult
  case object Correct   extends GuessResult
  case object Incorrect extends GuessResult
  case object Unchanged extends GuessResult
}
