#!/usr/bin/env amm
// -*- mode: scala; -*-

// wordle clone for console.
// usage: ./wordleClone.sc
//        ./wordleClone.sc --date 2022-03-13 --hardMode true

// circe
import $ivy.`io.circe::circe-generic:0.13.0`, io.circe._, io.circe.generic.auto._, io.circe.syntax._, io.circe.generic.JsonCodec
import $ivy.`io.circe::circe-generic-extras:0.13.0`, io.circe.generic.extras._, io.circe.generic.extras.semiauto._
import $ivy.`io.circe::circe-parser:0.13.0`, io.circe.parser._
import $ivy.`io.circe::circe-optics:0.13.0`, io.circe.optics.JsonPath._

import scala.io.AnsiColor
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val yyyyMMddFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

def removeFirst[A](xs: List[A])(x: A): List[A] = xs.patch(xs.indexOf(x), Nil, 1)
// alternatively: xs.zipWithIndex.filterNot(_._2 == xs.indexOf(x)).map(_._1)

@main
def runWordle(hardMode: Boolean = false, date: String = yyyyMMddFormatter.format(LocalDate.now())) = {
  // snapshot from embedded JS on https://www.nytimes.com/games/wordle/index.html taken on 2022-03-13.
  // targets start on 2021-06-19.
  val wordleSnapshot = scala.io.Source.fromFile("wordle.json").mkString
  val today = LocalDate.parse(date, yyyyMMddFormatter)

  case class Wordle(targets: List[String], valids: List[String])

  val wordle = decode[Wordle](wordleSnapshot).toOption.get

  val trueValids = wordle.targets ++ wordle.valids

  val targetsStartingDate = LocalDate.parse("2021-06-19", yyyyMMddFormatter)
  val targetDates = wordle.targets.indices.map(x => targetsStartingDate.plusDays(x))
  val dateToTarget = (wordle.targets zip targetDates).map(x => x._2 -> x._1).toMap

  val targetWord = dateToTarget(today)

  def additionalHardModeCheck(guess: String, previousGuess: String, target: String) = {
    val (greens, yellows, grays) = greensYellowsAndGrays(previousGuess, target)
    val greensUsed = greens.map(idx => (guess(idx), previousGuess(idx))).filterNot(x => x._1 == x._2).isEmpty
    val yellowsUsed = (guess.zipWithIndex).filterNot(x => greens.contains(x._2)).map(_._1).foldLeft(yellows.map(target.apply).toList){(unusedYellows, x) => if (unusedYellows.contains(x)) removeFirst(unusedYellows)(x) else unusedYellows}.isEmpty
    greensUsed && yellowsUsed
  }

  // todo: break this up and have validateWord return an ADT to indicate failure type to print more informative error message (including missing letters / their required positions in hard mode)
  def validateWord(guess: String, target: String, previousGuess: String, hardMode: Boolean) = !guess.toList.exists(c => !c.isLetter || c > 'z' || c.isUpper) && (guess.size == target.size) && trueValids.contains(guess) && (!hardMode || additionalHardModeCheck(guess, previousGuess, target))

  // indexes of colors
  def greensYellowsAndGrays(targetWord: String, guessedWord: String) = {
    val greens = (targetWord zip guessedWord).zipWithIndex.filter(x => x._1._1 == x._1._2).map(_._2)
    val yellows = guessedWord.toList.indices.filterNot(greens.contains).foldLeft((targetWord.toList.zipWithIndex.filterNot(x_idx => greens.contains(x_idx._2)).map(_._1), List.empty[Int]))((acc, x) => if (acc._1.contains(guessedWord(x))) (removeFirst(acc._1)(guessedWord(x)), acc._2 :+ x) else acc)._2
    val greys = guessedWord.indices.filterNot((greens ++ yellows).contains)
    (greens, yellows, greys)
  }

  println(s"Date: ${today}.")
  if (hardMode) println("Hard mode.")
  // todo: just make this a fold/scan
  var attemptNumber = 0
  var success = false
  var previousGuess = ""
  var guesses = Vector.empty[String]
  while (attemptNumber < 6 && !success) {
    val guessedWord = Option(scala.io.StdIn.readLine()).getOrElse("").trim.toLowerCase
    if (validateWord(guessedWord, targetWord, previousGuess, hardMode)) {
      val (greens, yellows, greys) = greensYellowsAndGrays(targetWord, guessedWord)
      attemptNumber = attemptNumber + 1
      println(attemptNumber + " " + (0 until guessedWord.size).map(x => if (greens.contains(x)) s"${AnsiColor.GREEN}${guessedWord(x)}${AnsiColor.RESET}" else if (yellows.contains(x)) s"${AnsiColor.YELLOW}${guessedWord(x)}${AnsiColor.RESET}" else if (greys.contains(x)) s"${AnsiColor.RED}${guessedWord(x)}${AnsiColor.RESET}" else "_").mkString)
      previousGuess = guessedWord
      guesses = guesses :+ guessedWord
      if (guessedWord == targetWord) success = true
    } else {
      println("Invalid word.")
    }
  }

  if (success) println("Congratulations! You guessed the target word.") else println("Out of guesses! The target word was " + targetWord + ".")

  println()
  println("To share, copy:")
  println(s"Date: ${today}" + (if (hardMode) ", hard mode." else "."))
  println(guesses.map{guessedWord => val (greens, yellows, greys) = greensYellowsAndGrays(targetWord, guessedWord); (0 until guessedWord.size).map(x => if (greens.contains(x)) "🟩" else if (yellows.contains(x)) "🟨" else if (greys.contains(x)) "🟥" else "_").mkString}.mkString("\n"))
}

