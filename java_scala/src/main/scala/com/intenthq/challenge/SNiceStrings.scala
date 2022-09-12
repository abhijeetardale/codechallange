package com.intenthq.challenge

import scala.annotation.tailrec
import scala.collection.immutable.Set

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  def nice(strings: List[String]): Int = {
    strings.foldLeft(0){(count, str) =>
      if(processRules(str)) count+1 else count
      //if(applyRules(str.toList, None, List(), rule2 = false, rule3 = true)) count+1 else count
    }
  }

  private def processRules(inputString: String) : Boolean = {

    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val rule1 = inputString.count(char => vowels.contains(char)) >= 3
    //val rule1 = "[aeiou]".r.findAllIn(inputString).length >= 3

    @tailrec
    def applyRule2(str: String, isValid: Boolean): Boolean =
      if (str.isEmpty || isValid) isValid
      else {
        val (segment, rest) = str.span(_ == str.head)
        applyRule2(rest, segment.length > 1)
      }

    val rule2 = applyRule2(inputString, isValid = false)

    val rule3 = "ab|cd|pq|xy".r.findAllIn(inputString).isEmpty

        println(
          s""" rule1: $rule1
             |rule2: $rule2
             |rule3: $rule3
             |""".stripMargin)

    rule1 && rule2 && rule3
  }



/*private def processRules(inputString: String) : Boolean = {

  // val rule1 = 3 <= inputString.count(_ == 'a') + inputString.count(_ == 'e') + inputString.count(_ == 'i') + inputString.count(_ == 'o') + inputString.count(_ == 'u')

      @tailrec
      def applyRule1(currentList:List[Char], last:Option[Char], rule1: List[Char]): Boolean ={
        (currentList.headOption, last) match {
          case (Some(cur), Some(_)) if List('a', 'e', 'i', 'o', 'u').contains(cur) =>
            applyRule1(currentList.tail, Some(cur), (rule1 :+ cur))
          case (Some(cur), None) if List('a', 'e', 'i', 'o', 'u').contains(cur) =>
            applyRule1(currentList.tail, Some(cur), (rule1 :+ cur))
          case (Some(cur), _)  =>
            applyRule1(currentList.tail, Some(cur), rule1)
          case _ => rule1.length >= 3
        }
      }

  val rule1 = applyRule1(inputString.toList, None, List[Char]())

  val rule2 = inputString.foldLeft("") {
    (last, current) => if (last == current.toString) last + current else if (last.length == 2) last else current.toString
  }.length == 2

  val rule3 = inputString.foldLeft((true, "")) { case ((result, last), current) =>
    last + current match {
      case "ab" | "cd" | "pq" | "xy" => (false, "")
      case _ if !result => (result, "")
      case _ => (true, current.toString)
    }
  }._1

      println(
        s""" rule1: $rule1
           |rule2: $rule2
           |rule3: $rule3
           |""".stripMargin)

  rule1 && rule2 && rule3
}*/

/*  @tailrec
  def applyRules(currentList:List[Char], last:Option[Char], rule1: List[Char], rule2: Boolean, rule3: Boolean): Boolean ={
    (currentList.headOption, last) match {
      case (Some(cur), Some(lst)) if List('a', 'e', 'i', 'o', 'u').contains(cur) && cur==lst =>
        applyRules(currentList.tail, Some(cur), (rule1 :+ cur), rule2 = true, rule3)
      case (Some(cur), Some(lst)) if cur==lst =>
        applyRules(currentList.tail, Some(cur), rule1, rule2 = true, rule3)
      case (Some(cur), Some(lst)) if List("ab","cd","pq","xy").contains(s"$lst$cur") =>
        applyRules(currentList.tail, Some(cur), rule1, rule2, rule3 = false)
      case (Some(cur), _) if List('a', 'e', 'i', 'o', 'u').contains(cur) =>
        applyRules(currentList.tail, Some(cur), (rule1 :+ cur), rule2, rule3)
      case (Some(cur), _)  =>
        applyRules(currentList.tail, Some(cur), rule1, rule2, rule3)
      case _ =>
        println(
          s""" rule1: $rule1
             |rule2: $rule2
             |rule3: $rule3
             |""".stripMargin)
        rule1.length >= 3 && rule2 && rule3
    }
  }*/
}
