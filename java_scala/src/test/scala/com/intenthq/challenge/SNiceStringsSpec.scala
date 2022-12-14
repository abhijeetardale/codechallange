package com.intenthq.challenge

import org.specs2.mutable.Specification

class SNiceStringsSpec extends Specification {

  section("scala")
  section("nice")
  "SNiceStrings" should {
    "ugknbfddgicrmopn is nice" in {
      SNiceStrings.nice(List("ugknbfddgicrmopn")) must_== 1
    }
    "aaa is nice" in {
      SNiceStrings.nice(List("aaa")) must_== 1
    }
    "jchzalrnumimnmhp is naughty" in {
      SNiceStrings.nice(List("jchzalrnumimnmhp")) must_== 0
    }
    "haegwjzuvuyypxyu is naughty" in {
      SNiceStrings.nice(List("haegwjzuvuyypxyu")) must_== 0
    }
    "dvszwmarrgswjxmb is naughty" in {
      SNiceStrings.nice(List("dvszwmarrgswjxmb")) must_== 0
    }
  }
  section("nice")
  section("scala")
  "SNiceStrings with more elements" should {
    "ugknbfddgicrmopn, aaa are nice" in {
      SNiceStrings.nice(List("ugknbfddgicrmopn", "aaa")) must_== 2
    }
    "jchzalrnumimnmhp, haegwjzuvuyypxyu, dvszwmarrgswjxmb are naughty" in {
      SNiceStrings.nice(List("jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb")) must_== 0
    }
    "ugknbfddgicrmopn, aaa are nice and jchzalrnumimnmhp, haegwjzuvuyypxyu, dvszwmarrgswjxmb are naughty" in {
      SNiceStrings.nice(List("ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb")) must_== 2
    }
  }
}