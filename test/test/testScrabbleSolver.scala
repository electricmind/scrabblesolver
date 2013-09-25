package test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scrabblesolver._

class testScrabbleSolver extends FlatSpec with Matchers {

    scrabblesolver.map1.clear()
    
    "A scrabblesolver" should "make keys" in {
        scrabblesolver.string2Keys("oppression".split("").tail) should
            be(List(("e", 1), ("s", 2), ("n", 1), ("i", 1), ("p", 2), ("r", 1), ("o", 2)))
    }

    "A scrabblesolver" should "make all keys" in {
        scrabblesolver.string2AllKeys("oppression".split("").tail) should
            be(List(("e", 1), ("s", 1), ("s", 2), ("n", 1), ("i", 1), ("p", 1), ("p", 2), ("r", 1), ("o", 1), ("o", 2)))
    }

    "A String" should "be splitted" in {
        "oppression".split("").tail should be(Array("o", "p", "p", "r", "e", "s", "s", "i", "o", "n"))
    }

    "A scrabblesolver" should "exist" in {
        scrabblesolver.map1 should be(Map())
    }

    "A map" should "be generated" in {
        scrabblesolver.collect(("an extrem oppression and bloody aggression").
            split(" ").toIterator)
        scrabblesolver.map1(("p", 2)).toList should be(List("oppression"))
        scrabblesolver.lookup("an") should be(List("an", "and"))
        scrabblesolver.lookup("and") should be(List("an", "and"))
//        scrabblesolver.lookup("anxtrem") should be(List("an", "and", "extrem"))
    }

    "A group" should "group a list of words" in {
        scrabblesolver.group(
            List("domes", "dies", "homids", "homid"), "wisdom") should be(
                List(
                    ("e", List("domes", "dies")),
                    ("h", List("homids", "homid"))))
        scrabblesolver.group(
            List("domes", "dies", "homids", "homid"), "wisom") should be(
                List(
                    ("de", List("domes", "dies")),
                    ("dh", List("homids", "homid"))))
    }
}
