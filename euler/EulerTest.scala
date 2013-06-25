package net.egp.euler
import org.scalatest.FlatSpec
import EulerTest._
class EulerTest extends FlatSpec {
  def p92(n: Int): Int = {
    (1 until n).count(endsAt89(_))
  }
  "p92(10)" should "=7" in assert(p92(10) === 7)
/*  "p92(100)" should "=80" in assert(p92(100) === 80)
  "p92(1000)" should "=857" in assert(p92(1000) === 857)
  "p92(10000)" should "=8558" in assert(p92(10000) === 8558)
  "p92(100000)" should "=85623" in assert(p92(100000) === 85623)
  "p92(1000000)" should "=856929" in assert(p92(1000000) === 856929)*/
  "p92(10000000)" should "=8581146" in assert(p92(10000000) === 8581146)
  /*  assert(nextTerm(1)===1)
  assert(nextTerm(2)===4)
  assert(nextTerm(3)===9)
  assert(nextTerm(4)===16)*/
}
object EulerTest {
  def sqdig(c: Char): Int = {
    val n = c - '0'
    //println(c + "-->" + n * n)
    n * n
  }
  def nextTerm(n: Int): Int = {
    n.toString.foldLeft(0)(_ + sqdig(_))
  }
  def endsAt89(n: Int): Boolean = n match {
    case 89 => true
    case 1 => false
    case _ => endsAt89(nextTerm(n))
  }
}