package runtime
import com.sun.org.apache.xpath.internal.operations.And

import scala.collection.immutable.::
import scala.io.Source

import logic.dsp_log
import logic.dgn_log

object weather_app extends App {

  val actionMap = Map[Int, () => Boolean](1 -> handleOne, 2 -> handleTwo, 3 -> handleThree,
    4 -> handleFour, 5 -> handleFive, 6 -> handleSix)

  def handleOne(): Boolean = {
    dsp_log.disp_recent_temps()
    true
  }

  def handleTwo(): Boolean = {
    dsp_log.disp_min_max_difs()
    true
  }

  def handleThree(): Boolean = {
    dsp_log.disp_mean_difs()
    true
  }

  def handleFour(): Boolean = {
    dsp_log.disp_lrgst_difs()
    true
  }

  def handleFive(): Boolean = {
    dsp_log.disp_sumry()
    true
  }

  def handleSix(): Boolean = {
    println("Goodbye!")
    false
  }

  var opt = 0
  do {
    try {
      opt = readOption
    }
    catch {
      case _: Exception => opt = 0
    }
  } while (menu(opt))


  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  def readOption: Int = {
    println(
      """|Select one of the following:
         |  1 - show most recent temps
         |  2 - show differences between max and min temps
         |  3 - Show mean temp differences
         |  4 - Show largest temp differences
         |  5 - Generate city temps summary
         |  6 - Quit""".stripMargin)
    readInt()
  }
}