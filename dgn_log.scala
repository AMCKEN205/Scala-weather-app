package logic

import scala.collection.immutable.::
import scala.io.Source

object dgn_log {

  def gen_w_data(fileName: String): Map[String, List[(Int, Int)]] = {
  try{
    val map  = for (line <- Source.fromFile(fileName).getLines()) yield {

      val map_item = line.split(",").map(_.trim).toList

      (map_item.head, get_int_tups(map_item.tail))
    }
    map.toMap
  }
  catch{
    case _ : Exception => Map("error can't load weather data...\nexiting application..." -> Nil)
    }
  }

  def build_most_recents_map(w_data: Map[String, List[(Int, Int)]]): Map[String, (Int, Int)] = {
    val most_recents = for ((city, temps) <- w_data) yield {
      (city, temps.last)
    }
    most_recents
  }

  def get_mean_difs(w_data: Map[String, List[(Int, Int)]]): Map[String, BigDecimal] = {

    val min_max_map = build_difs_map(w_data)

    val means = for((city, min_max_difs) <- min_max_map) yield {
      (city, BigDecimal(min_max_difs.sum/(min_max_difs.length).toDouble))
    }
    means
  }

  def get_lrgst(vals: List[Int]): Int = vals match {
    case h :: Nil => h
    case h :: th :: t => get_lrgst((if (h < th) th else h) :: t)
  }

  def get_int_tups(vals: List[String]): List[(Int, Int)] = {
    val tup_lst = vals.foldLeft(List[(Int, Int)]()) { (tup_lst_in, cur_val) =>

      val tup = cur_val.split(":").map(_.trim).toList

      tup_lst_in :+ (tup.head.toInt, tup.tail.head.toInt)
    }
    tup_lst
  }

  def build_difs_map(w_data: Map[String, List[(Int, Int)]]): Map[String, List[Int]] = {

    val difs = for ((city, temps) <- w_data) yield {
      (city, min_max_dif(temps))
    }
    difs
  }

  def min_max_dif(temps: List[(Int, Int)], difs: List[Int] = List[Int]()): List[Int] = temps match {
    case Nil => difs
    case h :: t => min_max_dif(t, difs :+ (h._2 - h._1))
  }

  def build_udests_sum(w_data: Map[String, List[(Int, Int)]]): Map[String, (Option[(Int,Int)])] = {
    val mst_recents = build_most_recents_map(w_data)
    val udests = get_usr_in("enter a destination to add to the destinations list, or enter quit to exit: ")
    val dests_lst = for((dest) <- udests) yield {
      (dest, mst_recents.get(dest))
    }
    dests_lst.toMap
  }

  def min_max_av(temps: List[(Int, Int)]): (BigDecimal, BigDecimal) = {
    val lst_sz = temps.length
    val min_max = ovrl_min_max(temps)
    (BigDecimal(min_max._1/lst_sz.toDouble), BigDecimal(min_max._2/lst_sz.toDouble))
  }

  def ovrl_min_max(temps: List[(Int, Int)], max_min : List[Int] = List[Int](0, 0)): (Int, Int) = temps match {
    case Nil => (max_min.head, max_min.tail.head)
    case h :: t => ovrl_min_max(t, List[Int]((max_min.head + h._1), (max_min.tail.head + h._2)))
  }

  def get_usr_in(u_prompt: String, inpt_vals: List[String] = List[String]()):List[String] =
    scala.io.StdIn.readLine(u_prompt) match {
    case "quit" => inpt_vals
    case "" => get_usr_in(u_prompt, inpt_vals)
    case inpt => get_usr_in(u_prompt, inpt_vals :+ inpt)

  }

  def gen_sumry_out(sumry: List[(String, Option[(Int,Int)])], out_build: String = ""): String = sumry match {
    case Nil => out_build
    case h :: t => gen_sumry_out(
      t, out_build +
        "\nDestination: " + h._1 + "\n" +
      "Last year temps:\n" +
      "   Min: " + h._2.get._1 + "\n" +
      "   Max: " + h._2.get._2 + "\n")
  }
}