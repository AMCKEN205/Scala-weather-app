package logic

object dsp_log {

  val w_data_mrch14 = dgn_log.gen_w_data("weatherdata14March.txt")

  w_data_mrch14.toList match {
    case data: List[(String,List[(Int,Int)])] if data.head._2.isEmpty => handle_err(data.head._1)
    case _ => //do nothing
  }

  def handle_err(err_out : String) ={
    println(err_out)
    System.exit(0)
  }

  def disp_recent_temps(w_data: Map[String, List[(Int, Int)]] = w_data_mrch14) = {
    val most_recents = dgn_log.build_most_recents_map(w_data)
    for ((city, temps) <- most_recents) {
      println(s"City: ${city} - Most recent temps: Min: ${temps._1} Max: ${temps._2}")
    }
  }

  def disp_min_max_difs(w_data: Map[String, List[(Int, Int)]] = w_data_mrch14) = {
    val min_max_map = dgn_log.build_difs_map(w_data)

    for ((city, min_max_difs) <- min_max_map) {
      var i = 0
      val min_max_strings = for(min_max_dif <- min_max_difs) yield {
        i += 1
        s"Y${i}: " + min_max_dif
      }
      println(s"City: ${city} - min max differences: ${min_max_strings.mkString(" ")}\n")
    }
  }

  def disp_mean_difs(w_data: Map[String, List[(Int, Int)]] = w_data_mrch14) = {
    val mean_map = dgn_log.get_mean_difs(w_data)

    for ((city, mean) <- mean_map) {
      println(s"City: ${city} - Mean min max differences: ${mean.setScale(2, BigDecimal.RoundingMode.HALF_UP)}\n")
    }
  }

  def disp_lrgst_difs(w_data: Map[String, List[(Int, Int)]] = w_data_mrch14) = {
    val difs_map = dgn_log.build_difs_map(w_data)

    for ((city, difs) <- difs_map){
      println(s"City: ${city} - Largest difference: ${dgn_log.get_lrgst(difs)}")
    }
  }

  def disp_sumry(w_data: Map[String, List[(Int, Int)]] = w_data_mrch14) = {
    val u_dsts = dgn_log.build_udests_sum(w_data)
    val sumry = u_dsts.filter(_._2.isDefined)
    val invld_dsts = u_dsts.filter(_._2.isEmpty)
    val min_max_get = sumry.values.toList.map(_.get)

    val min_max_av = min_max_get match {
      case Nil => (null,null)
      case _ => dgn_log.min_max_av(min_max_get)
    }

    min_max_av match {
      case (null,null) => println("\n\nNo values were entered!\n")
      case _ => println(s"\n\nUser destinations summary\n${dgn_log.gen_sumry_out(sumry.toList)}\n\n" +
      s"Destinations entered temp averages - Min: ${min_max_av._1.setScale(2, BigDecimal.RoundingMode.HALF_UP)}" +
      s" - Max: ${min_max_av._2.setScale(2, BigDecimal.RoundingMode.HALF_UP)}" )
    }

    disp_invld_dsts(invld_dsts.size, invld_dsts.keys.toList)
    println("\n")

  }

  def disp_invld_dsts(num_invlds: Int, dsts: List[String]) = num_invlds match {
    case sz: Int if sz > 0 => { println(s"You entered ${num_invlds} invalid destination(s), these were:")
      dsts.foreach(d => println(s"  ${d}")) }
    case _ => println("No invalid values input!")
  }
}