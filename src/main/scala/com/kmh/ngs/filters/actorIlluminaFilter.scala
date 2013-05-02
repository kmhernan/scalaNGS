package com.kmh.ngs.multiThread
import scala.actors._

object IlluminaFilter extends Actor {
  import com.kmh.ngs.io.IoUtil
  import com.kmh.ngs.readers._

  def act() {
    react {
      case (rec: FastqRecord, actor: Actor) =>
        actor ! runTest(rec)
        act()
      case "EXIT" =>
        println("IlluminaFilter Exiting.")
