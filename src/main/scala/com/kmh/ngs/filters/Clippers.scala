package com.kmh.ngs.filters
import com.kmh.ngs.formats.{Read, FastqRecord}
import scala.util.matching.Regex

/**
 * Object for clipping functions with fastq records 
 */
object ReadClippers { 

  type OptionMap = Map[Symbol, Any]

  lazy val iupac = Map[Char, String](
    'R'->"[AG]", 'Y'->"[CT]", 'S'->"[GC]", 'W'->"[AT]",
    'K'->"[GT]", 'M'->"[AC]", 'B'->"[CGT]", 'D'->"[AGT]",
    'H'->"[ACT]", 'V'->"[ACG]", 'N'->"[NATGC]")

  /**
   * Trim single-end reads by index and return a copy of the read
   */
  def trimSE(read: FastqRecord, start: Option[Int], end: Option[Int]): FastqRecord =
    (start, end) match {
      case (Some(st), Some(en)) => read.copy(sequence=read.sequence.slice(st-1, en),
                                             quality=read.quality.slice(st-1, en))
      case (Some(st), None) => read.copy(sequence=read.sequence.slice(st-1, read.sequence.length),
                                             quality=read.quality.slice(st-1, read.sequence.length))
      case (None, Some(en)) => read.copy(sequence=read.sequence.slice(0, en),
                                             quality=read.quality.slice(0, en))
      case (None, None) => read
    }

  /**
   * Clip single-end reads by search strings
   */
  def clipSE(read: FastqRecord, lead: Option[String], tail: Option[String], 
             keepLead: Boolean, keepTail: Boolean): Option[FastqRecord] = 
    (lead, tail) match {
      case (Some(ld), Some(tl)) =>
        // Define lead and tail regexes
        lazy val regLead = 
          new Regex("^(" + ld.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
        lazy val regTail = 
          new Regex("(" + tl.map(x => iupac.getOrElse(x,x)).mkString + ")")
        // First search for the lead
        (regLead findFirstMatchIn read.sequence) map (m => (m group "rest", m.start(2))) match {
          case Some(foundLead) =>
            // Now search for the tail within the newly clipped read 
            (regTail findFirstMatchIn foundLead._1) map (n => n.start) match {
              case Some(foundTail) => Some(read.copy(sequence = foundLead._1.take(foundTail), 
                quality = read.quality.slice(foundLead._2, foundLead._2 + foundTail)))
              case None => 
                if (keepTail) 
                  Some(read.copy(sequence = foundLead._1, 
                    quality = read.quality.slice(foundLead._2, read.quality.length)))
                else None
            }
          case None => 
            if (keepLead) 
              (regTail findFirstMatchIn read.sequence) map (n => n.start) match {
                case Some(foundTail) => Some(read.copy(sequence = read.sequence.take(foundTail),
                  quality = read.quality.take(foundTail)))
                case None => if (keepTail) Some(read) else None
              } 
            else None
        }

      case (Some(ld), None) =>
        lazy val regLead = 
          new Regex("^(" + ld.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
        (regLead findFirstMatchIn read.sequence) map (m => (m group "rest", m.start(2))) match {
          case Some(foundLead) => Some(read.copy(sequence = foundLead._1,
            quality = read.quality.slice(foundLead._2, read.quality.length)))
          case None => if (keepLead) Some(read) else None
        }

      case (None, Some(tl)) =>
        lazy val regTail = 
          new Regex("(" + tl.map(x => iupac.getOrElse(x,x)).mkString + ")")
        (regTail findFirstMatchIn read.sequence) map (n => n.start) match {
          case Some(foundTail) => Some(read.copy(sequence = read.sequence.take(foundTail),
            quality = read.quality.take(foundTail)))
          case None => if (keepTail) Some(read) else None
        }
      // Shouldn't happen 
      case (None, None) => Some(read) 
  }

  def apply(read: Read, userOpts: OptionMap): Option[Read] = read match { 
    case fq: FastqRecord =>
      if (userOpts.isDefinedAt('start) || userOpts.isDefinedAt('end))
        Some(trimSE(fq, userOpts.get('start).asInstanceOf[Option[Int]], 
                    userOpts.get('end).asInstanceOf[Option[Int]]))
      else 
        clipSE(fq, userOpts.get('clipLead).asInstanceOf[Option[String]], 
               userOpts.get('clipTail).asInstanceOf[Option[String]],
               userOpts.isDefinedAt('keepLead), userOpts.isDefinedAt('keepTail))
  }
}
