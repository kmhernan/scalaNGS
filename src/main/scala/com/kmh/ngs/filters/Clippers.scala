package com.kmh.ngs.filters
import com.kmh.ngs.formats.{Read, FastqRecord, PEFastqRecord}
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
   * Trim reads by index and return a copy of the read
   */
  def trim(read: Read, opt: OptionMap): Read = read match {
    case fq: FastqRecord =>
      (opt.get('start).asInstanceOf[Option[Int]], opt.get('end).asInstanceOf[Option[Int]]) match {
        case (Some(st), Some(en)) => fq.copy(sequence=fq.sequence.slice(st-1, en),
                                               quality=fq.quality.slice(st-1, en))
        case (Some(st), None) => fq.copy(sequence=fq.sequence.slice(st-1, fq.sequence.length),
                                               quality=fq.quality.slice(st-1, fq.sequence.length))
        case (None, Some(en)) => fq.copy(sequence=fq.sequence.slice(0, en),
                                               quality=fq.quality.slice(0, en))
        case (None, None) => fq 
      }

    case pefq: PEFastqRecord => { 
      val r1Seq = (opt.get('r1Five).asInstanceOf[Option[Int]], 
                   opt.get('r1Three).asInstanceOf[Option[Int]]) match {
        case (Some(st), Some(en)) => pefq.copy(sequence=pefq.sequence.slice(st-1, en),
                                               quality=pefq.quality.slice(st-1, en))
        case (Some(st), None) => pefq.copy(sequence=pefq.sequence.slice(st-1, pefq.sequence.length),
                                               quality=pefq.quality.slice(st-1, pefq.sequence.length))
        case (None, Some(en)) => pefq.copy(sequence=pefq.sequence.slice(0, en),
                                               quality=pefq.quality.slice(0, en))
        case (None, None) => pefq 
      }
 
      val r2Seq = (opt.get('r2Five).asInstanceOf[Option[Int]], 
                   opt.get('r2Three).asInstanceOf[Option[Int]]) match {
        case (Some(r25), Some(r23)) => pefq.read2.copy(
          sequence=pefq.read2.sequence.reverse.slice(r25-1, r23).reverse,
          quality=pefq.read2.quality.reverse.slice(r25-1, r23).reverse)

        case (Some(r25), None) => pefq.read2.copy(
          sequence=pefq.read2.sequence.reverse.slice(r25-1, pefq.read2.sequence.length).reverse,
          quality=pefq.read2.quality.reverse.slice(r25-1, pefq.read2.quality.length).reverse)

        case (None, Some(r23)) => pefq.read2.copy(
          sequence=pefq.read2.sequence.reverse.slice(0, r23).reverse,
          quality=pefq.read2.quality.reverse.slice(0, r23).reverse)

        case (None, None) => pefq.read2 
      }
   
      pefq.copy(sequence=r1Seq.sequence, quality=r1Seq.quality, read2=r2Seq)
    }
  }

  /**
   * Clip reads by search strings and return Option[Read] if based on user
   * keep commands.
   */
  def clip(read: Read, opt: OptionMap): Option[Read] = read match { 
    case fq: FastqRecord =>
      (opt.get('clipLead).asInstanceOf[Option[String]], 
       opt.get('clipTail).asInstanceOf[Option[String]]) match {
        case (Some(ld), Some(tl)) =>
          // Define lead and tail regexes
          lazy val regLead = 
            new Regex("^(" + ld.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
          lazy val regTail = 
            new Regex("(" + tl.map(x => iupac.getOrElse(x,x)).mkString + ")")
          // First search for the lead
          (regLead findFirstMatchIn fq.sequence) map (m => (m group "rest", m.start(2))) match {
            case Some(foundLead) =>
              // Now search for the tail within the newly clipped read 
              (regTail findFirstMatchIn foundLead._1) map (n => n.start) match {
                case Some(foundTail) => Some(fq.copy(sequence = foundLead._1.take(foundTail), 
                  quality = fq.quality.slice(foundLead._2, foundLead._2 + foundTail)))
                case None => 
                  if (opt.isDefinedAt('keepTail)) 
                    Some(fq.copy(sequence = foundLead._1, 
                      quality = fq.quality.slice(foundLead._2, fq.quality.length)))
                  else None
              }
            case None => 
              if (opt.isDefinedAt('keepLead)) 
                (regTail findFirstMatchIn fq.sequence) map (n => n.start) match {
                  case Some(foundTail) => Some(fq.copy(sequence = fq.sequence.take(foundTail),
                    quality = fq.quality.take(foundTail)))
                  case None => if (opt.isDefinedAt('keepTail)) Some(fq) else None
                } 
              else None
          }

        case (Some(ld), None) =>
          lazy val regLead = 
            new Regex("^(" + ld.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
          (regLead findFirstMatchIn fq.sequence) map (m => (m group "rest", m.start(2))) match {
            case Some(foundLead) => Some(fq.copy(sequence = foundLead._1,
              quality = fq.quality.slice(foundLead._2, fq.quality.length)))
            case None => if (opt.isDefinedAt('keepLead)) Some(fq) else None
          }

        case (None, Some(tl)) =>
          lazy val regTail = 
            new Regex("(" + tl.map(x => iupac.getOrElse(x,x)).mkString + ")")
          (regTail findFirstMatchIn fq.sequence) map (n => n.start) match {
            case Some(foundTail) => Some(fq.copy(sequence = fq.sequence.take(foundTail),
              quality = fq.quality.take(foundTail)))
            case None => if (opt.isDefinedAt('keepTail)) Some(fq) else None
          }
        // Shouldn't happen 
        case (None, None) => Some(fq) 
    }

    case pefq: PEFastqRecord =>
      val r1Seq = 
      (opt.get('r1ClipFive).asInstanceOf[Option[String]], 
       opt.get('r1ClipThree).asInstanceOf[Option[String]]) match {
         case (Some(r15), Some(r13)) =>
            lazy val regLead = 
              new Regex("^(" + r15.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
            lazy val regTail = 
              new Regex("(" + r13.map(x => iupac.getOrElse(x,x)).mkString + ")")
            // First search for the r15
            (regLead findFirstMatchIn pefq.sequence) map (m => (m group "rest", m.start(2))) match {
              case Some(foundLead) =>
                // Now search for the tail within the newly clipped read 
                (regTail findFirstMatchIn foundLead._1) map (n => n.start) match {
                  case Some(foundTail) => Some(pefq.copy(sequence = foundLead._1.take(foundTail), 
                    quality = pefq.quality.slice(foundLead._2, foundLead._2 + foundTail)))
                  case None => 
                    if (opt.isDefinedAt('r1KeepThree)) 
                      Some(pefq.copy(sequence = foundLead._1, 
                        quality = pefq.quality.slice(foundLead._2, pefq.quality.length)))
                    else None
                }

              case None => 
                if (opt.isDefinedAt('r1KeepFive))
                  (regTail findFirstMatchIn pefq.sequence) map (n => n.start) match {
                    case Some(foundTail) => Some(pefq.copy(sequence = pefq.sequence.take(foundTail),
                      quality = pefq.quality.take(foundTail)))
                    case None => if (opt.isDefinedAt('r1KeepThree)) Some(pefq) else None
                  }
                else None
            }

          case (Some(r15), None) =>
            lazy val regLead = 
              new Regex("^(" + r15.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
            (regLead findFirstMatchIn pefq.sequence) map (m => (m group "rest", m.start(2))) match {
              case Some(foundLead) => Some(pefq.copy(sequence = foundLead._1,
                quality = pefq.quality.slice(foundLead._2, pefq.quality.length)))
              case None => if (opt.isDefinedAt('r1KeepFive)) Some(pefq) else None
            }

          case (None, Some(r13)) =>
            lazy val regTail = 
              new Regex("(" + r13.map(x => iupac.getOrElse(x,x)).mkString + ")")
            (regTail findFirstMatchIn pefq.sequence) map (n => n.start) match {
              case Some(foundTail) => Some(pefq.copy(sequence = pefq.sequence.take(foundTail),
                quality = pefq.quality.take(foundTail)))
              case None => if (opt.isDefinedAt('r1KeepThree)) Some(pefq) else None
            }

          case (None, None) => Some(pefq) 
        }
    
      val r2Seq =
      (opt.get('r2ClipFive).asInstanceOf[Option[String]], 
       opt.get('r2ClipThree).asInstanceOf[Option[String]]) match {
         case (Some(r25), Some(r23)) =>
           lazy val reg25 = 
             new Regex("^(" + r25.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
           lazy val reg23 = 
             new Regex("(" + r23.map(x => iupac.getOrElse(x,x)).mkString + ")")
            (reg25 findFirstMatchIn pefq.read2.sequence.reverse) map (m => (m group "rest", m.start(2))) match {
              case Some(found25) =>
                // Now search for the tail within the newly clipped read 
                (reg23 findFirstMatchIn found25._1) map (n => n.start) match {
                  case Some(found23) => Some(pefq.read2.copy(sequence = found25._1.take(found23).reverse, 
                    quality = pefq.read2.quality.reverse.slice(found25._2, found25._2 + found23).reverse))
                  case None => 
                    if (opt.isDefinedAt('r2KeepThree)) 
                      Some(pefq.read2.copy(sequence = found25._1.reverse, 
                        quality = pefq.read2.quality.reverse.slice(found25._2, pefq.read2.quality.length).reverse))
                    else None 
                }

              case None =>
                if (opt.isDefinedAt('r2KeepFive))
                  (reg23 findFirstMatchIn pefq.read2.sequence.reverse) map (n => n.start) match {
                    case Some(found23) => 
                      Some(pefq.read2.copy(sequence = pefq.read2.sequence.reverse.take(found23).reverse, 
                        quality = pefq.read2.quality.reverse.slice(0, found23).reverse))
                    case None => if (opt.isDefinedAt('r2KeepThree)) Some(pefq.read2) else None
                  }
                else None 
            }

         case (Some(r25), None) =>
           lazy val reg25 = 
             new Regex("^(" + r25.map(x => iupac.getOrElse(x,x)).mkString + ")(.+)", "adapt", "rest")
            (reg25 findFirstMatchIn pefq.read2.sequence.reverse) map (m => (m group "rest", m.start(2))) match {
              case Some(found25) => Some(pefq.read2.copy(sequence = found25._1.reverse,
                quality= pefq.read2.quality.reverse.slice(found25._2, pefq.read2.sequence.length).reverse))
              case None => if (opt.isDefinedAt('r2KeepFive)) Some(pefq.read2) else None
            }

         case (None, Some(r23)) =>
           lazy val reg23 = 
             new Regex("(" + r23.map(x => iupac.getOrElse(x,x)).mkString + ")")
           (reg23 findFirstMatchIn pefq.read2.sequence.reverse) map (m => (m.start)) match {
             case Some(found23) => Some(pefq.read2.copy(
               sequence = pefq.read2.sequence.reverse.slice(0, found23).reverse,
               quality = pefq.read2.quality.reverse.slice(0, found23).reverse))
             case None => if (opt.isDefinedAt('r2KeepThree)) Some(pefq.read2) else None
           }

         case (None, None) => Some(pefq.read2)
      }

      (r1Seq, r2Seq) match {
        case (Some(rd1), Some(rd2)) => Some(pefq.copy(sequence = rd1.sequence, quality = rd1.quality,
          read2 = rd2))
        case (_,_) => None 
      } 
  }

  def apply(read: Read, userOpts: OptionMap): Option[Read] = 
    if (userOpts.isDefinedAt('start) || userOpts.isDefinedAt('end) ||
        userOpts.isDefinedAt('r1Five) || userOpts.isDefinedAt('r1Three) ||
        userOpts.isDefinedAt('r2Five) || userOpts.isDefinedAt('r2Three)) {
      if (userOpts.isDefinedAt('clipLead) || userOpts.isDefinedAt('clipTail) ||
          userOpts.isDefinedAt('r1ClipFive) || userOpts.isDefinedAt('r1ClipThree) ||
          userOpts.isDefinedAt('r2ClipFive) || userOpts.isDefinedAt('r2ClipThree))
        clip(trim(read, userOpts), userOpts)
      else
        Some(trim(read, userOpts))
    }

    else if (userOpts.isDefinedAt('clipLead) || userOpts.isDefinedAt('clipTail) ||
             userOpts.isDefinedAt('r1ClipFive) || userOpts.isDefinedAt('r1ClipThree) ||
             userOpts.isDefinedAt('r2ClipFive) || userOpts.isDefinedAt('r2ClipThree))
      clip(read, userOpts)

    else Some(read)
}
