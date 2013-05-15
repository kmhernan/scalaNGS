package com.kmh.ngs.formats
import org.eintr.loglady.Logging

class SAMrecord(
	query: String,
	flag: Int,
	target: String,
	mapq: Double,
	refstart: Int,
	cigar: String,
	sequence: String,
	quality: String) extends Logging {
  final val pair_flag = 0x1
  final val full_pair_flag = 0x2
  final val read_unmapped_flag = 0x4
  final val mate_unmapped_flag = 0x8
  final val read_strand_flag = 0x10
  final val mate_strand_flag = 0x20
  final val first_pair_flag = 0x40
  final val second_pair_flag = 0x80
  final val not_primary_aln_flag = 0x100 
  final val fail_vendor_qual_check_flag = 0x200
  final val duplicate_read_flag = 0x400
}

object SAMrecord {
  def apply(line: String) = {
    val lineArr = line.split("\t")
    new SAMrecord(lineArr(0),lineArr(1).toInt,
	lineArr(2),lineArr(3).toInt,lineArr(4).toDouble,
	lineArr(5),lineArr(9),lineArr(10))
  }
} 
