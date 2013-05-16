package com.kmh.ngs.formats
//import org.eintr.loglady.Logging

class SAMRecord(
	val query: String,
	val flag: Int,
	val target: String,
	val mapq: Int,
	val refstart: Int,
	val cigar: String,
	val sequence: String,
	val quality: String) {
  /*final val pair_flag = 0x1
  final val full_pair_flag = 0x2
  final val read_unmapped_flag = 0x4
  final val mate_unmapped_flag = 0x8
  final val read_strand_flag = 0x10
  final val mate_strand_flag = 0x20
  final val first_pair_flag = 0x40
  final val second_pair_flag = 0x80
  final val not_primary_aln_flag = 0x100 
  final val fail_vendor_qual_check_flag = 0x200
  final val duplicate_read_flag = 0x400*/

  def repr: Unit = {
    println("-"*80)
    println("%s\n%s\n%s\n%s\n%s\n%s\n".format(query, flag, target, mapq, refstart, cigar) +
            "%s\n%s".format(sequence, quality))
    println("-"*80)
  }

}

object SAMRecord {
  def apply(line: String, ct: Int) = {
    val lineArr = line.split("\t")
    if (lineArr.length < 11) {
      throw new RuntimeException("Error!! Line '%s' has less than ".format(ct) +
        "the required number of columns")
    }

    new SAMRecord(lineArr(0),lineArr(1).toInt,
	lineArr(2),lineArr(3).toInt,lineArr(4).toInt,
	lineArr(5),lineArr(9),lineArr(10))
  }
} 
