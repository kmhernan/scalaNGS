# NGStools

*A compilation of tools for filtering and manipulating various NGS format written in the Scala language.*

**Author: Kyle Hernandez**

**Email: kmhernan@utexas.edu**

========

## Install

1.  Download and install SBT (here)[http://www.scala-sbt.org/]
2.  `git clone https://github.com/kmhernan/scalaNGS.git` or click the `Download as zip file` button to the right
3.  Change to the scalaNGS folder
4.  `sbt update`
5.  `sbt assembly`
6.  The jar executable will be in `/target/scala-2.10/NGStool.jar`
7.  The accessory R script for plotting QC data is located `/scripts/PlotQualityStats.R`
    * This requires the following libraries: ggplot2, grid, and reshape2
    * Run from command-line as: `Rscript PlotQualityStats.R <input.stat> </ouput/prefix/of/image>`

## General Usage:

`java -jar NGStools.jar -T/-TOOL <tool> [-h/--help]`

Available tools:
* FilterReads - Filters NGS reads based on user-inputs.
* ReadStatistics - Creates a tab-delimited file containing various statistics, which can be fed into the accessory R-script PlotQualityStats.R

## FilterReads Usage:

### Solid reads

```
usage: java -jar NGStools.jar -T FilterReads -P/-PLATFORM solid
                              -I/-INPUT file.csfasta file.qual -O/-OUTPUT file.csfasta file.qual
                              [-START Int] [-END Int] [-HPOLY Double] [-MINQ Int] [--MISSING] [-h/--help]

Required Arguments:
  -I/-INPUT	Input raw read files: <file.csfasta> <file.qual>
  -O/-OUTPUT	Output filtered read files: <file.csfasta> <file.qual>

Optional Arguments:
  -START	5' cut position (1-based index)
  -END		3' cut position (1-based index)
  -HPOLY	Relative length of repetitive base to consider a homopolymer.
  -MINQ	Minimum average quality score allowed.
  --MISSING	Removes reads with missing data, required for mapping reads.
  -h/--help	Print this message and exit.
```

### Single-end Illumina

```
usage: java -jar NGStools.jar -T FilterReads -P/-PLATFORM SE_illumina 
                              -I/-INPUT file.fastq -O/-OUTPUT file.fastq -QV-OFFSET {33,64}
                              [-START Int] [-END Int] [-HPOLY Double] [-MINQ Int] [-NMISSING Int]
                              [-POLYA Double Int] [-h/--help]
                              
Required Arguments:
  -I/-INPUT <String>	Input raw read files: <file.fastq> or <file.fastq.gz>
  -O/-OUTPUT <String>	Output filtered read files: <file.fastq>
  -QV-OFFSET <String>	Phred-scaled offset [33, 64]

Optional Arguments:
  -START <Int>		5' cut position (1-based index)
  -END <Int>		3' cut position (1-based index). Ex. AlfI: -START 1 -END 36
  -HPOLY <Double>	Relative length of repetitive base to consider a homopolymer. (Proportion of read length; e.g., between 0 and 1)
  -MINQ <Int>		Minimum average quality score allowed.
  -NMISSING <Int>	Lower limit for N's allowed.
  -POLYA <Double> <Int>	Takes two values:
        		  1) ProportionLimit [Double] - If a read has trailing A's of length <value> * sequence length, trim them.
        		  2) MinimumSize [Int] - If the trimmed sequence is shorter than <value>, remove it.
  -h/--help		Print this message and exit.
```

### Paired-end Illumina

```
usage: java -jar NGStools.jar -T FilterReads -P/-PLATFORM PE_illumina 
                              {-R1/-READ1 file_R1.fastq -R2/-READ2 file_R2.fastq | -INTER file_R1_R2.fastq} 
                              {-O1/-OUTREAD1 file_R1.fastq -O2/-OUTREAD2 file_R2.fastq | -OUT-INTER file_R1_R2.fastq}
                              -QV-OFFSET {33, 64} [-START Int] [-END Int] [-HPOLY Double] [-MINQ Int] [-NMISSING Int]
                              [-POLYA Double Int] [-h/--help]

Required Arguments:
Supports both separated and interleaved paired-end Fastq files.
If input Fastq files are separated (mate-pairs must be sorted in same order):
  -R1/-READ1 <String>	Input raw fastq file for first paired-end: <file_R1.fastq> or <file_R1.fastq.gz>
  -R2/-READ2 <String>	Input raw fastq file for second paired-end: <file_R2.fastq> or <file_R2.fastq.gz>

If input Fastq file is interleaved (pair 1 must always be followed by its mate-pair 2):
  -INTER <String>	Input raw fastq file containing both pairs: <file_R1_R2.fastq> or <file_R1_R2.fastq.gz>

Regardless of input format, reads can be written to either separated or interleaved fastq files:
  -O1/-OUTPUT1 <String>	Output separated fastq file for first paired-end: <file_R1.fastq>
  -O2/-OUTPUT2 <String>	Output separated fastq file for second paired-end: <file_R2.fastq>
  -OUT-INTER <String>	Output interleaved fastq file: <file_R1_R2.fastq>

  -QV-OFFSET <Int>	Phred-scaled offset [33, 64]

Optional Arguments:
  -START <Int>		5' cut position (1-based index)
  -END <Int>		3' cut position (1-based index). Ex. AlfI: -START 1 -END 36
  -HPOLY <Double>	Relative length of repetitive base to consider a homopolymer. (Proportion of read length; e.g., between 0 and 1)
  -MINQ <Int>		Minimum average quality score allowed.
  -NMISSING <Int>	Lower limit for N's allowed.
  -POLYA <Double> <Int>	Takes two values:
        		  1) ProportionLimit <Double> - If a read has trailing A's of length <value> * sequence length, trim them.
        		  2) MinimumSize <Int> - If the trimmed sequence is shorter than <value>, remove it.
  -h/--help		Print this message and exit.
```

## ReadStatistics Usage:

```
usage: java -jar NGStools.jar -T ReadStatistics -I/-INPUT file.fastq -QV-OFFSET [33,64] [-O/-OUTPUT file.txt] [-h/--help]

Required arguments:
  -I/-INPUT <String>	Input fastq file: <file.fastq> or <file.fastq.gz>
  -QV-OFFSET <Int>	Phred-scaled offset [33, 64]

Optional Arguments:
  -O/-OUTPUT <String>	Output stats file: <file.txt> [default stdout]
  -h/--help		Print this message and exit.
```
