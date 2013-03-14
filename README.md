Genome Weaver
=== 
Genome Weaver is a set of utilities for manipulating large genomic data set using Scala (and Java).

## Features

* LArray: large arrays that can hold more than 2G (2^31) entries and can be released from the main memory immediately.
  * 2^31 (2GB) is the limitation of the default array. LArray uses Long (2^63) type indexes, so the entire human genome (3GB) data can be stored in LArray. 
  * And also, the default arrays in Scala(Java) consumes JVM heaps heavily and often causes OutOfMemory error when using large amount of genomic data. LArray uses memory space outside of the default JVM heap, and can release its acquired memory resource immediately.
* Efficient data structures for biological data (GLocus, GInterval, PrioritySearchTrees, suffix arrays, BWT, etc.)
* Biological data format readers 
* FM-index based alignment (soon)
* Visualiazation libraries for genome browsers. 
  * Related UTGB:<https://github.com/utgenome/utgb>

## Supported Formats

* FASTA
* FASTQ
* BED
* WIG

## ToDo

* Add format support
  * GFF
  * SAM/BAM (using picard)
  * AXT
  * bigWig/bigBED
  * Silk
* Data storage
  * compressed storage
  * split for parallel read/write


