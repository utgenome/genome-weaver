Genome Weaver
=== 
Genome Weaver is a set of utilities for manipulating large genomic data set using Scala (and Java).

## Features

* LArray: large arrays that can hold more than 2G (2^31) entries and can be released from the main memory immediately, unlike the default arrays.
* Efficient data structures for biological data (GLocus, GInterval, PrioritySearchTrees, suffix arrays, BWT, etc.)
* Biological data format readers 
* FM-index based alignment (soon)
* Visualiazation libraries for genome browsers. Related UTGB:<https://github.com/utgenome/utgb>

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


