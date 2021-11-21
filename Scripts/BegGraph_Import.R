library(rtracklayer)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)

## BedGraph file has been generated with Bowtie and Bismark
file_path <- "/Users/fabianbong/Documents/Honours_Project_R/Data/Processed_Data/CpG/Union/Combined/Methylation_Reproduce_NoNA.bed.txt"

## Load in samples
samples <- read.table(file_path, sep = "\t", header=TRUE)

## Remvoe any unecessary columns/rows and rename rows
samples <- samples[-1,]
samples <- samples[,-3]


## Remove duplicate rows
rowNames <- paste("CpG",sub('chr', '', samples$chrom),samples$start,sep="")
rowNamesDup <- duplicated(rowNames)

## Remvoe dup
samples <- samples[!rowNamesDup,]


## Rename rows
rownames(samples) <- paste("CpG",sub('chr', '', samples$chrom),samples$start,sep="")

## Rname start col
colnames(samples)[2] <- "pos"

