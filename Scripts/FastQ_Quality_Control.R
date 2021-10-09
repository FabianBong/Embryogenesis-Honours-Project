## Loading FASTQ Data and estimating methylation status

## Loading necessary packages
library(fastqcr) # Loading and quality analysis of fastQ files

## Set working directory for easy data access
setwd("/Users/fabianbong/Documents/Honours_Project_R/Data/")

## Check sample for quality 
fastqc_install() ## installing fastqc if not installed

fastQRReport <- fastqc()

## ANALYISIS OF THAT INFORMATION HERER ##

