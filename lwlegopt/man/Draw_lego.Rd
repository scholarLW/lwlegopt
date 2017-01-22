\name{Draw_lego}
\alias{Draw_lego}
\alias{lwlegopt}
\alias{lego}
\title{The tool is used to plot lego image for mutation spectrum}
\description{
   The tool is used to plot lego image for mutation spectrum.
}
\usage{
Draw_lego(
         h=NULL,         # Data matrix for plot lego, [96 x (n+2), n stands for sample counts] 
	 path=NULL,      # The directory of input file.
	 file_name=NULL, # The name of input file 
     	 title=NULL,     # Cancer name 	 
	 prepare=FALSE,  # Prophase data prepare  
     	 samtools=NULL,  # Samtools software,if prepare id TRUE, the parameter must be set
	 fasta=NULL,	 # Genome of human,if prepare id TRUE, the parameter must be set
     	 sort=NULL,      # Sort for trinucleotide percentage value
	 top=TRUE,       # Location of icon displaying
	 kge=3,          # Scale interval of Y axis 
	 color=NULL      # colours for lego figure
	 border=NULL     # colours for border
}
\arguments{
  \item{h}{Data matrix for plot lego}
  \item{path}{Data path}
  \item{file_name}{Name of input file}
  \item{title}{Cancer type}
  \item{prepare}{Prophase data prepare for the results detected by mutect}
  \item{samtools}{Samtools software}  
  \item{fasta}{Genome of human}  
  \item{sort}{Sort for trinucleotide percentage value, TRUE,FALSE and NULL}
  \item{top}{Location of icon displaying,TRUE,FALSE}
  \item{kge}{Scale interval of Y axis}
  \item{color}{colours for lego figure}
  \item{border}{colours for border}
}
\details{

}
\examples{
####### 1 #########
library(lwlegopt)
data(Mutdata)
Draw_lego(h)

####### 2 #########
library(lwlegopt)
Draw_lego(path="example",file_name='TCGA.originalGenomes_with_types.txt')

####### 3 #########
library(lwlegopt)
Draw_lego(path="example",file_name='combined_mutations.out.xls',title='CRC',prepare=TRUE,samtools='/usr/bin/samtools/samtools',fasta='/usr/Database/human_19/hg19_fasta_GATK/hg19.fasta')
}
