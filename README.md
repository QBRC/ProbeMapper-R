![QBRC_logo](QBRC.jpeg)

# ProbeMapper-R
The R package client for ProbeMapper

## Introduction

Access to gene expression data has become increasingly common in recent years; however, analysis has become more difficult as it is often desirable to integrate data from different platforms. Probe mapping across microarray platforms is the first and most crucial step for data integration. In this article, we systematically review and compare different approaches to map probes across seven platforms from different vendors: U95A, U133A and U133 Plus 2.0 from Affymetrix, Inc.; HT-12 v1, HT-12v2 and HT-12v3 from Illumina, Inc.; and 4112A from Agilent, Inc. We use a unique data set, which contains 56 lung cancer cell line samples—each of which has been measured by two different microarray platforms—to evaluate the consistency of expression measurement across platforms using different approaches. Based on the evaluation from the empirical data set, the BLAST alignment of the probe sequences to a recent revision of the Transcriptome generated better results than using annotations provided by Vendors or from Bioconductor's Annotate package. However, a combination of all three methods (deemed the ‘Consensus Annotation’) yielded the most consistent expression measurement across platforms. To facilitate data integration across microarray platforms for the research community, we develop a user-friendly web-based tool, an API and an R package to map data across different microarray platforms from Affymetrix, Illumina and Agilent.

## Citation

Allen JD, Wang S, Chen M, Girard L, Minna J, Xie Y, Xiao G*. Probe mapping across multiple microarray platforms, Briefings in Bioinformatics, 2012 Sep;13(5):547-54. doi: 10.1093/bib/bbr076. PMID: 22199380
