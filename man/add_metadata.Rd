% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_metadata.R
\name{add_metadata}
\alias{add_metadata}
\title{add_metadata}
\usage{
add_metadata(df, metadf, by_vcf, by_meta)
}
\arguments{
\item{df}{A rearranged vcf dataframe (arrange_data)}

\item{metadf}{A metadata dataframe}

\item{by_vcf}{A vector of column names in the vcf dataframe that should be used to merge the vcf data with the metadata}

\item{by_meta}{A vector of column names in the metadata dataframe that should be used to merge the metadata with the vcf data}
}
\value{
A vcf dataframe with metadata included
}
\description{
Adds metadata information to the vcf dataframe
}
\examples{
df <- data.frame(CHROM = c("A", "B"),
                 POS = c(234, 240),
                 REF = c("G", "A"),
                 ALT = c("A", "G")
)

sizes <- data.frame(segment = c("A", "B"),
                    SegmentSize = c(2280, 2274)
)

df

sizes

# Add a new column of sizes of the segments which are necessary for
# downstream calculations such as transition:transversion (tstv) and dNdS.
add_metadata(df, sizes, c('CHROM'), c('segment'))

}
