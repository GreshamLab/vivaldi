% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_shannon.R
\name{plot_shannon}
\alias{plot_shannon}
\title{plot_shannon}
\usage{
plot_shannon(shannon_df)
}
\arguments{
\item{shannon_df}{A dataframe that has been arranged (arrange_gt_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy)}
}
\value{
Three plots showing the nt Shannon, chrom Shannon, and full genome Shannon calculations
}
\description{
Reads in a dataframe that has been arranged (arrange_gt_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy) and outputs plots
}
\examples{
plot_shannon(shannon_df)
}
