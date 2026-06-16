# vivaldi
<!-- badges: start -->
[![R-CMD-check](https://github.com/GreshamLab/vivaldi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GreshamLab/vivaldi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**vivaldi** (**Vi**ral **Va**riant **L**ocation **a**nd **Di**versity) is an R package for analyzing intrahost viral variation from Illumina sequencing data. The package is built around variant call format (VCF) files and provides tools to:

- import and tidy viral variant calls from one or many VCF files
- merge technical replicates
- filter variants by coverage and allele frequency thresholds
- parse SnpEff annotations
- summarize shared variants and mutation spectra
- quantify diversity using metrics such as Shannon entropy and dN/dS
- generate publication-ready visualizations of variant positions, frequencies, and genome-wide patterns

The package was developed for viral diversity analyses such as those described in the final published paper:

> Roder AE, Johnson KEE, Knoll M, Khalfan M, Wang B, Schultz-Cherry S, Banakis S, Kreitman A, Mederos C, Wang W, Ruchnewitz D, Samanovic MI, Mulligan MJ, Lassig M, Łuksza M, Das S, Gresham D, Ghedin E. *Optimized Quantification of Intrahost Viral Diversity in SARS-CoV-2 and Influenza Virus Sequence Data*. **mBio**. 2023. https://doi.org/10.1128/mbio.01046-23

## Installation

### Install from CRAN

```r
install.packages("vivaldi")
```

### Install the development version from GitHub

```r
# install.packages("remotes")
remotes::install_github("GreshamLab/vivaldi")
```

## Package dependencies

`vivaldi` depends on widely used R packages for variant parsing, data wrangling, and visualization:

- [`vcfR`](https://cran.r-project.org/package=vcfR) for reading VCF files
- [`dplyr`](https://cran.r-project.org/package=dplyr), [`tidyr`](https://cran.r-project.org/package=tidyr), and [`magrittr`](https://cran.r-project.org/package=magrittr) for data manipulation
- [`ggplot2`](https://cran.r-project.org/package=ggplot2) and [`plotly`](https://cran.r-project.org/package=plotly) for plotting
- [`seqinr`](https://cran.r-project.org/package=seqinr) for reading reference FASTA files
- [`glue`](https://cran.r-project.org/package=glue) for string handling

## Input data

The main input is a directory of `.vcf` files. Depending on the workflow, you may also provide:

- a reference FASTA file used for alignment
- a table of segment or genome sizes
- a replicate mapping table for technical replicate merging
- SnpEff-annotated VCF files for annotation-aware downstream analyses

The package ships with example data in `/home/runner/work/vivaldi/vivaldi/inst/extdata`, including:

- `H1N1.fa` - example influenza reference FASTA
- `SegmentSize.csv` - segment size metadata
- `reps.csv` - replicate metadata
- `vcfs/` - example annotated VCF files

An example processed dataset is also included as `example_filtered_SNV_df`.

## Typical workflow

A common `vivaldi` analysis looks like this:

1. **Load VCF files** with `arrange_data()`
2. **Merge technical replicates** with `merge_replicates()` when replicate sequencing is available
3. **Filter low-confidence variants** with `filter_variants()`
4. **Expand annotation fields** with `prepare_annotations()`
5. **Add metadata** such as segment sizes with `add_metadata()`
6. **Summarize diversity** with functions such as `tstv_ratio()`, `shannon_entropy()`, and `dNdS_segment()`
7. **Visualize results** with functions such as `af_distribution()`, `snv_location()`, `snv_genome()`, `snv_segment()`, `plot_shannon()`, and `shared_snv_plot()`

## Quick start

```r
library(vivaldi)

vardir <- system.file("extdata", "vcfs", package = "vivaldi")
reference_fasta <- system.file("extdata", "H1N1.fa", package = "vivaldi")
seg_sizes <- system.file("extdata", "SegmentSize.csv", package = "vivaldi")
rep_info <- system.file("extdata", "reps.csv", package = "vivaldi")

vcf_df <- arrange_data(vardir, reference_fasta, annotated = "yes")
replicates <- read.csv(rep_info)
sizes <- read.csv(seg_sizes)

merged_df <- merge_replicates(
  vcf_df,
  replicates,
  rep1 = "rep1",
  rep2 = "rep2",
  cols = c("sample", "CHROM", "POS", "REF", "ALT", "ANN", "ALT_TYPE", "major", "minor")
)

filtered_df <- filter_variants(
  merged_df,
  coverage_cutoff = 0,
  frequency_cutoff = 0.01
)

annotated_df <- prepare_annotations(filtered_df)
annotated_df <- add_metadata(annotated_df, sizes, c("CHROM"), c("segment"))
annotated_df <- shannon_entropy(annotated_df, genome_size = 13133)

plot_shannon(annotated_df)
```

## Core functions

### Import and preparation

- `arrange_data()` - read VCF files and combine them into a single dataframe
- `read_reference_fasta_dna()` - extract chromosome or segment sizes from a FASTA file
- `prepare_annotations()` - split SnpEff annotations into separate columns
- `snpeff_info()` - parse annotation information from VCF INFO fields
- `add_metadata()` - join external metadata to the variant dataframe

### Filtering and replicate handling

- `filter_variants()` - apply coverage and allele frequency thresholds
- `merge_replicates()` - keep shared variants across sequencing replicates and compute summary frequencies

### Summary statistics

- `tally_it()` - count variants over user-defined groups
- `tstv_ratio()` - calculate transition/transversion ratios
- `shannon_entropy()` - calculate per-position, per-segment, and genome-wide diversity metrics
- `dNdS_segment()` - estimate dN/dS summaries by coding feature or segment

### Visualization and exploration

- `af_distribution()` - plot minor allele frequency distributions
- `position_allele_freq()` - inspect the allele frequencies of a single site across samples
- `shared_snv_plot()` and `shared_snv_table()` - identify and summarize shared variants
- `snv_location()` - visualize SNV positions across samples and segments
- `snv_genome()` - summarize SNVs across genomes
- `snv_segment()` - summarize SNVs by genome segment
- `plot_shannon()` - visualize Shannon entropy summaries
- `tstv_plot()` - visualize transition/transversion summaries

## Documentation

For a longer worked example, see the package vignette:

```r
browseVignettes("vivaldi")
```

Source documentation and examples are also available in the package help pages:

```r
help(package = "vivaldi")
```

## Testing and development

The repository uses standard R package tooling, including `testthat` tests under `/home/runner/work/vivaldi/vivaldi/tests/testthat` and GitHub Actions R-CMD-check workflows.

## Authors

Marissa Knoll, Katherine Johnson, Megan Hockman, Eric Borenstein, Mohammed Khalfan, Elodie Ghedin, and David Gresham

Maintainer: David Gresham <dg107@nyu.edu>

## Citation

If you use `vivaldi` in published research, please cite the published article rather than the preprint:

> Roder AE, Johnson KEE, Knoll M, Khalfan M, Wang B, Schultz-Cherry S, Banakis S, Kreitman A, Mederos C, Wang W, Ruchnewitz D, Samanovic MI, Mulligan MJ, Lassig M, Łuksza M, Das S, Gresham D, Ghedin E. *Optimized Quantification of Intrahost Viral Diversity in SARS-CoV-2 and Influenza Virus Sequence Data*. **mBio**. 2023. https://doi.org/10.1128/mbio.01046-23
