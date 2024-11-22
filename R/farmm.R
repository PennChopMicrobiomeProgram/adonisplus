#' Food And Resulting Microbial Metabolites (FARMM) study data
#'
#' Information on samples collected in the FARMM study, and a distance matrix
#' containing Bray-Curtis distances between the microbial communities in each
#' sample.
#'
#' @references Tanes C, Bittinger K, Gao Y, Friedman ES, Nessel L, Paladhi UR,
#'   Chau L, Panfen E, Fischbach MA, Braun J, Xavier RJ, Clish CB, Li H,
#'   Bushman FD, Lewis JD, Wu GD. Role of dietary fiber in the recovery of the
#'   human gut microbiome and its metabolome. Cell Host Microbe. 2021 Mar
#'   10;29(3):394-407.e5. doi: 10.1016/j.chom.2020.12.012. Epub 2021 Jan 12.
#'   PMID: 33440171; PMCID: PMC8022197.
#' @source \url{https://pubmed.ncbi.nlm.nih.gov/33440171/}
#' @name farmm

#' @rdname farmm
#' @format \code{farmm_samples} is a data frame with 414 rows and 11 variables:
#' \describe{
#'   \item{sample_id}{unique identifiers for each sample, to match the
#'     distances in \code{farmm_bc}}
#'   \item{subject_id}{unique identifiers for each subject in the study}
#'   \item{study_day}{the day of the study on which the sample was collected}
#'   \item{diet}{the diet assigned to each subject}
#'   \item{antibiotics}{the antibiotics status of each subject during the study.
#'     A factor with three levels: "pre" (before antibiotic exposure),
#'     "current" (during antibiotic exposure), and "post" (after antibiotic
#'     exposure)}
#'   \item{height}{the height of each subject}
#'   \item{weight}{the weight of each subject}
#'   \item{age}{the age of each subject}
#'   \item{bacterial_16S_copies}{the number of bacterial 16S rRNA gene copies
#'     per gram feces, giving an estimate of absolute bacterial abundance}
#'   \item{num_reads}{the number of high-quality, non-host sequencing reads per
#'     sample}
#'   \item{host_frac}{the fraction of host reads in the shotgun metagenomic DNA
#'     sequencing data}
#' }
"farmm_samples"

#' @rdname farmm
#' @format \code{farmm_bc} is a \code{dist} object containing the Bray-Curtis
#'   distances between the 414 samples listed in \code{farmm_samples}.
"farmm_bc"
