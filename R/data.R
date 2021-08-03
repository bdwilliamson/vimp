#' Neutralization sensitivity of HIV viruses to antibody VRC01
#' 
#' A dataset containing neutralization sensitivity -- measured using inhibitory 
#' concentration, the quantity of antibody necessary to neutralize a fraction of 
#' viruses in a given sample -- and viral features including: 
#' amino acid sequence features (measured using HXB2 coordinates), geographic region of origin,
#' subtype, and viral geometry. Accessed from the Los Alamos National Laboratory's (LANL's)
#' Compile, Analyze, and tally Neutralizing Antibody Panels (CATNAP) database.
#' 
#' @format A data frame with 611 rows and 837variables:
#' \describe{
#'   \item{seqname}{Viral sequence identifiers}
#'   \item{subtype.is.*}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{geographic.region.of.origin.is.*}{Dummy variables encoding the 
#'     geographic region of origin as 0/1. Regions are Asia, Europe/Americas,
#'     North Africa, and Southern Africa.}
#'   \item{ic50.censored}{A binary indicator of whether or not the IC-50 (the 
#'     concentration at which 50% of viruses are neutralized) was right-censored.
#'     Right-censoring is a proxy for a resistant virus.}
#'   \item{ic80.censored}{A binary indicator of whether or not the IC-80 (the 
#'     concentration at which 80% of viruses are neutralized) was right-censored.
#'     Right-censoring is a proxy for a resistant virus.}
#'   \item{ic50.geometric.mean.imputed}{Continuous IC-50. If neutralization 
#'     sensitivity for the virus was assessed in multiple studies, the geometric mean
#'     was taken.} 
#'   \item{ic80.geometric.mean.imputed}{Continuous IC-90. If neutralization 
#'     sensitivity for the virus was assessed in multiple studies, the geometric mean
#'     was taken.}
#'   \item{hxb2.*}{Amino acid sequence features denoting the presence (1) or absence (0)
#'     of a residue at a given HXB2-referenced site. For example, \code{hxb2.46.E.1mer} 
#'     records the presence of an E at HXB2-referenced site 46.}
#'   \item{sequons.total.*}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{cysteines.*}{The number of cysteines in various areas of the HIV
#'     viral envelope protein.}
#'   \item{length.*}{The length of various areas of the HIV viral envelope protein.}
#'   \item{taylor.*}{The steric bulk of residues at critical locations.}    
#' }
#' @source \url{https://github.com/benkeser/vrc01/data/fulldata.csv}
"vrc01"