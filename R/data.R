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
#'   \item{subtype.is.01_AE}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.02_AG}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.07_BC}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.A1}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.A1C}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.A1D}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.B}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.C}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.D}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.O}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{subtype.is.Other}{Dummy variables encoding the viral subtype as 0/1. 
#'     Possible subtypes are 01_AE, 02_AG, 07_BC, A1, A1C, A1D, B, C, D, O, Other.}
#'   \item{geographic.region.of.origin.is.Asia}{Dummy variables encoding the 
#'     geographic region of origin as 0/1. Regions are Asia, Europe/Americas,
#'     North Africa, and Southern Africa.}
#'   \item{geographic.region.of.origin.is.Europe.Americas}{Dummy variables encoding the 
#'     geographic region of origin as 0/1. Regions are Asia, Europe/Americas,
#'     North Africa, and Southern Africa.}
#'   \item{geographic.region.of.origin.is.N.Africa}{Dummy variables encoding the 
#'     geographic region of origin as 0/1. Regions are Asia, Europe/Americas,
#'     North Africa, and Southern Africa.}
#'   \item{geographic.region.of.origin.is.S.Africa}{Dummy variables encoding the 
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
#'   \item{hxb2.46.E.1mer}{Amino acid sequence features denoting the presence (1) or absence (0)
#'     of a residue at a given HXB2-referenced site. For example, \code{hxb2.46.E.1mer} 
#'     records the presence of an E at HXB2-referenced site 46.}
#'   ...
#'   \item{sequons.total.env}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.gp120}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.v5}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.loop.d}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.loop.e}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.vrc01}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.cd4}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.sj.fence}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{sequons.total.sj.trimer}{The total number of sequons in various areas of the 
#'     HIV viral envelope protein.}
#'   \item{cysteines.total.env}{The number of cysteines in various areas of the HIV
#'     viral envelope protein.}
#'   \item{cysteines.total.gp120}{The number of cysteines in various areas of the HIV
#'     viral envelope protein.}
#'   \item{cysteines.total.v5}{The number of cysteines in various areas of the HIV
#'     viral envelope protein.}
#'   \item{cysteines.total.vrc01}{The number of cysteines in various areas of the HIV
#'     viral envelope protein.}
#'   \item{length.env}{The length of various areas of the HIV viral envelope protein.}
#'   \item{length.gp120}{The length of various areas of the HIV viral envelope protein.}
#'   \item{length.v5}{The length of various areas of the HIV viral envelope protein.}
#'   \item{length.v5.outliers}{The length of various areas of the HIV viral envelope protein.}
#'   \item{length.loop.e}{The length of various areas of the HIV viral envelope protein.}
#'   \item{length.loop.e.outliers}{The length of various areas of the HIV viral envelope protein.}
#'   \item{taylor.small.total.v5}{The steric bulk of residues at critical locations.}    
#'   \item{taylor.small.total.loop.d}{The steric bulk of residues at critical locations.}  
#'   \item{taylor.small.total.cd4}{The steric bulk of residues at critical locations.}  
#' }
#' @source \url{https://github.com/benkeser/vrc01/data/fulldata.csv}
#' @usage data("vrc01")
"vrc01"