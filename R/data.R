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
#'     of a residue at the given HXB2-referenced site. For example, \code{hxb2.46.E.1mer} 
#'     records the presence of an E at HXB2-referenced site 46.}
#' \item{hxb2.46.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.46.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.46.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.46.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.61.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.61.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.61.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.61.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.97.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.97.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.97.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.97.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.124.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.124.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.125.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.125.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.127.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.127.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.C.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.C.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.156.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.156.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.156.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.156.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.156.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.156.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.179.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.181.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.181.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.181.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.181.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.190.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.197.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.197.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.197.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.198.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.198.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.198.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.198.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.241.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.241.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.241.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.241.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.276.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.276.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.276.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.276.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.278.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.278.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.278.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.278.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.278.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.279.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.279.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.279.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.279.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.279.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.280.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.280.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.280.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.280.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.281.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.281.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.281.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.281.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.281.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.281.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.281.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.282.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.282.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.282.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.282.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.282.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.282.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.283.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.283.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.283.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.283.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.290.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.321.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.328.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.365.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.369.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.369.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.369.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.369.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.369.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.369.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.371.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.371.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.371.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.371.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.374.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.374.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.374.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.386.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.386.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.386.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.386.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.386.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.389.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.W.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.C.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.W.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.W.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.C.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.415.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.425.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.425.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.426.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.426.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.426.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.426.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.426.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.428.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.428.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.428.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.429.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.429.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.429.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.429.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.429.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.429.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.429.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.430.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.430.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.430.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.430.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.430.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.431.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.431.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.432.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.432.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.432.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.432.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.455.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.455.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.455.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.455.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.455.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.455.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.456.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.456.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.456.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.456.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.456.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.456.W.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.456.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.457.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.458.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.458.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.458.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.458.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.459.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.459.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.459.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.459.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.459.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.459.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.gap.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.P.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.466.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.466.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.466.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.466.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.466.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.466.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.466.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.467.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.467.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.467.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.469.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.A.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.471.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.474.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.474.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.474.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.475.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.475.M.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.476.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.476.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.477.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.477.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.544.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.544.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.569.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.569.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.569.X.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.589.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.589.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.E.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.655.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.668.D.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.668.G.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.668.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.668.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.668.T.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.675.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.675.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.677.H.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.677.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.677.N.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.677.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.677.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.677.S.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.680.W.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.681.Y.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.683.K.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.683.Q.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.683.R.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.688.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.688.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.702.F.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.702.I.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.702.L.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.702.V.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.29.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.49.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.59.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.88.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.130.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.132.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.133.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.134.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.135.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.136.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.137.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.138.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.139.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.140.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.141.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.142.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.143.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.144.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.145.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.146.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.147.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.148.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.149.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.150.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.156.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.160.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.171.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.185.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.186.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.187.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.188.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.197.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.229.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.230.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.232.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.234.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.241.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.268.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.276.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.278.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.289.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.293.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.295.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.301.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.302.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.324.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.332.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.334.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.337.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.339.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.343.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.344.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.350.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.354.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.355.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.356.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.358.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.360.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.362.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.363.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.386.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.392.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.393.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.394.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.395.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.396.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.397.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.398.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.399.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.400.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.401.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.402.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.403.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.404.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.405.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.406.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.407.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.408.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.409.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.410.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.411.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.412.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.413.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.442.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.444.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.446.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.448.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.460.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.461.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.462.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.463.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.465.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.611.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.616.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.618.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.619.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.624.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.625.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.637.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.674.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.743.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.750.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.787.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#' \item{hxb2.816.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
#'   \item{hxb2.824.sequon_actual.1mer}{Amino acid sequence feature denoting the presence (1) or absence (0) of a residue at the given HXB2-referenced site.}
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
#' @source \url{https://github.com/benkeser/vrc01/blob/master/data/fulldata.csv}
#' @usage data("vrc01")
"vrc01"