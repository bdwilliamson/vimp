.onAttach <- function(...) {
  pkg_desc <- utils::packageDescription("vimp")
  packageStartupMessage(paste0(
    "vimp version ", pkg_desc$Version,
    ": ", pkg_desc$Title
  ))
  packageStartupMessage(paste0(
    "Package created on ", utils::packageDate("vimp")
  ))
}