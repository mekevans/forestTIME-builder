#' Estimate biomass and carbon using NSVB framework
#'
#' Estimates biomass and carbon variables using the National Scale Volume and
#' Biomass estimators (NSVB).
#'
#' @param data a data frame or tibble; generally the output of [fia_annualize()].
#' 
#' @author David Walker
#' @references Westfall, J.A., Coulston, J.W., Gray, A.N., Shaw, J.D., Radtke,
#' P.J., Walker, D.M., Weiskittel, A.R., MacFarlane, D.W., Affleck, D.L.R.,
#' Zhao, D., Temesgen, H., Poudel, K.P., Frank, J.M., Prisley, S.P., Wang, Y.,
#' Sánchez Meador, A.J., Auty, D., Domke, G.M., 2024. A national-scale tree
#' volume, biomass, and carbon modeling system for the United States. U.S.
#' Department of Agriculture, Forest Service. \doi{doi:10.2737/wo-gtr-104}
#' 
#' @returns a tibble with the additional columns `DRYBIO_AG` and `CARBON_AG`
#' that correspond to the FIAdb definitions of those variables.
#' 
#' @export
fia_estimate <- function(data) {
  data |>
    prep_carbon() |>
    estimate_carbon()
}