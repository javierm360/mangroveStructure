#' Estimate aboveground biomass of mangrove trees
#'
#' Estimates the aboveground dry biomass of individual mangrove trees
#' using species-specific allometric equations based on diameter at breast
#' height (DBH).
#'
#' For \emph{Rhizophora mangle} and \emph{Avicennia germinans}, the
#' equations are from Yepes et al. (2016). For
#' \emph{Laguncularia racemosa}, the equation is from Imbert and Rollet
#' (1989).
#'
#' DBH should be measured at 1.30 m above ground or, for
#' \emph{Rhizophora mangle} with aerial roots, 30 cm above the highest
#' aerial root.
#'
#' The equations estimate aboveground dry biomass for individual trees.
#' Biomass per unit area must be calculated separately by aggregating
#' individual tree biomass and accounting for the sampled area.
#'
#' @param df A data frame containing mangrove tree data.
#' @param species A character string giving the name of the column
#'   containing species names.
#' @param dbh A character string giving the name of the column containing
#'   diameter at breast height (DBH), in cm.
#'
#' @return A numeric vector containing estimated aboveground dry biomass
#'   for each tree, in kg tree-1. Values are returned in the same order
#'   as the rows of \code{df}. Species not included in the allometric
#'   equations return \code{NA}.
#'
#' @references
#' Yepes, A., Zapata, M., Bolivar, J., Monsalve, A., Espinosa, S. M.,
#' Sierra-Correa, P. C., & Sierra, A. (2016). Tree above-ground biomass
#' allometries for carbon stocks estimation in the Caribbean mangroves
#' in Colombia. Revista de Biología Tropical, 64(2), 913-926.
#' \doi{10.15517/rbt.v64i2.18141}
#'
#' Imbert, D., & Rollet, B. (1989). Phytomasse aérienne et production
#' primaire dans la mangrove de Guadeloupe. Annales des Sciences
#' Forestières, 46(1), 55-78.
#'
#' @examples
#' mangrove_data <- data.frame(
#'   species = c(
#'     "Rhizophora mangle",
#'     "Avicennia germinans",
#'     "Laguncularia racemosa"
#'   ),
#'   dbh = c(10, 15, 12)
#' )
#'
#' Mangrove_biomass(
#'   df = mangrove_data,
#'   species = "species",
#'   dbh = "dbh"
#' )
#'
#' @export

Mangrove_biomass <- function(df,
                             species = "species",
                             dbh = "dbh") {

  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }

  if (!is.character(species) || length(species) != 1) {
    stop("'species' must be a single character string.")
  }

  if (!is.character(dbh) || length(dbh) != 1) {
    stop("'dbh' must be a single character string.")
  }

  if (!species %in% names(df)) {
    stop("The species column specified by 'species' was not found in 'df'.")
  }

  if (!dbh %in% names(df)) {
    stop("The DBH column specified by 'dbh' was not found in 'df'.")
  }

  Especie <- as.character(df[[species]])
  DBH <- df[[dbh]]

  if (!is.numeric(DBH)) {
    stop("The DBH column must be numeric.")
  }

  if (any(DBH < 0, na.rm = TRUE)) {
    stop("DBH values must be greater than or equal to zero.")
  }

  biomass <- rep(NA_real_, length(DBH))

  biomass[Especie == "Rhizophora mangle"] <-
    exp(2.59 * log(DBH[Especie == "Rhizophora mangle"]) - 1.91)

  biomass[Especie == "Avicennia germinans"] <-
    exp(2.45 * log(DBH[Especie == "Avicennia germinans"]) - 1.96)

  biomass[Especie == "Laguncularia racemosa"] <-
    0.209 * DBH[Especie == "Laguncularia racemosa"]^2.24

  biomass
}
