# Function with allometric equations from Yepes et al.(2016)
# for Rhizophora mangle (the DAP is 30 cm above the las root)
# and Avicennia germinans, and Imbert and Rollet (1989) for Laguncularia racemosa
#The result is expressed in dry kg per unit of area.
#Max DBH values to fit are 40 for RM, 35 for Ag, 35 for Lr

Mangrove_biomass <- function(df, species_col, dbh_col) {

  Especie <- df[[species_col]]
  DBH <- df[[dbh_col]]

  ifelse(
    Especie == "Rhizophora mangle",
    exp(2.59 * log(DBH) - 1.91),
    ifelse(
      Especie == "Avicennia germinans",
      exp(2.45 * log(DBH) - 1.96),
      ifelse(
        Especie == "Laguncularia racemosa",
        0.209 * (DBH ^ 2.24),
        NA
      )
    )
  )
}
