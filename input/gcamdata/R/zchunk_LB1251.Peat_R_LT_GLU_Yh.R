#' module_aglu_LB1251.PeatFrac_R_LT_GLU_Yh
#'
#' Peatland versus mineral soil fractions by (selected) GCAM region / aggregate land type / historical year / GLU.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: (something).
#' @details Aggregate the \code{aglu/RTI_peatland_data} dataset to GCAM regions and use this to disaggregate land cover
#' by LUT into peatland versus mineral soil fractions. Returns the fractions and the soil carbon contents of peatlands.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread nesting
#' @author GPK April 2020
module_aglu_LB1251.PeatFrac_R_LT_GLU_Yh <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/Peatland_land_mapping",
             FILE = "aglu/Peatland_soil_carbon",
             FILE = "aglu/RTI_peatland_categories",
             FILE = "aglu/RTI_peatland_data",
             "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
             "L125.LC_bm2_R_LT_Yh_GLU"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1251.SoilTypeShare_R_Soil_LT_C_GLU",
             "L1251.PeatCarbonContent_kgm2_R_LT_C_GLU"))
  } else if(command == driver.MAKE) {

    iso <- GCAM_region_ID <- Land_Type <- year <- GLU <- Area_bm2 <- LT_HYDE <-
        land_code <- LT_SAGE <- NULL    # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs

    get_data(all_data, "common/iso_GCAM_regID") %>%
      select(iso, GCAM_region_ID) ->
      iso_GCAM_regID
    Peatland_land_mapping <- get_data(all_data, "aglu/Peatland_land_mapping")
    Peatland_soil_carbon <- get_data(all_data, "aglu/Peatland_soil_carbon")
    RTI_peatland_categories <- get_data(all_data, "aglu/RTI_peatland_categories")
    RTI_peatland_data <- get_data(all_data, "aglu/RTI_peatland_data")
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU", strip_attributes = TRUE)
    L125.LC_bm2_R_LT_Yh_GLU <- get_data(all_data, "L125.LC_bm2_R_LT_Yh_GLU", strip_attributes = TRUE)

    # Perform computations
    # Compile peatland land cover by GCAM region, GLU, and available land class
    L1251.PeatLand_bm2_R_LT_GLU <- RTI_peatland_data %>%
      gather(key = RTI_LUT, value = value, -iso, -glu_code, -soil_type) %>%
      replace_na(list(value = 0)) %>%
      inner_join(RTI_peatland_categories, by = "RTI_LUT") %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID), by = "iso") %>%
      mutate(GLU = paste(aglu.GLU, sprintf("%03d", glu_code), sep = aglu.GLU_NAME_DELIMITER)) %>%
      group_by(GCAM_region_ID, GLU, Intermediate_Land_Type) %>%
      summarise(peatland = sum(value) * CONV_ONES_THOUS) %>%
      ungroup()

    # Compile GCAM's land cover data on land cover by "intermediate" land cover class, used for the crosswalk between
    # the RTI data and GCAM data
    L1251.LC_bm2_R_HarvCropLand_C_GLU <- mutate(L122.LC_bm2_R_HarvCropLand_C_Yh_GLU, Land_Type = "HarvCropLand") %>%
      filter(year == max(HISTORICAL_YEARS))
    L1251.LC_bm2_R_LTint_GLU <- L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(year == max(HISTORICAL_YEARS),
             Land_Type != "HarvCropLand") %>%
      bind_rows(L1251.LC_bm2_R_HarvCropLand_C_GLU) %>%
      left_join_error_no_match(Peatland_land_mapping,
                               by = c("Land_Type", "GCAM_commodity"),
                               ignore_columns = "Intermediate_Land_Type") %>%
      drop_na(Intermediate_Land_Type) %>%
      group_by(GCAM_region_ID, GLU, Intermediate_Land_Type) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    # Compute the peat fraction by land use region and "intermediate" land cover class
    L1251.PeatFrac_R_LTint_GLU <- L1251.LC_bm2_R_LTint_GLU %>%
      inner_join(L1251.PeatLand_bm2_R_LT_GLU,
                 by = c("GCAM_region_ID", "Intermediate_Land_Type", "GLU")) %>%
      # where peatlands are more than the region/glu/LT, re-set it to the region/glu/LT total
      # 4/23/2019 gpk - this is only performed in two of 622 observations
      mutate(peatland = if_else(peatland > value, value, peatland),
             peatland_share = if_else(value == 0, 0, peatland / value)) %>%
      select(GCAM_region_ID, Intermediate_Land_Type, GLU, peatland_share)

    # Join in the peat fractions by land cover totals
    # Because the peat data are only provided for a recent year, there is no reason to keep all of the historical years
    # Also re-set the GCAM_commodity for the land types that produce an output but aren't on HarvCropLand (Forest, Pasture)
    L1251.LC_bm2_R_LT_C_GLU <- L125.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type != "HarvCropLand") %>%
      bind_rows(mutate(L122.LC_bm2_R_HarvCropLand_C_Yh_GLU, Land_Type = "HarvCropLand")) %>%
      filter(year == max(HISTORICAL_YEARS)) %>%
      left_join_error_no_match(Peatland_land_mapping,
                               by = c("Land_Type", "GCAM_commodity"),
                               ignore_columns = "Intermediate_Land_Type") %>%
      left_join(L1251.PeatFrac_R_LTint_GLU,
                by = c("GCAM_region_ID", "Intermediate_Land_Type", "GLU")) %>%
      mutate(GCAM_commodity = if_else(Land_Type %in% c("Forest", "Pasture"),
                                      Land_Type,
                                      GCAM_commodity))

    # Create the final data tables:
    #1. Share of peatland and mineral soil fractions.
    L1251.PeatShare_R_LT_C_GLU <- L1251.LC_bm2_R_LT_C_GLU %>%
      drop_na(peatland_share) %>%
      mutate(Soil_Type = "Peat",
             share = peatland_share)
    L1251.MineralShare_R_LT_C_GLU <- L1251.LC_bm2_R_LT_C_GLU %>%
      replace_na(list(peatland_share = 0)) %>%
      mutate(Soil_Type = "Mineral",
             share = 1 - peatland_share)
    L1251.SoilTypeShare_R_Soil_LT_C_GLU <-
      bind_rows(L1251.PeatShare_R_LT_C_GLU, L1251.MineralShare_R_LT_C_GLU) %>%
      select(GCAM_region_ID, Soil_Type, Land_Type, GCAM_commodity, GCAM_subsector, GLU, share)

    #2. Peatland soil carbon content
    # Only provided in region x glu x land types that have peatlands
    L1251.PeatCarbonContent_kgm2_R_LT_C_GLU <- L1251.LC_bm2_R_LT_C_GLU %>%
      filter(year == max(HISTORICAL_YEARS),
             !is.na(peatland_share)) %>%
      inner_join(Peatland_soil_carbon, by = "Land_Type") %>%
      mutate(soil_c = soil_c * CONV_THA_KGM2) %>%
      select(GCAM_region_ID, Land_Type, GCAM_commodity, GCAM_subsector, GLU, soil_c)

    # Produce outputs
    L1251.SoilTypeShare_R_Soil_LT_C_GLU %>%
      add_title("Soil type share (peat vs mineral) by GCAM region / soil type / land type / GCAM_commodity / GLU / year", overwrite = T) %>%
      add_units("billion square meters") %>%
      add_comments("Computed from peatland fractions from recent data (applied to all historical years)") %>%
      add_precursors("common/iso_GCAM_regID", "aglu/Peatland_land_mapping", "aglu/RTI_peatland_categories",
                     "aglu/RTI_peatland_data", "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "L125.LC_bm2_R_LT_Yh_GLU") ->
      L1251.SoilTypeShare_R_Soil_LT_C_GLU

    L1251.PeatCarbonContent_kgm2_R_LT_C_GLU %>%
      add_title("Peatland soil carbon content by GCAM region / land type / GCAM_commodity / GLU", overwrite = T) %>%
      add_units("kg C per m2") %>%
      add_comments("Vegetative carbon is assumed unaffected by whether on peat or mineral soil") %>%
      same_precursors_as(L1251.SoilTypeShare_R_Soil_LT_C_GLU) %>%
      add_precursors("aglu/Peatland_soil_carbon")->
      L1251.PeatCarbonContent_kgm2_R_LT_C_GLU

    return_data(L1251.SoilTypeShare_R_Soil_LT_C_GLU,
                L1251.PeatCarbonContent_kgm2_R_LT_C_GLU)
  } else {
    stop("Unknown command")
  }
}
