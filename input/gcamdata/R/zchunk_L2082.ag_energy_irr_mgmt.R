#' module_aglu_L2082.ag_energy_irr_mgmt
#'
#' Specifies energy (liquid fuel) demand coefficients for all ag technologies; adjusts nonLandVariableCost to remove liquid fuel cost.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2082.AgCoef_en_ag_irr_mgmt}, \code{L2082.AgCoef_en_bio_irr_mgmt}, \code{L2082.AgCost_ag_irr_mgmt_adj}, \code{L2082.AgCost_bio_irr_mgmt_adj}.
#' @details This chunk assigns the energy demand quantities energy/industry (L132) to all agricultural technologies.
#' Adjust nonLandVariableCost to remove the now explicitly computed energy cost.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by_at mutate select
#' @author GPK March 2019
module_aglu_L2082.ag_energy_irr_mgmt <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c( FILE = "common/GCAM_region_names",
              FILE = "water/basin_to_country_mapping",
              "L2324.BaseService_Off_road",
              "L101.ag_Prod_Mt_R_C_Y_GLU",
              "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
              "L2252.AgProduction_ag_irr_mgmt",
              "L2052.AgProdChange_ag_irr_ref",
              "L2062.AgCoef_Fert_bio_irr_mgmt"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2082.AgCoef_en_ag_irr_mgmt",
             "L2082.AgCoef_en_bio_irr_mgmt",
             "L2082.BaseService_agEn"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # silence package check notes
    year <- value <- GCAM_region_ID <- energy_EJ <- LC_bm2 <- area_coef <- GCAM_commodity <-
      Prod_Mt <- region <- minicam.energy.input <- coefficient <- AgProductionTechnology <-
      level <- calOutputValue <- output_hi <- output_lo <- output_tot <- coef_lo <- coef_hi <-
      future_year <- AgSupplySector <- base_coef <- AgProdChange <- AgYieldRatio <-
      CumAgYieldRatio <- max_coef <- max_global_coef <-nonLandVariableCost <- fuel_cost <- NULL

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    L2324.BaseService_Off_road <- get_data(all_data, "L2324.BaseService_Off_road", strip_attributes = TRUE)
    L101.ag_Prod_Mt_R_C_Y_GLU <- get_data(all_data, "L101.ag_Prod_Mt_R_C_Y_GLU", strip_attributes = TRUE)
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU <- get_data(all_data, "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU", strip_attributes = TRUE)
    L2252.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2252.AgProduction_ag_irr_mgmt", strip_attributes = TRUE)
    L2052.AgProdChange_ag_irr_ref <- get_data(all_data, "L2052.AgProdChange_ag_irr_ref", strip_attributes = TRUE)
    L2062.AgCoef_Fert_bio_irr_mgmt <- get_data(all_data, "L2062.AgCoef_Fert_bio_irr_mgmt", strip_attributes = TRUE)

    # Calculate agricultural energy output using calibrated inputs * coefficients
    L2082.out_EJ_R_agenergy_F_Yh <- L2324.BaseService_Off_road %>%
      filter(energy.final.demand == "agricultural energy use") %>%
      mutate(value = base.service * energy.CROP_ENERGY_RATIO) %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    # calculate ag energy per unit land area, for allocating energy to crop types
    # assumption is that all crops have the same energy requirements per unit land area
    L2082.LC_bm2_R_Cropland <- L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(LC_bm2 = sum(value)) %>%
      ungroup()

    L2082.agenergyIO_F_Yh <- filter(L2082.out_EJ_R_agenergy_F_Yh, year %in% MODEL_BASE_YEARS) %>%
      group_by(GCAM_region_ID, year) %>%
      summarise(energy_EJ = sum(value)) %>%
      ungroup %>%
      left_join_error_no_match(L2082.LC_bm2_R_Cropland, by = c("GCAM_region_ID", "year")) %>%
      mutate(area_coef = energy_EJ / LC_bm2)

    # 3/6/2019: agricultural energy use per unit cropland is very high in some regions; South Korea, Japan, Northern
    # Africa and Colombia all have more than 2x as much energy per unit cropland as the USA. This is likely not actually
    # correct, and would cause issues later on if not addressed up front. Specifically, fuel costs would be very high,
    # potentially causing negative values for other production costs, profit rates, or both. The method below assigns
    # a maximum of the USA's energy per unit cropland times an exogenous multiplier.
    L2082.agenergyIO_F_Yh <- L2082.agenergyIO_F_Yh %>%
      group_by(year) %>%
      mutate(area_coef = if_else(area_coef > area_coef[ GCAM_region_ID == gcam.USA_CODE] * energy.MAX_EN_PER_CRPLND_RATIO_USA,
                                    area_coef[ GCAM_region_ID == gcam.USA_CODE] * energy.MAX_EN_PER_CRPLND_RATIO_USA,
                                    area_coef)) %>%
      ungroup()

    # Calculate base service we are actually accounting for here by multiplying adjusted coefficients by land
    L2082.BaseServiceAdj_agenergy <- L2082.agenergyIO_F_Yh %>%
      mutate(service_adj = LC_bm2 * area_coef) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, year, service_adj)

    # Adjust L2324.BaseService_Off_road based off of the base service we are applying to crops
    L2082.BaseService_agEn <- L2324.BaseService_Off_road  %>%
      filter(energy.final.demand == "agricultural energy use") %>%
      left_join_error_no_match(L2082.BaseServiceAdj_agenergy, by = c("region", "year")) %>%
      mutate(base.service = base.service - service_adj) %>%
      select(-service_adj)

    # Aggregate the land use regions, in order to get the coefficients per crop
    L2082.ag_Prod_Mt_R_C_Y <- filter(L101.ag_Prod_Mt_R_C_Y_GLU, year %in% MODEL_BASE_YEARS) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(Prod_Mt = sum(value)) %>%
      ungroup()

    # Use the area coefs to get ag energy by crop, and calculate the energy IO coefficient by crop (GJ/kg)
    L2082.agenergyIO_R_C_Yh <- filter(L122.LC_bm2_R_HarvCropLand_C_Yh_GLU, year %in% MODEL_BASE_YEARS) %>%
      rename(LC_bm2 = value) %>%
      left_join_error_no_match(select(L2082.agenergyIO_F_Yh, GCAM_region_ID, year, area_coef),
                                by = c("GCAM_region_ID", "year")) %>%
      mutate(energy_EJ = area_coef * LC_bm2) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(energy_EJ = sum(energy_EJ)) %>%
      ungroup() %>%
      left_join_error_no_match(L2082.ag_Prod_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "year")) %>%
      mutate(minicam.energy.input = "agricultural energy use",
             coefficient = if_else(Prod_Mt > 0, energy_EJ / Prod_Mt, 0)) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, AgSupplySector = "GCAM_commodity", "GCAM_subsector",year, minicam.energy.input, coefficient)

    # The coefficients are now ready to be applied to the technology level.
    # Hi and lo tech levels are assigned different coefficients, based on an exogenous multiplier
    # These technologies are specifically disaggregated here
    L2082.AgProduction_ag_irr_mgmt <- L2252.AgProduction_ag_irr_mgmt %>%
      mutate(level = if_else(grepl("_hi", AgProductionTechnology), "output_hi", "output_lo"),
             AgProductionTechnology = sub("_hi", "", AgProductionTechnology),
             AgProductionTechnology = sub("_lo", "", AgProductionTechnology),
             GCAM_subsector = sub("_.*", "", AgSupplySubsector ) ) %>%
      select(LEVEL2_DATA_NAMES[["AgTechYr"]], GCAM_subsector, level, calOutputValue) %>%
      spread(key = level, value = calOutputValue)

    L2082.AgCoef_en_ag_irr_mgmt <- L2082.AgProduction_ag_irr_mgmt %>%
      left_join_error_no_match(L2082.agenergyIO_R_C_Yh, by = c("region", "AgSupplySector", "GCAM_subsector", "year")) %>%
      mutate(output_tot = output_hi + output_lo,
             coef_lo = (coefficient * output_tot) / (aglu.FUEL_IO_RATIO_HI_LO * output_hi + output_lo),
             coef_hi = coef_lo * aglu.FUEL_IO_RATIO_HI_LO,
             Soil_Type = if_else(endsWith(AgProductionTechnology, "Peat"), "Peat", "Mineral"),
             AgProductionTechnology = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgProductionTechnology)) %>%
      replace_na(list(coef_lo = 0, coef_hi = 0)) %>%
      repeat_add_columns(tibble(level = c("hi", "lo"))) %>%
      mutate(coefficient = round(if_else(level == "hi", coef_hi, coef_lo), aglu.DIGITS_CALOUTPUT + 1),
             AgProductionTechnology = paste(AgProductionTechnology, level, sep = aglu.MGMT_DELIMITER),
             AgProductionTechnology = if_else(Soil_Type == "Peat",
                                              paste0(AgProductionTechnology, aglu.PEAT_DELIMITER, Soil_Type),
                                              AgProductionTechnology)) %>%
      select(LEVEL2_DATA_NAMES[["AgCoef"]], GCAM_subsector)

    # For future adjustment of energy-related coefs, make each region/crop/year increase towards the maximum observed
    # value for the region/crop in the USA in the base year. For now, the coefs are increased at the same rate as the ag
    # productivity change (until they reach the assumed max value). Values > USA are held constant.
    L2082.USAMaxCoef <- filter(L2082.AgCoef_en_ag_irr_mgmt, year == max(MODEL_BASE_YEARS) & region == gcam.USA_REGION) %>%
      group_by(AgSupplySector, GCAM_subsector) %>%
      summarise(max_coef = max(coefficient)) %>%
      ungroup()

    L2082.GlobalMaxCoef <- filter(L2082.AgCoef_en_ag_irr_mgmt, year == max(MODEL_BASE_YEARS)) %>%
      group_by(AgSupplySector, GCAM_subsector) %>%
      summarise(max_global_coef = max(coefficient)) %>%
      ungroup()

    L2082.AgCoef_en_ag_irr_mgmt_fut <- filter(L2082.AgCoef_en_ag_irr_mgmt, year == max(MODEL_BASE_YEARS)) %>%
      # left_join because no PalmFruit in USA
      left_join(L2082.USAMaxCoef, by = c("AgSupplySector", "GCAM_subsector")) %>%
      left_join_error_no_match(L2082.GlobalMaxCoef, by = c("AgSupplySector", "GCAM_subsector")) %>%
      mutate(max_coef = if_else(is.na(max_coef), max_global_coef, max_coef)) %>%
      repeat_add_columns(tibble(future_year = c(max(MODEL_BASE_YEARS), MODEL_FUTURE_YEARS))) %>%
      select(-year, -max_global_coef) %>%
      rename(year = future_year,
             base_coef = coefficient) %>%
      left_join_error_no_match(L2052.AgProdChange_ag_irr_ref, by = LEVEL2_DATA_NAMES[["AgTechYr"]],
                               ignore_columns = "AgProdChange") %>%
      mutate(AgYieldRatio = (1 + AgProdChange)^(year - lag(year))) %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      group_by_at(c(LEVEL2_DATA_NAMES[["AgTech"]], "minicam.energy.input")) %>%
      mutate(CumAgYieldRatio = cumprod(AgYieldRatio)) %>%
      ungroup() %>%
      mutate(coefficient = if_else(base_coef * CumAgYieldRatio > max_coef,  base_coef, base_coef * CumAgYieldRatio)) %>%
      select(LEVEL2_DATA_NAMES[["AgCoef"]])

    L2082.AgCoef_en_ag_irr_mgmt <- bind_rows(L2082.AgCoef_en_ag_irr_mgmt, L2082.AgCoef_en_ag_irr_mgmt_fut) %>%
      select(-GCAM_subsector)

    # calculate the bioenergy energy input requirement
    bio_en_IO <- aglu.BIO_FUEL_INPUT_GJHA / aglu.BIO_YIELD_THA / aglu.BIO_ENERGY_CONTENT_GJT

    # Table L2082.AgCoef_en_bio_irr_mgmt: Energy input-output coefficients by region / bioenergy crop / year / GLU
    # For bioenergy crops, there isn't any need to worry about "output"
    # The equation for coef_lo simplifies to coef_avg * 2 / (Ratio + 1). coef_hi is twice this (4 / (Ratio + 1))

    L2082.AgCoef_en_bio_irr_mgmt <- L2062.AgCoef_Fert_bio_irr_mgmt %>%
      mutate(minicam.energy.input = "agricultural energy use",
             coefficient = round(if_else(grepl("_lo", AgProductionTechnology),
                                         bio_en_IO * 2 / (aglu.FUEL_IO_RATIO_HI_LO + 1),
                                         bio_en_IO * 4 / (aglu.FUEL_IO_RATIO_HI_LO + 1)),
                                 aglu.DIGITS_CALOUTPUT))

    # Produce outputs
    L2082.AgCoef_en_ag_irr_mgmt %>%
      add_title("Energy input-output coefficients for agricultural technologies") %>%
      add_units("GJ per kg crop") %>%
      add_comments("Note: we are using the exogenously differentiated coefficients for hi versus lo management techs") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping", "L2324.BaseService_Off_road",
                     "L101.ag_Prod_Mt_R_C_Y_GLU", "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
                     "L2252.AgProduction_ag_irr_mgmt", "L2052.AgProdChange_ag_irr_ref") ->
      L2082.AgCoef_en_ag_irr_mgmt

    L2082.AgCoef_en_bio_irr_mgmt %>%
      add_title("Energy input-output coefficients for bioenergy technologies") %>%
      add_units("unitless IO") %>%
      add_comments("Bioenergy crop energy IO coefficients are exogenous") %>%
      add_precursors("L2062.AgCoef_Fert_bio_irr_mgmt") ->
      L2082.AgCoef_en_bio_irr_mgmt

    L2082.BaseService_agEn  %>%
      add_title("Revised base service for agricultural energy use") %>%
      add_units("EJ") %>%
      add_comments("Counter-balances the effects of explicitly including agricultural energy use as input to crop production") %>%
      add_precursors("L2324.BaseService_Off_road", "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU", "common/GCAM_region_names")  ->
      L2082.BaseService_agEn


    return_data(L2082.AgCoef_en_ag_irr_mgmt,
                L2082.AgCoef_en_bio_irr_mgmt,
                L2082.BaseService_agEn)
  } else {
    stop("Unknown command")
  }
}
