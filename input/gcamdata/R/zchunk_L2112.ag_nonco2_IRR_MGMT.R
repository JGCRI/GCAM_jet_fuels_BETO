# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L2112.ag_nonco2_IRR_MGMT
#'
#' Disaggregate non-CO2 agricultral emissions by production technology
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}, \code{L2112.AWBEmissions}, \code{L2112.AGREmissions}. The corresponding file in the
#' original data system was \code{L2112.ag_nonco2_IRR_MGMT.R} (emissions level2).
#' @details Disaggregates agricultural emissions the basis of production by scaling emissions by a technology factor
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr replace_na unite
#' @author KD July 2017
module_emissions_L2112.ag_nonco2_IRR_MGMT <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "aglu/Peatland_land_mapping",
             FILE = "aglu/EPA_peatland_EFs",
             FILE = "aglu/RTI_peatland_categories",
             "L2012.AgProduction_For",
             "L2012.AgProduction_Past",
             "L2111.AGREmissions",
             "L2111.AGRBio",
             "L2111.AWB_BCOC_EmissCoeff",
             "L2111.nonghg_max_reduction",
             "L2111.nonghg_steepness",
             "L2111.AWBEmissions",
             "L2252.AgProduction_ag_irr_mgmt",
             "L2052.AgCost_bio_irr_mgmt",
             "L222.LN2_MgdAllocation",
             "L2231.LN3_MgdAllocation_noncrop",
             "L2252.LN5_MgdAllocation_crop",
             "L2052.AgProdChange_ag_irr_ref"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2112.AGRBio",
             "L2112.AWB_BCOC_EmissCoeff",
             "L2112.nonghg_max_reduction",
             "L2112.nonghg_steepness",
             "L2112.AWBEmissions",
             "L2112.AGREmissions",
             "L2112.OutputEmissCoeff_ag_peat",
             "L2112.OutputEmissCoeff_For_peat",
             "L2112.OutputEmissCoeff_Past_peat"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    AgProductionTechnology <- level <- year <- region <- AgSupplySector <- AgSupplySubsector <-
      AgProductionTechnology_nolvl <- calOutputValue <- AgProductionTechnology_lvl <- total <-
      share_tech <- input.emissions <- Non.CO2 <- NULL  # silence package check notes

    # Load required inputs
    Peatland_land_mapping <- get_data(all_data, "aglu/Peatland_land_mapping")
    EPA_peatland_EFs <- get_data(all_data, "aglu/EPA_peatland_EFs")
    RTI_peatland_categories <- get_data(all_data, "aglu/RTI_peatland_categories")
    L2012.AgProduction_For <- get_data(all_data, "L2012.AgProduction_For")
    L2012.AgProduction_Past <- get_data(all_data, "L2012.AgProduction_Past")
    L2111.AWBEmissions <- get_data(all_data, "L2111.AWBEmissions")
    L2111.AGREmissions <- get_data(all_data, "L2111.AGREmissions")
    L2111.AGRBio <- get_data(all_data, "L2111.AGRBio")
    L2111.AWB_BCOC_EmissCoeff <- get_data(all_data, "L2111.AWB_BCOC_EmissCoeff")
    L2111.nonghg_max_reduction <- get_data(all_data, "L2111.nonghg_max_reduction")
    L2111.nonghg_steepness <- get_data(all_data, "L2111.nonghg_steepness")
    L2252.AgProduction_ag_irr_mgmt <- get_data(all_data, "L2252.AgProduction_ag_irr_mgmt", strip_attributes = TRUE)
    L2052.AgCost_bio_irr_mgmt <- get_data(all_data, "L2052.AgCost_bio_irr_mgmt")
    L222.LN2_MgdAllocation <- get_data(all_data, "L222.LN2_MgdAllocation")
    L2231.LN3_MgdAllocation_noncrop <- get_data(all_data, "L2231.LN3_MgdAllocation_noncrop")
    L2252.LN5_MgdAllocation_crop <- get_data(all_data, "L2252.LN5_MgdAllocation_crop")
    L2052.AgProdChange_ag_irr_ref <- get_data(all_data, "L2052.AgProdChange_ag_irr_ref")


    # #=======#=======#=======#=======#=======#=======#=========
    # For all of the ag emission tables, expand to management levels and peat soils (where appropriate)
    L2111.Bio_peat_table <- subset(L2052.AgCost_bio_irr_mgmt, endsWith(AgProductionTechnology, paste0(aglu.PEAT_DELIMITER, "Peat"))) %>%
      mutate(AgSupplySubsector = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgSupplySubsector),
             AgProductionTechnology = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgProductionTechnology),
             Soil_Type = "Peat") %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, Soil_Type) %>%
      distinct()

    L2111.Ag_peat_table <- subset(L2252.AgProduction_ag_irr_mgmt, endsWith(AgProductionTechnology, paste0(aglu.PEAT_DELIMITER, "Peat"))) %>%
      mutate(AgSupplySubsector = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgSupplySubsector),
             AgProductionTechnology = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgProductionTechnology),
             Soil_Type = "Peat") %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, Soil_Type) %>%
      distinct()

    L2111.AGRBio %>%
      repeat_add_columns(tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") %>%
      append_peat_technologies(L2111.Bio_peat_table) ->
      L2112.AGRBio

    L2111.AWB_BCOC_EmissCoeff %>%
      repeat_add_columns(tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") %>%
      append_peat_technologies(L2111.Ag_peat_table) ->
      L2112.AWB_BCOC_EmissCoeff

    L2111.nonghg_max_reduction %>%
      repeat_add_columns(tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") %>%
      append_peat_technologies(L2111.Ag_peat_table) ->
      L2112.nonghg_max_reduction

    L2111.nonghg_steepness %>%
      repeat_add_columns(tibble(level = c("hi", "lo"))) %>%
      unite(AgProductionTechnology, AgProductionTechnology, level, sep = "_") %>%
      append_peat_technologies(L2111.Ag_peat_table) ->
      L2112.nonghg_steepness


    # For the tables whose emissions are read as quantities rather than rates,
    # disaggregate emissions on the basis of production.

    # First calculate the total emissions for each region, supply sector, subsector,
    # technology level in the most recent model base year. This total will latter be
    # used to  calculate share weights.
    L2252.AgProduction_ag_irr_mgmt %>%
      mutate(AgSupplySubsector_nopeat = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgSupplySubsector),
             AgProductionTechnology_nolvl = gsub("_hi|_lo", "", AgProductionTechnology),
             AgProductionTechnology_nolvl = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgProductionTechnology_nolvl)) %>%
      group_by(region, AgSupplySector, AgSupplySubsector_nopeat, AgProductionTechnology_nolvl, year) %>%
      summarise(total = sum(calOutputValue)) %>%
      ungroup() ->
      L2112.AgProduction_ag_irr_nomgmt_aggregate

    # Now format the ag production data so that so that the subsector and technology categories can be
    # joined with the "total" data from the aggregated data frame above.
    L2252.AgProduction_ag_irr_mgmt %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, calOutputValue) %>%
      mutate(AgSupplySubsector_nopeat = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgSupplySubsector),
             AgProductionTechnology_nolvl = gsub("_hi|_lo", "", AgProductionTechnology),
             AgProductionTechnology_nolvl = sub(paste0(aglu.PEAT_DELIMITER, "Peat"), "", AgProductionTechnology_nolvl)) ->
      L2112.AgProduction_ag

    # Calculate the share weights or the fraction of emissions for each region, sector, subsector,
    # technology and level for the using the aggrated total emissions determined above.
    # Eventually these emission shares will be used as a factor to disaggregate the emissions input.
    # NaNs from aggregate techs whose output is zero can be safely re-set to zero (zero-output techs can't have emissions anyway)
    L2112.AgProduction_ag %>%
      left_join_error_no_match(L2112.AgProduction_ag_irr_nomgmt_aggregate,
                               by = c("region", "AgSupplySector", "year", "AgSupplySubsector_nopeat", "AgProductionTechnology_nolvl")) %>%
      mutate(share_tech = calOutputValue / total) %>%
      replace_na(list(share_tech = 0)) ->
      L2112.AgProduction_ag_share


    # Combine non agricultural waste burning emissions and agricultural waste burning
    # emissions into a single data frame. Add management level information to production
    # technology column.
    L2111.AWBEmissions %>%
      bind_rows(L2111.AGREmissions) %>%
      rename(AgSupplySubsector_nopeat = AgSupplySubsector,
             AgProductionTechnology_nolvl = AgProductionTechnology) ->
      L2112.awb_agr_emissions

    # Match the shares(fraction of emissions) to the data frames containing
    # emissions quantities for the agricultural water burning and agricultural
    # non waste burning emissions.
    L2112.AgProduction_ag_share %>%
      inner_join(L2112.awb_agr_emissions, by = c("region", "AgSupplySector", "AgSupplySubsector_nopeat", "AgProductionTechnology_nolvl", "year")) ->
      L2112.awb_agr_emissions

      # Multiply the input.emissions by the share allocated to each technology
    L2112.awb_agr_emissions %>%
      mutate(input.emissions  = input.emissions * share_tech) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology,
             year, Non.CO2, input.emissions) ->
      L2112.awb_agr_emissions_disag

    # The disaggregated agricultural waste burning emissions.
    L2112.awb_agr_emissions_disag %>%
      filter(grepl("AWB", Non.CO2)) ->
      L2112.AWBEmissions

    # The disaggregated non agricultural waste burning emissions.
    L2112.awb_agr_emissions_disag %>%
      filter(!grepl("AWB", Non.CO2)) ->
      L2112.AGREmissions

    # 8/6/2020 gpk - for all peatland technologies, read in exogenous coefficients
    # This is somewhat complicated. There's a question of whether these peatland emissions are in addition to the
    # already-estimated ones, versus replacing them. Also no technology can have both input-emissions and emiss-coef,
    # so ones in AGREmissions have to be replaced.
    L2112.Peat_EFs <- left_join(EPA_peatland_EFs, RTI_peatland_categories,
                                 by = "RTI_LUT") %>%
      drop_na(Intermediate_Land_Type) %>%
      left_join(Peatland_land_mapping, by = "Intermediate_Land_Type") %>%
      mutate(EF_kgm2 = EF * CONV_THA_KGM2)

    L2112.Peat_EFs_crop <- L2112.Peat_EFs %>%
      drop_na(GCAM_commodity) %>%
      select(AgSupplySector = GCAM_commodity, Non.CO2, EF_kgm2)

    # For non-crop lands that are in commercial production, there were no existing emissions coefficients or input-emissions
    # to replace. So, we don't even need to make the nonco2 objects for any zero-emiss-coef examples.
    L2112.Peat_EFs_noncrop <- L2112.Peat_EFs %>%
      filter(is.na(GCAM_commodity),
             EF_kgm2 > 0) %>%
      select(AgSupplySector = Land_Type, Non.CO2, EF_kgm2) %>% distinct()

    # These peat emissions factors are indicated per unit land area, whereas the coefs in the model are
    # indicated per unit crop production. Need to divide by the yield. Start with emissions table, join in
    # production, land area, and area-based emissions factors to compute the new emissions factors.
    L2112.OutputEmissCoeff_ag_peat <- filter(L2252.AgProduction_ag_irr_mgmt,
                                             grepl("_Peat", AgProductionTechnology),
                                             year == max(MODEL_BASE_YEARS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Production = calOutputValue) %>%
      inner_join(select(L2252.LN5_MgdAllocation_crop, region, LandLeaf, year, allocation),
                 by = c("region", AgProductionTechnology = "LandLeaf", "year")) %>%
      left_join(L2112.Peat_EFs_crop, by = "AgSupplySector") %>%
      mutate(year = min(MODEL_BASE_YEARS),
             Yield = if_else(Production == 0 & allocation == 0, 0, Production / allocation),
             emiss.coef = if_else(Yield == 0, 0, EF_kgm2 / Yield)) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeffAg"]])

    # Calculation of emission factors dividing the values by the base year yield do not consider the yield improvements over time.
    # We add this dynamic by dividing the computed base-year values by the exogenous agricultural productivity increase (L2052.AgProdChange_ag_irr_ref)

    L2112.OutputEmissCoeff_ag_peat %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      # use left_join because there AgProdChange starts in 2020
      left_join(L2052.AgProdChange_ag_irr_ref, by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      replace_na(list(AgProdChange = 1)) %>%
      mutate(AgProdChange_adj = if_else(year > MODEL_FINAL_BASE_YEAR, (1 + AgProdChange), AgProdChange )) %>%
      group_by(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, Non.CO2) %>%
      mutate(CumAgProdChange = cumprod(AgProdChange_adj)) %>%
      ungroup() %>%
      # adjust EFs based on their cumulative AgProdChange
      mutate(emiss.coef = emiss.coef / CumAgProdChange) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeffAg"]]) -> L2112.OutputEmissCoeff_ag_peat


# For existing input-emissions tables, remove the duplicates
    L2112.AGREmissions <- anti_join(L2112.AGREmissions, L2112.OutputEmissCoeff_ag_peat,
                                    by = c(LEVEL2_DATA_NAMES[["AgTech"]], "Non.CO2"))

    # Forest and pasture
    # The process here is similar to above, but the data come from more disparate sources
    L2112.OutputEmissCoeff_Past_peat <- filter(L2012.AgProduction_Past,
                                             grepl("_Peat", AgProductionTechnology),
                                             year == max(MODEL_BASE_YEARS),
                                             calOutputValue > 0) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Production = calOutputValue) %>%
      inner_join(select(L222.LN2_MgdAllocation, region, LandLeaf, year, allocation),
                 by = c("region", AgProductionTechnology = "LandLeaf", "year")) %>%
      left_join(L2112.Peat_EFs_noncrop, by = "AgSupplySector") %>%
      mutate(year = min(MODEL_BASE_YEARS),
             Yield = Production / allocation,
             emiss.coef = EF_kgm2 / Yield) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeffAg"]])

    L2112.OutputEmissCoeff_For_peat <- filter(L2012.AgProduction_For,
                                              grepl("_Peat", AgProductionTechnology),
                                              year == max(MODEL_BASE_YEARS),
                                              calOutputValue > 0) %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year, Production = calOutputValue) %>%
      inner_join(select(L2231.LN3_MgdAllocation_noncrop, region, LandLeaf, year, allocation),
                 by = c("region", AgProductionTechnology = "LandLeaf", "year")) %>%
      left_join(L2112.Peat_EFs_noncrop, by = "AgSupplySector") %>%
      mutate(year = min(MODEL_BASE_YEARS),
             Yield = Production / allocation,
             emiss.coef = EF_kgm2 / Yield) %>%
      select(LEVEL2_DATA_NAMES[["OutputEmissCoeffAg"]])

    # #=======#=======#=======#=======#=======#=======#=========
    # Produce outputs
    L2112.AGRBio %>%
      add_title("Bio N2O Coefficients by region, technology, and management level ") %>%
      add_units("kg N2O per GJ bioenergy") %>%
      add_comments("L211.AGRBio repeated by IRR and RFD technologies") %>%
      add_legacy_name("L2112.AGRBio") %>%
      add_precursors("L2111.AGRBio",
                     "L2052.AgCost_bio_irr_mgmt") ->
      L2112.AGRBio

    L2112.AWB_BCOC_EmissCoeff %>%
      add_title("Agricultural Waste Burning BC/OC Emissions Coefficients by management") %>%
      add_units("kt/Mt") %>%
      add_comments("L2111.AWB_BCOC_EmissCoeff repeated high and low management") %>%
      add_legacy_name("L2112.AWB_BCOC_EmissCoeff") %>%
      add_precursors("L2111.AWB_BCOC_EmissCoeff",
                     "L2252.AgProduction_ag_irr_mgmt") ->
      L2112.AWB_BCOC_EmissCoeff

    L2112.nonghg_max_reduction %>%
      add_title("Non-GHG maximum emissions coefficient reduction by agricultural technology and management") %>%
      add_units("Percent reduction from base-year emissions coefficient") %>%
      add_comments("L2111.nonghg_max_reduction repeated by high and low management") %>%
      add_legacy_name("L2112.nonghg_max_reduction") %>%
      add_precursors("L2111.nonghg_max_reduction",
                     "L2252.AgProduction_ag_irr_mgmt") ->
      L2112.nonghg_max_reduction

    L2112.nonghg_steepness %>%
      add_title("Steepness of non-GHG emissions reduction for agricultural technologies") %>%
      add_units("Unitless") %>%
      add_comments("L2111.nonghg_steepness repeated by high and low management level") %>%
      add_legacy_name("L2112.nonghg_steepness") %>%
      add_precursors("L2111.nonghg_steepness",
                     "L2252.AgProduction_ag_irr_mgmt") ->
      L2112.nonghg_steepness

    L2112.AWBEmissions %>%
      add_title("Input table of agricultural waste burning emissions by production") %>%
      add_units("Tg") %>%
      add_comments("Production share weights are set for irrigated vs. rainfed, same for high and low management.") %>%
      add_legacy_name("L2112.AWBEmissions") %>%
      add_precursors("L2111.AWBEmissions", "L2252.AgProduction_ag_irr_mgmt") ->
      L2112.AWBEmissions

    L2112.AGREmissions %>%
      add_title("Input table for the agricultural emissions by production") %>%
      add_units("Tg") %>%
      add_comments("Production share weights are set for irrigated vs. rainfed, same for high and low management.") %>%
      add_legacy_name("L2112.AGREmissions") %>%
      add_precursors("L2111.AGREmissions", "L2252.AgProduction_ag_irr_mgmt") ->
      L2112.AGREmissions

    L2112.OutputEmissCoeff_ag_peat %>%
      add_title("Table for peat soil emissions factors on cropland") %>%
      add_units("kg species / kg crop") %>%
      add_comments("Emissions coefficients for crop production on peatlands, as provided by Kemen Austin/RTI") %>%
      add_precursors("L2252.LN5_MgdAllocation_crop", "L2252.AgProduction_ag_irr_mgmt",
                     "aglu/Peatland_land_mapping", "aglu/EPA_peatland_EFs", "aglu/RTI_peatland_categories", "L2052.AgProdChange_ag_irr_ref") ->
      L2112.OutputEmissCoeff_ag_peat

    L2112.OutputEmissCoeff_For_peat %>%
      add_title("Table for peat soil emissions factors on managed forest") %>%
      add_units("kg species / m3 of roundwood") %>%
      add_comments("Emissions coefficients for roundwood production on peatlands, as provided by Kemen Austin/RTI") %>%
      add_precursors("L2231.LN3_MgdAllocation_noncrop", "L2012.AgProduction_For",
                     "aglu/Peatland_land_mapping", "aglu/EPA_peatland_EFs", "aglu/RTI_peatland_categories") ->
      L2112.OutputEmissCoeff_For_peat

    L2112.OutputEmissCoeff_Past_peat %>%
      add_title("Table for peat soil emissions factors on grazed pastures") %>%
      add_units("kg species / kg of forage") %>%
      add_comments("Emissions coefficients for forage production on peatlands, as provided by Kemen Austin/RTI") %>%
      add_precursors("L222.LN2_MgdAllocation", "L2012.AgProduction_Past",
                     "aglu/Peatland_land_mapping", "aglu/EPA_peatland_EFs", "aglu/RTI_peatland_categories") ->
      L2112.OutputEmissCoeff_Past_peat


    return_data(L2112.AGRBio, L2112.AWB_BCOC_EmissCoeff, L2112.nonghg_max_reduction, L2112.AWBEmissions,
                L2112.nonghg_steepness, L2112.AGREmissions, L2112.OutputEmissCoeff_ag_peat,
                L2112.OutputEmissCoeff_For_peat, L2112.OutputEmissCoeff_Past_peat)

  } else {
    stop("Unknown command")
  }
}
