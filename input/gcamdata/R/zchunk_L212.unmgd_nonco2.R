# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L212.unmgd_nonco2
#'
#' Outputs unmanaged land emissions and emissions coefficients for forest fires, deforestation, grassland fires and fixed emissions from unmanaged pasture on peat soils.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs:\code{L212.AgSupplySector}, \code{L212.AgSupplySubsector}, \code{L212.ItemName},
#' \code{L212.GRASSEmissions}, \code{L212.FORESTEmissions_FF}, \code{L212.FORESTEmissions_D}, \code{L212.GRASSEmissionsFactors_BCOC},
#' \code{L212.FORESTEmissionsFactors_BCOC_FF}, \code{L212.FORESTEmissionsFactors_BCOC_D}, \code{L212.FORESTEmissionsFactors_future},
#' \code{L212.ItemName_prot}, \code{L212.GRASSEmissions_prot}, \code{L212.GRASSEmissions_noprot}, \code{L212.FORESTEmissions_FF_prot},
#' \code{L212.FORESTEmissions_FF_noprot}, \code{L212.FORESTEmissions_D_prot}, \code{L212.FORESTEmissions_D_noprot}, \code{L212.GRASSEmissionsFactors_BCOC_prot},
#' \code{L212.GRASSEmissionsFactors_BCOC_noprot}, \code{L212.FORESTEmissionsFactors_BCOC_FF_prot}, \code{L212.FORESTEmissionsFactors_BCOC_FF_noprot},
#' \code{L212.FORESTEmissionsFactors_BCOC_D_prot}, \code{L212.FORESTEmissionsFactors_BCOC_D_noprot}, \code{L212.PASTUREEmissionsFactors_Peat},
#' \code{L212.PASTUREEmissionsFactors_Peat_prot}, \code{L212.PASTUREEmissionsFactors_Peat_noprot}. The corresponding file in the
#' original data system was \code{L212.unmgd_nonco2.R} (emissions level2).
#' @details Outputs unmanaged land emissions and emissions coefficients for forest fires, deforestation, grassland fires, and fixed emissions from unmanaged pasture on peat soils.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter left_join mutate select
#' @author RLH August 2017
module_emissions_L212.unmgd_nonco2 <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "water/basin_to_country_mapping",
             FILE = "emissions/A_regions",
             FILE = "aglu/EPA_peatland_EFs",
             FILE = "aglu/RTI_peatland_categories",
             "L124.nonco2_tg_R_grass_Y_GLU",
             "L124.nonco2_tg_R_forest_Y_GLU",
             "L124.deforest_coefs",
             "L125.bcoc_tgbkm2_R_grass_2000",
             "L125.bcoc_tgbkm2_R_forest_2000",
             "L125.deforest_coefs_bcoc",
             "L1251.SoilTypeShare_R_Soil_LT_C_GLU",
             "L126.ProtectFrac_R_LT_GLU",
             "L222.LN2_Unmgd_PeatSoilCarbon"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L212.AgSupplySector",
             "L212.AgSupplySubsector",
             "L212.ItemName",
             "L212.GRASSEmissions",
             "L212.FORESTEmissions_FF",
             "L212.FORESTEmissions_D",
             "L212.GRASSEmissionsFactors_BCOC",
             "L212.FORESTEmissionsFactors_BCOC_FF",
             "L212.FORESTEmissionsFactors_BCOC_D",
             "L212.FORESTEmissionsFactors_future",
             "L212.ItemName_prot",
             "L212.GRASSEmissions_prot",
             "L212.GRASSEmissions_noprot",
             "L212.FORESTEmissions_FF_prot",
             "L212.FORESTEmissions_FF_noprot",
             "L212.FORESTEmissions_D_prot",
             "L212.FORESTEmissions_D_noprot",
             "L212.GRASSEmissionsFactors_BCOC_prot",
             "L212.GRASSEmissionsFactors_BCOC_noprot",
             "L212.FORESTEmissionsFactors_BCOC_FF_prot",
             "L212.FORESTEmissionsFactors_BCOC_FF_noprot",
             "L212.FORESTEmissionsFactors_BCOC_D_prot",
             "L212.FORESTEmissionsFactors_BCOC_D_noprot",
             "L212.FORESTEmissionsFactors_future_prot",
             "L212.PASTUREEmissionsFactors_Peat",
             "L212.PASTUREEmissionsFactors_Peat_prot",
             "L212.PASTUREEmissionsFactors_Peat_noprot"))
  } else if(command == driver.MAKE) {

    # Silencing package checks
    GCAM_region_ID <- region <- AgSupplySubsector <- GLU <- itemName <- AgSupplySector <-
      UnmanagedLandTechnology <- year <- value <- Non.CO2 <- em_factor <- technology <-
      emiss.coef <- input.emissions <- input.name <- Land_Type <- prot_frac <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names", strip_attributes = TRUE)
    basin_to_country_mapping <- get_data(all_data, "water/basin_to_country_mapping")
    A_regions <- get_data(all_data, "emissions/A_regions")
    L124.nonco2_tg_R_grass_Y_GLU <- get_data(all_data, "L124.nonco2_tg_R_grass_Y_GLU", strip_attributes = TRUE) %>%
      replace_GLU(basin_to_country_mapping)
    L124.nonco2_tg_R_forest_Y_GLU <- get_data(all_data, "L124.nonco2_tg_R_forest_Y_GLU", strip_attributes = TRUE) %>%
      replace_GLU(basin_to_country_mapping)
    L124.deforest_coefs <- get_data(all_data, "L124.deforest_coefs", strip_attributes = TRUE)
    L125.bcoc_tgbkm2_R_grass_2000 <- get_data(all_data, "L125.bcoc_tgbkm2_R_grass_2000", strip_attributes = TRUE)
    L125.bcoc_tgbkm2_R_forest_2000 <- get_data(all_data, "L125.bcoc_tgbkm2_R_forest_2000", strip_attributes = TRUE)
    L125.deforest_coefs_bcoc <- get_data(all_data, "L125.deforest_coefs_bcoc", strip_attributes = TRUE)
    L1251.SoilTypeShare_R_Soil_LT_C_GLU <- get_data(all_data, "L1251.SoilTypeShare_R_Soil_LT_C_GLU", strip_attributes = TRUE)
    L126.ProtectFrac_R_LT_GLU <- get_data(all_data, "L126.ProtectFrac_R_LT_GLU", strip_attributes = TRUE)
    L222.LN2_Unmgd_PeatSoilCarbon <- get_data(all_data, "L222.LN2_Unmgd_PeatSoilCarbon", strip_attributes = TRUE)
    EPA_peatland_EFs <- get_data(all_data, "aglu/EPA_peatland_EFs")
    RTI_peatland_categories <- get_data(all_data, "aglu/RTI_peatland_categories")

    # ===================================================
    # Unmanaged land sector info
    # L212.AgSupplySector: Sector info for unmanaged land technology
    # NOTE: only making unmanaged land sectors in regions that have aglu related emissions
    # Also note, not including the BC/OC tables here, which are from a different data source, and have no GLU detail
    # If regions/LTs have default BC/OC emissions factors but don't have any other gases (from EDGAR), they are dropped
    L212.regions <- unique(c(L124.nonco2_tg_R_grass_Y_GLU$GCAM_region_ID, L124.nonco2_tg_R_forest_Y_GLU$GCAM_region_ID))
    L212.regions <- GCAM_region_names %>%
      filter(GCAM_region_ID %in% L212.regions | region %in% L222.LN2_Unmgd_PeatSoilCarbon$region) %>%
      select(region)

    L212.AgSupplySector <- L212.regions %>%
      mutate(AgSupplySector = "UnmanagedLand",
             output.unit = "none",
             input.unit = "thous km2",
             price.unit = "none",
             calPrice = -1,
             market = region,
             logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = -3,
             # Not in old-data-system, using because of removal of get_logit_fn_tables
             logit.type = NA)

    # L212.ItemName: Land item to relate emissions to
    # Note: only making unmanaged land technologies in the regions/glus with the corresponding land use type available
    L212.ItemName_R_LT_GLU <- bind_rows(unique(L124.nonco2_tg_R_grass_Y_GLU[c("GCAM_region_ID", "Land_Type", "GLU")]),
                                     unique(L124.nonco2_tg_R_forest_Y_GLU[c("GCAM_region_ID", "Land_Type", "GLU")]))

    #10/21/2020 nam: adding unmanaged pasture emissions from different source
    L212.UnmanagedPasture_ItemName <- L222.LN2_Unmgd_PeatSoilCarbon %>%
      mutate(AgSupplySector = "UnmanagedLand",
             AgSupplySubsector = UnmanagedLandLeaf,
             UnmanagedLandTechnology = UnmanagedLandLeaf,
             itemName = UnmanagedLandLeaf) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, itemName)

    L212.ItemName <- tibble(AgSupplySector = "UnmanagedLand",
                            AgSupplySubsector = c("ForestFire", "Deforest", "GrasslandFires"),
                            itemName = c("UnmanagedForest", "UnmanagedForest", "Grassland")) %>%
      # Add in region and GLU
      left_join(L212.ItemName_R_LT_GLU, by = c("itemName" = "Land_Type")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(AgSupplySubsector = paste(AgSupplySubsector, GLU, sep = "_"),
             UnmanagedLandTechnology = AgSupplySubsector,
             itemName = paste(itemName, GLU, sep = "_")) %>%
      # Repeat for base years
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, itemName) %>%
      bind_rows(L212.UnmanagedPasture_ItemName)

    # PEATLANDS DISAGGREGATION
    # Make the tables with relevant peatland information for land shares
    L1251.SoilTypeShare_R_Soil_LT_C_GLU <- L1251.SoilTypeShare_R_Soil_LT_C_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(is.na(GCAM_commodity)) %>%
      replace_GLU(map = basin_to_country_mapping)

    #Formatting table for use with apply_peat_landshares(). Peat soil shares are the same between protected and unprotected land on the same land type/GLU
    L1251.SoilTypeShare_R_Soil_itemName <- bind_rows(L1251.SoilTypeShare_R_Soil_LT_C_GLU %>% mutate(itemName = paste(Land_Type, GLU, sep=aglu.LT_GLU_DELIMITER)),
                                                     L1251.SoilTypeShare_R_Soil_LT_C_GLU %>% mutate(itemName = paste0('Protected', Land_Type, aglu.LT_GLU_DELIMITER, GLU)))

    #Unmanaged pasture emissions
    # L212.PASTUREEmissionsFactors_Peat: Constant emissions from unmanaged pasture
    #10/21/2020 nam: adding unmanaged pasture emissions from different source
    L212.PASTUREEmissionsFactors_Peat <- left_join(EPA_peatland_EFs, RTI_peatland_categories,
                                                   by = "RTI_LUT") %>%
      filter(Intermediate_Land_Type == "UnmanagedPasture") %>%
      mutate(emiss.coef = EF * CONV_THA_KGM2) %>%
      mutate(AgSupplySector = 'UnmanagedLand') %>%
      select(AgSupplySector, Non.CO2, emiss.coef) %>%
      left_join(L212.UnmanagedPasture_ItemName, by = 'AgSupplySector') %>%
      #all of these emissions only apply to peatland
      mutate(AgSupplySubsector = paste(AgSupplySubsector, "Peat", sep = "_")) %>%
      mutate(UnmanagedLandTechnology = paste(UnmanagedLandTechnology, "Peat", sep = "_")) %>%
      mutate(input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, Non.CO2, emiss.coef,input.name)


    # Grassland emissions
    # L212.GrassEmissions: Grassland fire emissions in all regions
    # 9/21/2020 gpk - keep the "GLU" column at the end for later joining in protected land fractions
    L212.GRASSEmissions <- L124.nonco2_tg_R_grass_Y_GLU %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      mutate(value = round(value, emissions.DIGITS_EMISSIONS),
             AgSupplySector = "UnmanagedLand",
             AgSupplySubsector = paste("GrasslandFires", GLU, sep = "_"),
             UnmanagedLandTechnology = AgSupplySubsector,
             input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename_SO2(A_regions) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, Non.CO2, input.emissions = value, input.name,GLU) %>%
      left_join_error_no_match(L212.ItemName, by=c("region","AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year")) %>%
      apply_peat_emissions(L1251.SoilTypeShare_R_Soil_itemName) %>%
      select(-itemName)

    # Grassland emissions factors for BC/OC
    # L212.GrassEmissions: Grassland fire emissions factors for BC/OC in all regions
    L212.GRASSEmissionsFactors_BCOC <- L212.GRASSEmissions %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year) %>%
      distinct() %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L125.bcoc_tgbkm2_R_grass_2000$Non.CO2))) %>%
      # Add in GCAM_region_ID for joining
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      # Join in emission factor values
      left_join(L125.bcoc_tgbkm2_R_grass_2000, by = c("Non.CO2", "GCAM_region_ID","year")) %>%
      mutate(em_factor=if_else(is.na(em_factor),0,round(em_factor, emissions.DIGITS_EMISSIONS)),input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, Non.CO2, emiss.coef = em_factor, input.name)

    # L212.Forest: Forest fire and deforestation emissions in all regions
    # Will split up by technology in final product creation
    # 9/21/2020 gpk - keep the "GLU" column at the end for later joining in protected land fractions
    L212.FOREST <- L124.nonco2_tg_R_forest_Y_GLU %>%
      filter(year %in% emissions.MODEL_BASE_YEARS) %>%
      filter(year<= max(emissions.DEFOREST_COEF_YEARS)) %>%
      mutate(value = round(value, emissions.DIGITS_EMISSIONS),
             AgSupplySector = "UnmanagedLand",
             AgSupplySubsector = paste(technology, GLU, sep = "_"),
             UnmanagedLandTechnology = AgSupplySubsector) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      rename_SO2(A_regions) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, technology, year, Non.CO2, input.emissions = value, GLU) %>%
      left_join_error_no_match(L212.ItemName, by=c("region","AgSupplySector", "AgSupplySubsector", "UnmanagedLandTechnology", "year")) %>%
      apply_peat_emissions(L1251.SoilTypeShare_R_Soil_itemName) %>%
      select(-itemName)


    # L212.FORESTEmissionsFactors_BCOC: Forest emissions factors for BC/OC in all regions
    # Will split up by technology in final product creation
    L212.FORESTEmissionsFactors_BCOC <- L212.FOREST %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, technology, year) %>%
      distinct() %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L125.bcoc_tgbkm2_R_forest_2000$Non.CO2))) %>%
      # Add in GCAM_region_ID for joining
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      # Join in emission factor values
      left_join(L125.bcoc_tgbkm2_R_forest_2000, by = c("Non.CO2", "technology", "GCAM_region_ID")) %>%
      mutate(em_factor = if_else(is.na(em_factor),0,round(em_factor, emissions.DIGITS_EMISSIONS))) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, Non.CO2, emiss.coef = em_factor, technology)

    # Reading in default emissions factors for deforestation in future periods.
    # Because the model's driver for deforestation is not necessarily linked to the estimated quantity of emissions,
    # the deforestation-related emissions in the base years may be zero (regions with net afforestation), or very high
    # (regions with slightly net deforestation). This method just uses a global average for future non-co2 emissions
    # from deforestation
    L212.default_coefs <- bind_rows(select(L124.deforest_coefs, Non.CO2, emiss.coef),
                                    select(L125.deforest_coefs_bcoc, Non.CO2, emiss.coef))

    L212.FORESTEmissionsFactors_future <- L212.FOREST %>%
      filter(technology == "Deforest") %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology) %>%
      distinct() %>%
      # Take minimum model year greater than emissions model base years
      mutate(year = as.integer(min(MODEL_YEARS[MODEL_YEARS > max(emissions.DEFOREST_COEF_YEARS)]))) %>%
      repeat_add_columns(L212.default_coefs) %>%
      mutate(emiss.coef = round(emiss.coef, emissions.DIGITS_EMISSIONS)) %>%
      rename_SO2(A_regions) %>%
      select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, Non.CO2, emiss.coef)

    # We do not actually care about the logit here but we need a value to avoid errors
    #using partial code of apply_peat_emissions to append _Peat to appropriate subsectors
    L212.AgSupplySubsector <- L212.ItemName %>%
      left_join(L1251.SoilTypeShare_R_Soil_itemName, by=intersect(names(L212.ItemName), names(L1251.SoilTypeShare_R_Soil_itemName)))

    L212.AgSupplySubsector[L212.AgSupplySubsector$Soil_Type == "Peat", "AgSupplySubsector"] <-
      apply(L212.AgSupplySubsector[L212.AgSupplySubsector$Soil_Type == "Peat", "AgSupplySubsector"],
            2,
            function(x){x <- paste(x, "Peat", sep = aglu.PEAT_DELIMITER)})

    L212.AgSupplySubsector <- L212.AgSupplySubsector %>%
      select(region, AgSupplySector, AgSupplySubsector) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS),
             logit.exponent = -3,
             logit.type = NA)

    # Writing files with modified emissions for protected lands.
    # Note: the protected lands input file is intended to be read after the normal one,
    # so rather than copy all of the duplicate information (e.g., supplysector and subsector, itemname),
    # these files only read in the new technologies and modified emissions levels

    L126.ProtectFrac_R_LT_GLU %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      replace_GLU(map = basin_to_country_mapping) %>%
      select(region, GLU, ProtectFract) ->
      L126.ProtectFrac_R_LT_GLU

    #9/30/20 nam: ItemName needs to be expanded to include peat land types so final XMLs get all periods (even those without emissions info) added to all land types
    #forgive my sins in overwriting this variable name three times
    L212.ItemName <-  L212.ItemName %>%
      left_join(L1251.SoilTypeShare_R_Soil_itemName, by=intersect(names(L212.ItemName), names(L1251.SoilTypeShare_R_Soil_itemName)))
    L212.ItemName[L212.ItemName$Soil_Type == "Peat", c("UnmanagedLandTechnology","AgSupplySubsector","itemName")] <-
      apply(L212.ItemName[L212.ItemName$Soil_Type == "Peat", c("UnmanagedLandTechnology","AgSupplySubsector","itemName")],
            2,
            function(x){x <- paste(x, "Peat", sep = aglu.PEAT_DELIMITER)})
    L212.ItemName <- L212.ItemName %>% select(region, AgSupplySector, AgSupplySubsector, UnmanagedLandTechnology, year, itemName)

    # Land item: protected land use types now prefixed with protected; non-protected keeps the same name so no need to write out
    # Land item: copy for prot and noprot, prefix the LTs with "Protected" in the prot file
    L212.ItemName_prot <- L212.ItemName %>%
      mutate(UnmanagedLandTechnology = paste0("Protected", UnmanagedLandTechnology),
             itemName = paste0("Protected", itemName))



    # Emissions: names in protected files are prefixed, and emissions in both are multiplied by protected land fraction
    # Grassland emissions
    # 9/21/2020 gpk - replace default aglu.PROTECT_LAND_FRACT with region-GLU-specific protected land fractions
    L212.GRASSEmissions_prot <- L212.GRASSEmissions %>%
      left_join_error_no_match(L126.ProtectFrac_R_LT_GLU, by = c("region", "GLU")) %>%
      mutate(UnmanagedLandTechnology = paste0("Protected", UnmanagedLandTechnology),
              input.emissions = input.emissions * ProtectFract) %>% select(-GLU)

    L212.GRASSEmissions_noprot <- L212.GRASSEmissions %>%
      left_join_error_no_match(L126.ProtectFrac_R_LT_GLU, by = c("region", "GLU")) %>%
      mutate(input.emissions = input.emissions * (1 - ProtectFract)) %>% select(-GLU)

    # Forest emissions - fires and deforest
    L212.FORESTEmissions_prot <- L212.FOREST %>%
      left_join_error_no_match(L126.ProtectFrac_R_LT_GLU, by = c("region", "GLU")) %>%
      mutate(UnmanagedLandTechnology = paste0("Protected", UnmanagedLandTechnology),
             input.emissions = input.emissions * ProtectFract) %>% select(-GLU)

    L212.FORESTEmissions_noprot <- L212.FOREST %>%
      left_join_error_no_match(L126.ProtectFrac_R_LT_GLU, by = c("region", "GLU")) %>%
      mutate(input.emissions = input.emissions * (1 - ProtectFract)) %>% select(-GLU)

    # Emissions factors: names in protected files are prefixed, and factors are left unchanged
    L212.GRASSEmissionsFactors_BCOC_prot <- L212.GRASSEmissionsFactors_BCOC %>%
      mutate(UnmanagedLandTechnology = paste0("Protected", UnmanagedLandTechnology))


    L212.FORESTEmissionsFactors_BCOC_prot <- L212.FORESTEmissionsFactors_BCOC %>%
      mutate(UnmanagedLandTechnology = paste0("Protected", UnmanagedLandTechnology))

    # Unmanaged pasture emissions factors (10/21/2020 nam)
    L212.PASTUREEmissionsFactors_Peat_prot <- L212.PASTUREEmissionsFactors_Peat %>%
      mutate(UnmanagedLandTechnology = paste0("Protected", UnmanagedLandTechnology))

    # Create future emissions factors for protected lands
    L212.FORESTEmissionsFactors_future_prot <- L212.FORESTEmissionsFactors_future %>%
      mutate(UnmanagedLandTechnology = paste0("Protected", UnmanagedLandTechnology))



    # ===================================================
    # Produce outputs
    L212.AgSupplySector %>%
      add_title("Logit Exponents for Unmanaged Land Sector") %>%
      add_units("Unitless") %>%
      add_comments("Hard-coded constant values") %>%
      add_legacy_name("L212.AgSupplySector") %>%
      add_precursors("common/GCAM_region_names", "L124.nonco2_tg_R_grass_Y_GLU",
                     "L124.nonco2_tg_R_forest_Y_GLU") ->
      L212.AgSupplySector

    L212.ItemName %>%
      add_title("Mapping File for Unmanaged Land") %>%
      add_units("NA") %>%
      add_comments("Mapping categories from L124.nonco2_tg_R_grass_Y_GLU, L124.nonco2_tg_R_forest_Y_GLU, and constants") %>%
      add_legacy_name("L212.ItemName") %>%
      add_precursors("common/GCAM_region_names", "water/basin_to_country_mapping",
                     "L124.nonco2_tg_R_grass_Y_GLU", "L124.nonco2_tg_R_forest_Y_GLU", "L222.LN2_Unmgd_PeatSoilCarbon") ->
      L212.ItemName

    L212.AgSupplySubsector %>%
      add_title("Logit Exponents for Unmanaged Land Subsectors") %>%
      add_units("Unitless") %>%
      add_comments("Constant logit exponent added to L212.ItemName") %>%
      add_legacy_name("L212.AgSupplySubsector") %>%
      same_precursors_as(L212.ItemName) ->
      L212.AgSupplySubsector

    L212.GRASSEmissions %>%
      add_title("Historical Emissions-Grassland Fires") %>%
      add_units("Tg/yr") %>%
      add_comments("Supply sectors and region names added to L124.nonco2_tg_R_grass_Y_GLU") %>%
       add_legacy_name("L212.GRASSEmissions") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "L124.nonco2_tg_R_grass_Y_GLU", "water/basin_to_country_mapping", "L1251.SoilTypeShare_R_Soil_LT_C_GLU") ->
      L212.GRASSEmissions

    L212.FOREST %>%
      filter(technology == "ForestFire") %>%
      select(-technology) %>%
      mutate(input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      add_title("Historical Emissions-Forest Fires") %>%
      add_units("Tg/yr") %>%
      add_comments("Supply sectors and region names added to L124.nonco2_tg_R_forest_Y_GLU") %>%
      add_legacy_name("L212.FORESTEmissions_FF") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "L124.nonco2_tg_R_forest_Y_GLU", "water/basin_to_country_mapping", "L1251.SoilTypeShare_R_Soil_LT_C_GLU") ->
      L212.FORESTEmissions_FF

    L212.FOREST %>%
      filter(technology == "Deforest") %>%
      select(-technology) %>%
      add_title("Historical Emissions-Deforestation") %>%
      add_units("Tg/yr") %>%
      add_comments("Supply sectors and region names added to L124.nonco2_tg_R_forest_Y_GLU") %>%
      add_legacy_name("L212.FORESTEmissions_D") %>%
      same_precursors_as(L212.FORESTEmissions_FF) ->
      L212.FORESTEmissions_D

    L212.GRASSEmissionsFactors_BCOC %>%
      add_title("Historical BC/OC Emissions Coefficients-Grassland Fires") %>%
      add_units("Tg/bm2") %>%
      add_comments("comments describing how data generated") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC") %>%
      add_precursors("common/GCAM_region_names", "L125.bcoc_tgbkm2_R_grass_2000",
                     "L124.nonco2_tg_R_grass_Y_GLU", "water/basin_to_country_mapping", "L1251.SoilTypeShare_R_Soil_LT_C_GLU") ->
      L212.GRASSEmissionsFactors_BCOC

    L212.PASTUREEmissionsFactors_Peat %>%
      add_title("Unmanaged Pasture Emissions Coefficients on Peatland") %>%
      add_units("Tg/bm2") %>%
      add_comments("Emissions factors directly from RTI emissions factors on peatland") %>%
      add_legacy_name("L212.PASTUREEmissionsFactors_Peat") %>%
      add_precursors("common/GCAM_region_names", "aglu/EPA_peatland_EFs",
                     "aglu/RTI_peatland_categories", "L222.LN2_Unmgd_PeatSoilCarbon") ->
      L212.PASTUREEmissionsFactors_Peat

    L212.FORESTEmissionsFactors_BCOC %>%
      filter(technology == "ForestFire") %>%
      select(-technology) %>%
      mutate(input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      add_title("Historical BC/OC Emissions Coefficients-Forest Fires") %>%
      add_units("Tg/bm2") %>%
      add_comments("L125.bcoc_tgbkm2_R_grass_2000 emissions factors added to L212.GRASSEmissions") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF") %>%
      add_precursors("common/GCAM_region_names", "L125.bcoc_tgbkm2_R_forest_2000",
                     "L124.nonco2_tg_R_forest_Y_GLU", "water/basin_to_country_mapping", "L1251.SoilTypeShare_R_Soil_LT_C_GLU") ->
      L212.FORESTEmissionsFactors_BCOC_FF

    L212.FORESTEmissionsFactors_BCOC %>%
      filter(technology == "Deforest") %>%
      select(-technology) %>%
      add_title("Historical BC/OC Emissions Coefficients-Deforestation") %>%
      add_units("Tg/bm2") %>%
      add_comments("L125.bcoc_tgbkm2_R_forest_2000 emissions factors") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D") %>%
      same_precursors_as(L212.FORESTEmissionsFactors_BCOC_FF) ->
      L212.FORESTEmissionsFactors_BCOC_D

    L212.FORESTEmissionsFactors_future %>%
      add_title("Future BC/OC Emissions Coefficients-Deforestation") %>%
      add_units("kg/m2") %>%
      add_comments("L212.default_coefs values added to first future year") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_future") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "L124.nonco2_tg_R_forest_Y_GLU", "water/basin_to_country_mapping",
                     "L124.deforest_coefs", "L125.deforest_coefs_bcoc", "L1251.SoilTypeShare_R_Soil_LT_C_GLU") ->
      L212.FORESTEmissionsFactors_future

    L212.FORESTEmissionsFactors_future_prot %>%
      add_title("Future BC/OC Emissions Coefficients-Deforestation for Protected Lands") %>%
      add_units("kg/m2") %>%
      add_comments("L212.default_coefs values added to first future year") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_future") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "L124.nonco2_tg_R_forest_Y_GLU", "water/basin_to_country_mapping",
                     "L124.deforest_coefs", "L125.deforest_coefs_bcoc", "L1251.SoilTypeShare_R_Soil_LT_C_GLU") ->
      L212.FORESTEmissionsFactors_future_prot

    L212.ItemName_prot %>%
      add_title("Mapping File for Protected Unmanaged Land") %>%
      add_units("NA") %>%
      add_comments("Same info as L212.ItemName") %>%
      add_legacy_name("L212.ItemName_prot") %>%
      same_precursors_as(L212.ItemName) ->
      L212.ItemName_prot

    L212.GRASSEmissions_prot %>%
      add_title("Historical Protected Emissions-Grassland Fires") %>%
      add_units("Tg/yr") %>%
      add_comments("L212.GRASSEmissions values multiplied by protected land fraction") %>%
      add_legacy_name("L212.GRASSEmissions_prot") %>%
      same_precursors_as(L212.GRASSEmissions) %>%
      add_precursors("L126.ProtectFrac_R_LT_GLU") ->
      L212.GRASSEmissions_prot

    L212.GRASSEmissions_noprot %>%
      add_title("Historical Unprotected Emissions-Grassland Fires") %>%
      add_units("Tg/yr") %>%
      add_comments("L212.GRASSEmissions values multiplied by 1 minus protected land fraction") %>%
      add_legacy_name("L212.GRASSEmissions_noprot") %>%
      same_precursors_as(L212.GRASSEmissions_prot) ->
      L212.GRASSEmissions_noprot

    L212.FORESTEmissions_prot %>%
      filter(technology == "ForestFire") %>%
      select(-technology) %>%
      mutate(input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      add_title("Historical Protected Emissions-Forest Fires") %>%
      add_units("Tg/yr") %>%
      add_comments("L212.FORESTEmissions_FF values multiplied by protected land fraction") %>%
      add_legacy_name("L212.FORESTEmissions_FF_prot") %>%
      same_precursors_as(L212.FORESTEmissions_FF) %>%
      add_precursors("L126.ProtectFrac_R_LT_GLU") ->
      L212.FORESTEmissions_FF_prot

    L212.FORESTEmissions_noprot %>%
      filter(technology == "ForestFire") %>%
      select(-technology) %>%
      mutate(input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      add_title("Historical Unprotected Emissions-Forest Fires") %>%
      add_units("Tg/yr") %>%
      add_comments("L212.FORESTEmissions_FF values multiplied by 1 minus protected land fraction") %>%
      add_legacy_name("L212.FORESTEmissions_FF_noprot") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions",
                     "L124.nonco2_tg_R_grass_Y_GLU", "water/basin_to_country_mapping")->
      L212.FORESTEmissions_FF_noprot

    L212.FORESTEmissions_prot %>%
      filter(technology == "Deforest") %>%
      select(-technology) %>%
      add_title("Historical Protected Emissions-Deforestation") %>%
      add_units("Tg/yr") %>%
      add_comments("L212.FORESTEmissions_D values multiplied by protected land fraction") %>%
      add_legacy_name("L212.FORESTEmissions_D_prot") %>%
      same_precursors_as(L212.FORESTEmissions_D) %>%
      add_precursors("L126.ProtectFrac_R_LT_GLU") ->
      L212.FORESTEmissions_D_prot

    L212.FORESTEmissions_noprot %>%
      filter(technology == "Deforest") %>%
      select(-technology) %>%
      add_title("Historical Unprotected Emissions-Deforestation") %>%
      add_units("Tg/yr") %>%
      add_comments("L212.FORESTEmissions_D values multiplied by 1 minus protected land fraction") %>%
      add_legacy_name("L212.FORESTEmissions_D_noprot") %>%
      same_precursors_as(L212.FORESTEmissions_D) %>%
      add_precursors("L126.ProtectFrac_R_LT_GLU") ->
      L212.FORESTEmissions_D_noprot

    L212.GRASSEmissionsFactors_BCOC_prot %>%
      add_title("BC/OC Protected Emissions Factors-Grassland Fires") %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.GRASSEmissionsFactors_BCOC") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC_prot") %>%
      same_precursors_as(L212.GRASSEmissionsFactors_BCOC) ->
      L212.GRASSEmissionsFactors_BCOC_prot

    L212.GRASSEmissionsFactors_BCOC %>%
      # Useless mutate to add different attributes
      mutate(region = region) %>%
      add_title("BC/OC Unprotected Emissions Factors-Grassland Fires", overwrite = TRUE) %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.GRASSEmissionsFactors_BCOC") %>%
      add_legacy_name("L212.GRASSEmissionsFactors_BCOC_noprot", overwrite = TRUE) %>%
      same_precursors_as(L212.GRASSEmissionsFactors_BCOC) ->
      L212.GRASSEmissionsFactors_BCOC_noprot

    L212.PASTUREEmissionsFactors_Peat_prot %>%
      add_title("Protected Unmanaged Pasture Emissions Coefficients on Peatland") %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.PASTUREEmissionsFactors_Peat") %>%
      add_comments("Emissions factors directly from RTI emissions factors on peatland") %>%
      add_legacy_name("L212.PASTUREEmissionsFactors_Peat_prot") %>%
      same_precursors_as(L212.PASTUREEmissionsFactors_Peat) ->
      L212.PASTUREEmissionsFactors_Peat_prot

    L212.PASTUREEmissionsFactors_Peat %>%
      mutate(region = region) %>% #useless mutate to add different attributes
      add_title("Unprotected Unmanaged Pasture Emissions Coefficients on Peatland") %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.PASTUREEmissionsFactors_Peat") %>%
      add_comments("Emissions factors directly from RTI emissions factors on peatland") %>%
      add_legacy_name("L212.PASTUREEmissionsFactors_Peat_noprot") %>%
      same_precursors_as(L212.PASTUREEmissionsFactors_Peat) ->
      L212.PASTUREEmissionsFactors_Peat_noprot


    L212.FORESTEmissionsFactors_BCOC_prot %>%
      filter(technology == "ForestFire") %>%
      select(-technology) %>%
      mutate(input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      add_title("BC/OC Protected Emissions Factors-Forest Fires") %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.FORESTEmissionsFactors_BCOC_FF") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF_prot") %>%
      same_precursors_as(L212.FORESTEmissionsFactors_BCOC_FF) ->
      L212.FORESTEmissionsFactors_BCOC_FF_prot

    L212.FORESTEmissionsFactors_BCOC_FF %>%
      mutate(input.name = emissions.UNMGD_LAND_INPUT_NAME) %>%
      add_title("BC/OC Unprotected Emissions Factors-Forest Fires", overwrite = TRUE) %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.FORESTEmissionsFactors_BCOC_FF") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_FF_noprot", overwrite = TRUE) %>%
      same_precursors_as(L212.FORESTEmissionsFactors_BCOC_FF) ->
      L212.FORESTEmissionsFactors_BCOC_FF_noprot

    L212.FORESTEmissionsFactors_BCOC_prot %>%
      filter(technology == "Deforest") %>%
      select(-technology) %>%
      add_title("BC/OC Protected Emissions Factors-Deforestation") %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.FORESTEmissionsFactors_BCOC_D") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D_prot") %>%
      same_precursors_as(L212.FORESTEmissionsFactors_BCOC_D) ->
      L212.FORESTEmissionsFactors_BCOC_D_prot

    L212.FORESTEmissionsFactors_BCOC_D %>%
      # Useless mutate to add different attributes
      mutate(region = region) %>%
      add_title("BC/OC Unprotected Emissions Factors-Deforestation", overwrite = TRUE) %>%
      add_units("Tg/bm2") %>%
      add_comments("Same values as L212.FORESTEmissionsFactors_BCOC_D") %>%
      add_legacy_name("L212.FORESTEmissionsFactors_BCOC_D_noprot", overwrite = TRUE) %>%
      same_precursors_as(L212.FORESTEmissionsFactors_BCOC_D) ->
      L212.FORESTEmissionsFactors_BCOC_D_noprot

    return_data(L212.AgSupplySector, L212.AgSupplySubsector, L212.ItemName, L212.GRASSEmissions,
                L212.FORESTEmissions_FF, L212.FORESTEmissions_D, L212.GRASSEmissionsFactors_BCOC,
                L212.FORESTEmissionsFactors_BCOC_FF, L212.FORESTEmissionsFactors_BCOC_D,
                L212.FORESTEmissionsFactors_future, L212.ItemName_prot, L212.GRASSEmissions_prot,
                L212.GRASSEmissions_noprot, L212.FORESTEmissions_FF_prot, L212.FORESTEmissions_FF_noprot,
                L212.FORESTEmissions_D_prot, L212.FORESTEmissions_D_noprot, L212.GRASSEmissionsFactors_BCOC_prot,
                L212.GRASSEmissionsFactors_BCOC_noprot, L212.FORESTEmissionsFactors_BCOC_FF_prot,
                L212.FORESTEmissionsFactors_BCOC_FF_noprot, L212.FORESTEmissionsFactors_BCOC_D_prot,
                L212.FORESTEmissionsFactors_BCOC_D_noprot, L212.FORESTEmissionsFactors_future_prot,
                L212.PASTUREEmissionsFactors_Peat, L212.PASTUREEmissionsFactors_Peat_prot, L212.PASTUREEmissionsFactors_Peat_noprot)
  } else {
    stop("Unknown command")
  }
}
