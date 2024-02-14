#' module_energy_L271.ag_en_freight_inputs
#'
#' Generate inputs of freight transportation to energy and agricultural commodities, and modify the
#' final demands of freight transportation services accordingly
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L271.TechCoef_freight}, \code{L271.TechInputPMult_freight}, \code{L271.TechCost_freight}, \code{L271.BaseService_freightNetEnAg}.
#' @details The freight input-output coefficients are exogenous, as are cost adjustments and price unit conversions.
#'   They are multiplied by commodity flow volumes to compute the trn_freight quantities that are no longer part of the
#'   final demands, and this deduction is performed here.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by if_else inner_join mutate rename select summarise ungroup
#' @importFrom tidyr complete nesting
#' @author GPK March 2019
module_energy_L271.ag_en_freight_inputs <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A21.globaltech_coef",
             FILE = "energy/A271.freight_coef",
             FILE = "energy/A271.freight_cost_adj",
             "L1011.en_bal_EJ_R_Si_Fi_Yh",
             "L122.in_EJ_R_gasproc_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L2262.Production_refinedLiquids_reg_dom",
             "L2262.Production_refinedLiquids_reg_imp",
             "L254.BaseService_trn",
             "L202.StubTechProd_in",
             "L203.StubTechProd_food",
             "L203.StubTechProd_nonfood_crop",
             "L221.StubTechCalInput_bioOil",
             "L240.Production_reg_imp",
             "L240.Production_tra",
             "L239.Production_reg_imp",
             "L239.Production_reg_dom",
             "L239.Production_tra"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L271.StubTechCoef_freight_FoodDemand",
             "L271.StubTechCoef_freight_NonFoodDemand",
             "L271.TechCoef_freight_en_ag_other",
             "L271.StubTechInputPMult_freight_FoodDemand",
             "L271.StubTechInputPMult_freight_NonFoodDemand",
             "L271.TechInputPMult_freight_en_ag_other",
             "L271.TechCost_freight",
             "L271.BaseService_freightNetEnAg"))
  } else if(command == driver.MAKE) {

    value <- region <- supplysector <- subsector <- technology <- minicam.energy.input <-
      price.unit.conversion <- year <- coefficient <- minicam.non.energy.input <- input.cost <-
      fuel <- energy_EJ <- freight_tkm <- sector <- biomassOil_coef <- GCAM_region_ID <-
      stub.technology <- calOutputValue <- base.service <- NULL # silence package check notes

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A271.freight_coef <- get_data(all_data, "energy/A271.freight_coef")
    A271.freight_cost_adj <- get_data(all_data, "energy/A271.freight_cost_adj")
    L1011.en_bal_EJ_R_Si_Fi_Yh <- get_data(all_data, "L1011.en_bal_EJ_R_Si_Fi_Yh", strip_attributes = TRUE)
    L122.in_EJ_R_gasproc_F_Yh <- get_data(all_data, "L122.in_EJ_R_gasproc_F_Yh", strip_attributes = TRUE)
    L122.out_EJ_R_refining_F_Yh <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh", strip_attributes = TRUE)
    L2262.Production_refinedLiquids_reg_dom <- get_data(all_data, "L2262.Production_refinedLiquids_reg_dom", strip_attributes = TRUE)
    L2262.Production_refinedLiquids_reg_imp <- get_data(all_data, "L2262.Production_refinedLiquids_reg_imp", strip_attributes = TRUE)
    L254.BaseService_trn <- get_data(all_data, "L254.BaseService_trn", strip_attributes = TRUE) %>%
      filter(sce == "CORE")
    L202.StubTechProd_in <- get_data(all_data, "L202.StubTechProd_in", strip_attributes = TRUE)
    L203.StubTechProd_food <- get_data(all_data, "L203.StubTechProd_food", strip_attributes = TRUE)
    L203.StubTechProd_nonfood_crop <- get_data(all_data, "L203.StubTechProd_nonfood_crop", strip_attributes = TRUE)
    L240.Production_reg_imp <- get_data(all_data, "L240.Production_reg_imp", strip_attributes = TRUE)
    L240.Production_tra <- get_data(all_data, "L240.Production_tra", strip_attributes = TRUE)
    A21.globaltech_coef <- get_data(all_data, "energy/A21.globaltech_coef",)
    L221.StubTechCalInput_bioOil <- get_data(all_data, "L221.StubTechCalInput_bioOil", strip_attributes = TRUE)
    L239.Production_reg_imp <- get_data(all_data, "L239.Production_reg_imp", strip_attributes = TRUE)
    L239.Production_reg_dom <- get_data(all_data, "L239.Production_reg_dom", strip_attributes = TRUE)
    L239.Production_tra <- get_data(all_data, "L239.Production_tra", strip_attributes = TRUE)

    # L271.TechCoef_freight: technology coefficients of LCA-type inputs
    L271.TechCoef_Pmult_freight <- gather_years( A271.freight_coef, value_col = "coefficient") %>%
      complete(nesting(region, supplysector, subsector, technology, minicam.energy.input, price.unit.conversion),
               year = unique(c(year, MODEL_YEARS))) %>%
      group_by(region, supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient, rule = 2),
                                 energy.DIGITS_COEFFICIENT),
             price.unit.conversion = round(approx_fun(year, price.unit.conversion),
                                           energy.DIGITS_COEFFICIENT),
             market.name = if_else(grepl("traded", supplysector),
                                   substr(subsector, 1, nchar(subsector) - nchar(supplysector) - 1),
                                   region)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS)

    L271.TechCoef_freight <- select(L271.TechCoef_Pmult_freight, LEVEL2_DATA_NAMES[["TechCoef"]])
    L271.TechInputPMult_freight <- select(L271.TechCoef_Pmult_freight, LEVEL2_DATA_NAMES[["TechInputPMult"]])

    # L271.TechCost_freight: technology costs for techs that take freight transportation inputs
    L271.TechCost_freight <- gather_years( A271.freight_cost_adj, value_col = "input.cost") %>%
      complete(nesting(region, supplysector, subsector, technology, minicam.non.energy.input),
               year = unique(c(year, MODEL_YEARS))) %>%
      group_by(region, supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = round(approx_fun(year, input.cost, rule = 2),
                                energy.DIGITS_COST)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # To compute the tkm to deduct from freight transportation service demand, we
    # need to get calibrated values for all of the technologies that have demand in the base years
    # Here, we collect all of these values

    # OIL and COAL
    # Calibrated values for regional and traded fossil fuels
    L271.in_EJ_R_Fossil <- bind_rows(L239.Production_reg_imp, L239.Production_reg_dom, L239.Production_tra) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(-share.weight.year, -subs.share.weight, -tech.share.weight)

    # REGIONAL BIOMASS
    # 8/13/2020 - gpk - the IEA energy balances don't track biomass feedstock inputs to biogas production,
    # so they aren't in L1011.en_bal, though they are in the model.
    # Need to add these in specifically
    L271.in_EJ_R_TPES_bio <- filter(L1011.en_bal_EJ_R_Si_Fi_Yh,
                                    sector == "TPES",
                                    fuel == "biomass",
                                    year %in% MODEL_BASE_YEARS) %>%
      bind_rows(filter(L122.in_EJ_R_gasproc_F_Yh, fuel == "biomass")) %>%
      group_by(GCAM_region_ID, fuel, year) %>%
      summarise(calOutputValue = sum(value)) %>%
      ungroup() %>%
      mutate(supplysector = "regional biomass", subsector = "regional biomass", technology = "regional biomass") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID, -fuel)

    # ETHANOL FEEDSTOCKS
    L271.out_EJ_R_ethanol <- filter(L122.out_EJ_R_refining_F_Yh,
                                    grepl("ethanol", sector),
                                    year %in% MODEL_BASE_YEARS) %>%
      rename(calOutputValue = value) %>%
      mutate(supplysector = case_when(
        sector == "corn ethanol" ~ "regional corn for ethanol",
        sector == "sugar cane ethanol" ~ "regional sugar for ethanol"
      )) %>%
      # Set subsector and technology to supplysector name as well
      mutate(subsector = supplysector, technology = supplysector) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, supplysector, subsector, technology, year, calOutputValue)

    # BIODIESEL FEEDSTOCKS
    L271.biomassoil_coef <- A21.globaltech_coef %>%
      filter(grepl("regional biomassOil", supplysector)) %>%
      gather_years() %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = sort(unique(c(year, MODEL_YEARS)))) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(biomassOil_coef = approx_fun(year, value)) %>%
      ungroup() %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-minicam.energy.input, -value)

    L271.in_EJ_R_TPES_bioOil <- select(L221.StubTechCalInput_bioOil, region, supplysector, subsector, technology = stub.technology,
                                       year, calibrated.value) %>%
      left_join_error_no_match(L271.biomassoil_coef,
                               by = c("supplysector", "subsector", "technology", "year")) %>%
      mutate(calOutputValue = calibrated.value / biomassOil_coef) %>%
      select(-biomassOil_coef, -calibrated.value)

    # REFINED LIQUIDS
    # Note - using "refined liquids enduse" as a proxy for all refined liquids. As noted in the assumptions table, the
    # method is not set up to handle different tkm coefficients for the different consumer classes of refined liquids. This
    # is fine for now as there is no data on that anyway. Still the issue is that there is nowhere in the data system that
    # the quantities of refined liquids enduse vs industrial are actually written out.
    L271.out_EJ_R_dom_refliq <- filter(L2262.Production_refinedLiquids_reg_dom,
                                       year %in% MODEL_BASE_YEARS,
                                       supplysector == "regional refined oil") %>%
      mutate(supplysector = "refined liquids enduse", subsector = "refined oil", technology = "refined oil") %>%
      select(region, supplysector, subsector, technology, year, calOutputValue)

    # imported
    L271.out_EJ_R_imp_refliq <- filter(L2262.Production_refinedLiquids_reg_imp,
                                       year %in% MODEL_BASE_YEARS,
                                       supplysector == "regional refined oil") %>%
      mutate(supplysector = "refined liquids enduse", subsector = "refined oil", technology = "refined oil") %>%
      select(region, supplysector, subsector, technology, year, calOutputValue)

    L271.out_EJ_R_refliq <- bind_rows(L271.out_EJ_R_dom_refliq, L271.out_EJ_R_imp_refliq) %>%
      group_by(region, supplysector, subsector, technology, year) %>%
      summarise(calOutputValue = sum(calOutputValue)) %>%
      ungroup

    # CROP CONSUMPTION (domestic + imports)
    # 11/14/2018 modification - this needs to include the individual components to the demands (food, non-food, feed)
    # in order to not double-count the freight transport for crops used for biofuel production (accounted elsewhere)
    L271.in_Mt_R_DomSupply_crops <- bind_rows( L202.StubTechProd_in, L203.StubTechProd_food,
                                               L203.StubTechProd_nonfood_crop, L240.Production_reg_imp) %>%
      mutate(technology = if_else(is.na(technology), stub.technology, technology)) %>%
      select(region, supplysector, subsector, technology, year, calOutputValue)


    # CROP EXPORTS
    L271.in_Mt_R_Exports_crops <- select(L240.Production_tra,
                                         region, supplysector, subsector, technology, year, calOutputValue)

    # Since trn inputs to fossil fuels are quite large, we will first deduct based on all the other commodities
    # Then, we will adjust any fossil fuel coefficients in historical years that create problems
    # Combine all of the calibrated values together
    L271.calValue_en_crops_nonFossil <- bind_rows(L271.in_EJ_R_TPES_bio, L271.out_EJ_R_ethanol, L271.in_EJ_R_TPES_bioOil,
                                        L271.out_EJ_R_refliq,
                                        L271.in_Mt_R_DomSupply_crops, L271.in_Mt_R_Exports_crops)


    # First deduct based on non-fossil trade
    L271.tkm_tot_en_crops_nonFossil <- filter(L271.TechCoef_freight,
                                    year %in% MODEL_BASE_YEARS,
                                    # filtering out these refined liquids that we don't have data for
                                    !(supplysector == "refined liquids enduse" & subsector == "biodiesel"),
                                    !(supplysector == "refined liquids enduse" & subsector == "renewable diesel"),
                                    supplysector != "refined liquids gasoline pool",
                                    supplysector != "refined liquids industrial",
                                    supplysector != "aviation fuels",
                                    !supplysector %in% L271.in_EJ_R_Fossil$supplysector
    ) %>%
      left_join_error_no_match(L271.calValue_en_crops_nonFossil,
                by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(freight_tkm = coefficient * calOutputValue,
             # For exports, we need to rewrite to exporting region, not USA
             region = if_else(grepl("traded", supplysector), substr(subsector, 1, nchar(subsector) - nchar(supplysector) - 1), region)) %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(freight_tkm = sum(freight_tkm)) %>%
      ungroup()

    L271.BaseService_freightNetEnAg_nonFossil <- left_join(L254.BaseService_trn,
                                                            L271.tkm_tot_en_crops_nonFossil,
                                                          by = c("region", "year", "energy.final.demand" = "minicam.energy.input")) %>%
      mutate(base.service = if_else(is.na(freight_tkm), base.service, base.service - freight_tkm)) %>%
      select(LEVEL2_DATA_NAMES[["BaseService"]])

    # Then deduct based on fossil trade
    # Start by checking how much would be deducted with current coefficients
    L271.tkm_tot_en_Fossil <- filter(L271.TechCoef_freight,
                                           year %in% MODEL_BASE_YEARS,
                                          supplysector %in% L271.in_EJ_R_Fossil$supplysector
    ) %>%
      left_join_error_no_match(L271.in_EJ_R_Fossil,
                               by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(freight_tkm = coefficient * calOutputValue,
             # For exports, we need to rewrite to exporting region, not USA
             region = if_else(grepl("traded", supplysector), substr(subsector, 1, nchar(subsector) - nchar(supplysector) - 1), region)) %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(freight_tkm = sum(freight_tkm)) %>%
      ungroup()

    L271.BaseService_freightNetEnAg_Fossil <- inner_join(L271.BaseService_freightNetEnAg_nonFossil,
                                                         L271.tkm_tot_en_Fossil,
                                                            by = c("region", "year", "energy.final.demand" = "minicam.energy.input")) %>%
      mutate(ratio = freight_tkm / base.service)

    # Need to adjust if more than half of remaining service goes to fossil shipping
    L271.Fossil_coef_adjust <- L271.BaseService_freightNetEnAg_Fossil %>%
      filter(ratio > 0.5) %>%
      select(-ratio)

    L271.TechCoef_freight_fossil_adjust <- L271.TechCoef_freight %>%
      # Filter to fossil sectors
      semi_join(L271.in_EJ_R_Fossil, by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      # Create second region column to account for traded fossils
      mutate(region2 = if_else(grepl("traded", supplysector), substr(subsector, 1, nchar(subsector) - nchar(supplysector) - 1), region)) %>%
      # Join in the base service filtered to the problem regions
      right_join(semi_join(L271.BaseService_freightNetEnAg_Fossil, L271.Fossil_coef_adjust,
                           by = c("region", "energy.final.demand", "year")),
                 by = c("region2" = "region", "year", "minicam.energy.input" = "energy.final.demand")) %>%
      filter(freight_tkm > 0) %>%
      # Adjust coefficients to account for half of the remaining base.service
      mutate(adj_ratio = base.service * 0.5 / freight_tkm,
             coefficient = coefficient * adj_ratio) %>%
      select(names(L271.TechCoef_freight))

    # Rewrite the coefficients that need to be changed in L271.TechCoef_freight
    L271.TechCoef_freight <- L271.TechCoef_freight %>%
      # remove old fossil coefficients, then add back in adjusted ones
      anti_join(L271.TechCoef_freight_fossil_adjust, by = c("region", "supplysector", "subsector", "technology", "year", "minicam.energy.input", "market.name")) %>%
      bind_rows(L271.TechCoef_freight_fossil_adjust)

    # Repeat same steps to deduct the fossil tkm, but actually deduct this time
    L271.tkm_tot_en_Fossil <- filter(L271.TechCoef_freight,
                                     year %in% MODEL_BASE_YEARS,
                                     supplysector %in% L271.in_EJ_R_Fossil$supplysector
    ) %>%
      left_join_error_no_match(L271.in_EJ_R_Fossil,
                               by = c("region", "supplysector", "subsector", "technology", "year")) %>%
      mutate(freight_tkm = coefficient * calOutputValue,
             # For exports, we need to rewrite to exporting region, not USA
             region = if_else(grepl("traded", supplysector), substr(subsector, 1, nchar(subsector) - nchar(supplysector) - 1), region)) %>%
      group_by(region, minicam.energy.input, year) %>%
      summarise(freight_tkm = sum(freight_tkm)) %>%
      ungroup()

    L271.BaseService_freightNetEnAg <- left_join(L271.BaseService_freightNetEnAg_nonFossil,
                                                         L271.tkm_tot_en_Fossil,
                                                         by = c("region", "year", "energy.final.demand" = "minicam.energy.input")) %>%
      mutate(base.service = if_else(is.na(freight_tkm), base.service, base.service - freight_tkm)) %>%
      select(LEVEL2_DATA_NAMES[["BaseService"]]) %>%
      # filter to only changed demand sectors
      semi_join(bind_rows(L271.tkm_tot_en_Fossil, L271.tkm_tot_en_crops_nonFossil),
                by = c("region", "energy.final.demand" = "minicam.energy.input", "year"))

    # Separate tables because they require different commands for xml creation
    # FoodDemand has nesting subsectors and needs stub.technology
    L271.StubTechCoef_freight_FoodDemand <- L271.TechCoef_freight %>%
      filter(grepl("^FoodDemand", supplysector)) %>%
      rename(stub.technology = technology) %>%
      left_join_error_no_match(distinct(L203.StubTechProd_food, supplysector, subsector, stub.technology, subsector0),
                               by = c("supplysector", "subsector", "stub.technology"))
    L271.StubTechInputPMult_freight_FoodDemand <- L271.TechInputPMult_freight %>%
      filter(grepl("^FoodDemand", supplysector))%>%
      rename(stub.technology = technology) %>%
      left_join_error_no_match(distinct(L203.StubTechProd_food, supplysector, subsector, stub.technology, subsector0),
                               by = c("supplysector", "subsector", "stub.technology"))

    # NonFoodDemand needs stub.technology
    L271.StubTechCoef_freight_NonFoodDemand <- L271.TechCoef_freight %>%
      filter(grepl("^NonFoodDemand", supplysector)) %>%
      rename(stub.technology = technology)
    L271.StubTechInputPMult_freight_NonFoodDemand <- L271.TechInputPMult_freight %>%
      filter(grepl("^NonFoodDemand", supplysector)) %>%
      rename(stub.technology = technology)

    # Everything else can stay as is
    L271.TechCoef_freight_en_ag_other <- L271.TechCoef_freight %>%
      anti_join(bind_rows(L271.StubTechCoef_freight_FoodDemand, L271.StubTechCoef_freight_NonFoodDemand),
                by = c("region", "supplysector", "subsector", "technology" = "stub.technology", "year", "minicam.energy.input", "coefficient", "market.name"))
    L271.TechInputPMult_freight_en_ag_other <- L271.TechInputPMult_freight %>%
      anti_join(bind_rows(L271.StubTechInputPMult_freight_FoodDemand, L271.StubTechInputPMult_freight_NonFoodDemand),
                by = c("region", "supplysector", "subsector", "technology" = "stub.technology", "year", "minicam.energy.input", "price.unit.conversion"))

    # Produce outputs
    L271.StubTechCoef_freight_FoodDemand %>%
      add_title("Input-output coefficients of freight transport into FoodDemand ag commodities") %>%
      add_units("million tkm per EJ for energy goods; km of travel distance for ag goods") %>%
      add_comments("Adds a trn_freight input to commodities that are shipped") %>%
      add_comments("IO coefs reflect travel distances and, for energy goods, energy contents") %>%
      add_precursors("energy/A271.freight_coef", "L203.StubTechProd_food") ->
      L271.StubTechCoef_freight_FoodDemand

    L271.StubTechCoef_freight_NonFoodDemand %>%
      add_title("Input-output coefficients of freight transport into NonFoodDemand commodities") %>%
      add_units("million tkm per EJ for energy goods; km of travel distance for ag goods") %>%
      add_comments("Adds a trn_freight input to commodities that are shipped") %>%
      add_comments("IO coefs reflect travel distances and, for energy goods, energy contents") %>%
      same_precursors_as(L271.StubTechCoef_freight_FoodDemand) ->
      L271.StubTechCoef_freight_NonFoodDemand

    L271.TechCoef_freight_en_ag_other %>%
      add_title("Input-output coefficients of freight transport into energy and ag commodities (excluding FoodDemand and NonFoodDemand)") %>%
      add_units("million tkm per EJ for energy goods; km of travel distance for ag goods") %>%
      add_comments("Adds a trn_freight input to commodities that are shipped") %>%
      add_comments("IO coefs reflect travel distances and, for energy goods, energy contents") %>%
      same_precursors_as(L271.StubTechCoef_freight_FoodDemand) ->
      L271.TechCoef_freight_en_ag_other

    L271.StubTechInputPMult_freight_FoodDemand %>%
      add_title("Price-unit-conversions on trn_freight inputs to shipped FoodDemand commodities") %>%
      add_units("Unitless multiplier") %>%
      add_comments("Reduces the cost paid for shipping from the composite trn_freight cost") %>%
      add_comments("Applies to goods mostly shipped by rail or ship, not truck, so initial costs are over-estimated") %>%
      add_precursors("energy/A271.freight_coef", "L203.StubTechProd_food") ->
      L271.StubTechInputPMult_freight_FoodDemand

    L271.StubTechInputPMult_freight_NonFoodDemand %>%
      add_title("Price-unit-conversions on trn_freight inputs to shipped NonFoodDemand commodities") %>%
      add_units("Unitless multiplier") %>%
      add_comments("Reduces the cost paid for shipping from the composite trn_freight cost") %>%
      add_comments("Applies to goods mostly shipped by rail or ship, not truck, so initial costs are over-estimated") %>%
      same_precursors_as(L271.StubTechInputPMult_freight_FoodDemand) ->
      L271.StubTechInputPMult_freight_NonFoodDemand

    L271.TechInputPMult_freight_en_ag_other %>%
      add_title("Price-unit-conversions on trn_freight inputs to shipped commodities (excluding FoodDemand and NonFoodDemand)") %>%
      add_units("Unitless multiplier") %>%
      add_comments("Reduces the cost paid for shipping from the composite trn_freight cost") %>%
      add_comments("Applies to goods mostly shipped by rail or ship, not truck, so initial costs are over-estimated") %>%
      same_precursors_as(L271.StubTechInputPMult_freight_FoodDemand) ->
      L271.TechInputPMult_freight_en_ag_other

    L271.TechCost_freight %>%
      add_title("Non-energy costs of shipped commodities, adjusted for explicitly modeled shipping costs") %>%
      add_units("1975$/GJ, 1975$/kg") %>%
      add_comments("Counter-balances the cost effects of explicitly including freight transport costs") %>%
      add_precursors("energy/A271.freight_cost_adj") ->
      L271.TechCost_freight

    L271.BaseService_freightNetEnAg %>%
      add_title("Revised base service of freight transportation final demand sectors") %>%
      add_units("million tonne-km") %>%
      add_comments("Counter-balances the effects of explicitly including freight transport inputs to energy and ag commodities") %>%
      add_precursors("common/GCAM_region_names", "energy/A271.freight_coef", "L1011.en_bal_EJ_R_Si_Fi_Yh",
                      "L122.in_EJ_R_gasproc_F_Yh", "L122.out_EJ_R_refining_F_Yh",
                     "L2262.Production_refinedLiquids_reg_dom", "L2262.Production_refinedLiquids_reg_imp",
                     "L254.BaseService_trn", "L202.StubTechProd_in", "L203.StubTechProd_food", "L203.StubTechProd_nonfood_crop",
                     "L240.Production_reg_imp", "L240.Production_tra","energy/A21.globaltech_coef","L221.StubTechCalInput_bioOil",
                     "L239.Production_reg_imp", "L239.Production_reg_dom", "L239.Production_tra") ->
      L271.BaseService_freightNetEnAg

    return_data(L271.StubTechCoef_freight_FoodDemand,
                L271.StubTechCoef_freight_NonFoodDemand,
                L271.TechCoef_freight_en_ag_other,
                L271.StubTechInputPMult_freight_FoodDemand,
                L271.StubTechInputPMult_freight_NonFoodDemand,
                L271.TechInputPMult_freight_en_ag_other,
                L271.TechCost_freight,
                L271.BaseService_freightNetEnAg)

  } else {
    stop("Unknown command")
  }
}
