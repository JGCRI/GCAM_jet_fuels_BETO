# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L202.an_input
#'
#' Produce a wide range of animal-related resource tables: production, import, resource curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L202.RenewRsrc}, \code{L202.RenewRsrcPrice}, \code{L202.Rsrc},
#' \code{L202.RsrcPrice}, \code{L202.maxSubResource}, \code{L202.RenewRsrcCurves},
#' \code{L202.UnlimitedRenewRsrcCurves}, \code{L202.UnlimitedRenewRsrcPrice}, \code{L202.Supplysector_oil},
#' \code{L202.StubTechFractSecOut_oil}, \code{L202.GlobalTechShrwt_oil}, \code{L202.GlobalTechCoef_oil},
#' \code{L202.StubTechProd_oil}, \code{L202.StubTechFractSecOut_oil}, \code{L202.StubTechFractProd_oil},
#' \code{L202.StubTechFractCalPrice_oil}, \code{L202.StubTechCoef_oil}, \code{L202.Supplysector_in},
#' \code{L202.SubsectorAll_in}, \code{L202.StubTech_in}, \code{L202.StubTechInterp_in}, \code{L202.GlobalTechCoef_in},
#' \code{L202.GlobalTechShrwt_in}, \code{L202.StubTechProd_in}, \code{L202.Supplysector_an},
#' \code{L202.SubsectorAll_an}, \code{L202.StubTech_an}, \code{L202.StubTechInterp_an}, \code{L202.StubTechProd_an},
#' \code{L202.StubTechCoef_an}, \code{L202.GlobalTechCost_an}. The corresponding file in the original data system was \code{L202.an_input.R}
#' (aglu level2).
#' @details This chunk produces 22 animal-related resource tables: production, import, resource curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join bind_rows distinct filter if_else group_by left_join mutate select summarise
#' @importFrom tidyr complete replace_na
#' @author BBL August 2017
module_aglu_L202.an_input <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_regions",
             FILE = "aglu/A_agRenewRsrc",
             FILE = "aglu/A_agRenewRsrcCurves",
             FILE = "aglu/A_agRsrc",
             FILE = "aglu/A_agSubRenewRsrc",
             FILE = "aglu/A_agUnlimitedRsrcCurves",
             FILE = "aglu/A_an_input_supplysector",
             FILE = "aglu/A_an_input_subsector",
             FILE = "aglu/A_an_input_technology",
             FILE = "aglu/A_Crushing_sector",
             FILE = "aglu/A_Crushing_subsector",
             FILE = "aglu/A_Crushing_technology_coef",
             FILE = "aglu/A_Crushing_technology_cost",
             FILE = "aglu/A_Crushing_technology_secout",
             FILE = "aglu/A_an_input_globaltech_shrwt",
             FILE = "aglu/A_an_supplysector",
             FILE = "aglu/A_an_subsector",
             FILE = "aglu/A_an_technology",
             "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
             "L107.an_FeedIO_R_C_Sys_Fd_Y",
             "L107.an_Feed_Mt_R_C_Sys_Fd_Y",
             "L108.ag_Feed_Mt_R_C_Y",
             "L109.ag_ALL_Mt_R_C_Y",
             "L109.an_ALL_Mt_R_C_Y",
             "L1091.GrossTrade_Mt_R_C_Y",
             "L132.ag_an_For_Prices",
             "L1321.ag_prP_R_C_75USDkg",
             "L1321.an_prP_R_C_75USDkg"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L202.RenewRsrc",
             "L202.RenewRsrcPrice",
             "L202.Rsrc",
             "L202.RsrcPrice",
             "L202.maxSubResource",
             "L202.RenewRsrcCurves",
             "L202.ResTechShrwt",
             "L202.UnlimitedRenewRsrcCurves",
             "L202.UnlimitedRenewRsrcPrice",
             "L202.Supplysector_oil",
             "L202.SubsectorAll_oil",
             "L202.GlobalTechShrwt_oil",
             "L202.GlobalTechCoef_oil",
             "L202.StubTechProd_oil",
             "L202.StubTechFractSecOut_oil",
             "L202.StubTechFractProd_oil",
             "L202.StubTechFractCalPrice_oil",
             "L202.StubTechCoef_oil",
             "L202.Supplysector_in",
             "L202.SubsectorAll_in",
             "L202.SubsectorInterpTo_in",
             "L202.StubTech_in",
             "L202.StubTechInterp_in",
             "L202.GlobalTechCost_oil",
             "L202.GlobalTechCoef_in",
             "L202.GlobalTechShrwt_in",
             "L202.StubTechProd_in",
             "L202.Supplysector_an",
             "L202.SubsectorAll_an",
             "L202.GlobalTechShrwt_an",
             "L202.StubTechInterp_an",
             "L202.StubTechProd_an",
             "L202.StubTechCoef_an",
             "L202.StubTechCost_an",
             "L202.ag_consP_R_C_75USDkg"
             ))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- market <- region <- renewresource <- GCAM_commodity <- GCAM_region_ID <- Prod_Mt <- value <-
      sub.renewable.resource <- year <- price <- unlimited.resource <- technology <- supplysector <-
      subsector <- share.weight <- calOutputValue <- subs.share.weight <- coefficient <- maxSubResource <-
      stub.technology <- output_supplysector <- grade <- extractioncost <- calPrice <- unit <- share_Fd <-
      feed <- wtd_price <- Feed_Mt <- FeedPrice_USDkg <- FeedCost_bilUSD <- CommodityPrice_USDkg <-
      FeedCost_USDkg <- nonFeedCost <- NetExp_Mt <- share.weight.year <- fixedOutput <- ethanol <-
      biomassOil_tech <- biodiesel <- resource <- fractional.secondary.output <- primary.crop <-
      resource <- subresource <- default_price <- revenue <- weight <- SalesRevenue_bilUSD <- tradedP <-
      Exp_wtd_price <- ImpShare <- PrP <- GrossExp_Mt <-
      minicam.energy.input <- output.ratio <- FeedOut <- Crushing_Mt <- Input <-
      P0 <- P1 <- variable <- input.cost <- NULL  # silence package check notes

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    A_agRenewRsrc <- get_data(all_data, "aglu/A_agRenewRsrc", strip_attributes = TRUE)
    A_agRenewRsrcCurves <- get_data(all_data, "aglu/A_agRenewRsrcCurves", strip_attributes = TRUE)
    A_agRsrc <- get_data(all_data, "aglu/A_agRsrc", strip_attributes = TRUE)
    A_agSubRenewRsrc <- get_data(all_data, "aglu/A_agSubRenewRsrc", strip_attributes = TRUE)
    A_agUnlimitedRsrcCurves <- get_data(all_data, "aglu/A_agUnlimitedRsrcCurves", strip_attributes = TRUE)
    A_an_input_supplysector <- get_data(all_data, "aglu/A_an_input_supplysector", strip_attributes = TRUE)
    A_an_input_subsector <- get_data(all_data, "aglu/A_an_input_subsector", strip_attributes = TRUE)
    A_an_input_technology <- get_data(all_data, "aglu/A_an_input_technology", strip_attributes = TRUE)
    A_Crushing_sector <- get_data(all_data, "aglu/A_Crushing_sector")
    A_Crushing_subsector <- get_data(all_data, "aglu/A_Crushing_subsector")
    A_Crushing_technology_coef <- get_data(all_data, "aglu/A_Crushing_technology_coef")
    A_Crushing_technology_cost <- get_data(all_data, "aglu/A_Crushing_technology_cost")
    A_Crushing_technology_secout <- get_data(all_data, "aglu/A_Crushing_technology_secout")
    A_an_input_globaltech_shrwt <- get_data(all_data, "aglu/A_an_input_globaltech_shrwt")
    A_an_supplysector <- get_data(all_data, "aglu/A_an_supplysector", strip_attributes = TRUE)
    A_an_subsector <- get_data(all_data, "aglu/A_an_subsector", strip_attributes = TRUE)
    A_an_technology <- get_data(all_data, "aglu/A_an_technology", strip_attributes = TRUE)
    L132.ag_an_For_Prices <- get_data(all_data, "L132.ag_an_For_Prices", strip_attributes = TRUE)
    L109.ag_ALL_Mt_R_C_Y <- get_data(all_data, "L109.ag_ALL_Mt_R_C_Y", strip_attributes = TRUE)
    L1091.GrossTrade_Mt_R_C_Y <- get_data(all_data, "L1091.GrossTrade_Mt_R_C_Y", strip_attributes = TRUE)
    L1321.ag_prP_R_C_75USDkg <- get_data(all_data, "L1321.ag_prP_R_C_75USDkg", strip_attributes = TRUE)
    L1321.an_prP_R_C_75USDkg <- get_data(all_data, "L1321.an_prP_R_C_75USDkg", strip_attributes = TRUE)

    # 2. Build tables
    # Base table for resources - add region names to Level1 data tables (lines 49-70 old file)

    # Following datasets are already 'long' so just skip the old interpolate_and_melt step
    # Helper function to get_data, join with GCAM region names, and filter to base years
    get_join_filter <- function(x) {   #
      get_data(all_data, x, strip_attributes = TRUE) %>%
        left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
        filter(year %in% MODEL_BASE_YEARS)
    }

    L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt <- get_join_filter("L107.an_Prod_Mt_R_C_Sys_Fd_Y")
    L202.an_FeedIO_R_C_Sys_Fd_Y.mlt <- get_join_filter("L107.an_FeedIO_R_C_Sys_Fd_Y")
    L202.an_Feed_Mt_R_C_Sys_Fd_Y.mlt <- get_join_filter("L107.an_Feed_Mt_R_C_Sys_Fd_Y")
    L202.ag_Feed_Mt_R_C_Y.mlt <- get_join_filter("L108.ag_Feed_Mt_R_C_Y")
    L202.ag_ALL_Mt_R_C_Y <- get_join_filter("L109.ag_ALL_Mt_R_C_Y")
    L202.an_ALL_Mt_R_C_Y <- get_join_filter("L109.an_ALL_Mt_R_C_Y")
    L202.GrossTrade_Mt_R_C_Y <- get_join_filter("L1091.GrossTrade_Mt_R_C_Y") %>%
      filter(year == max(MODEL_BASE_YEARS))

    # L202.RenewRsrc: generic resource attributes
    # Here, and in general below, we extend data across all GCAM regions for a particular set of
    # level 2 output columns; here also substitute region data when market is "regional"
    A_agRenewRsrc %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["RenewRsrc"]], GCAM_region_names) %>%
      mutate(market = if_else(market == "regional", region, market)) ->
      L202.RenewRsrc

    # L202.RenewRsrcPrice: resource prices
    L202.RenewRsrc %>%
      select(region, renewresource) %>%
      mutate(year = min(MODEL_BASE_YEARS), price = gcam.DEFAULT_PRICE) ->
      L202.RenewRsrcPrice

    # L202.Rsrc: depletable resources in aglu
    A_agRsrc %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Rsrc"]], GCAM_region_names) %>%
      mutate(market = if_else(market == "regional", region, market)) ->
      L202.Rsrc

    # L202.RsrcPrice: depletable resource prices Ideally, feedcake prices should be set equal to the price of their
    # primary commodities, in each region However because the prices observed in each region reflect a weighted average
    # between domestic production and imports, and because producer prices on indigenous production in some regions
    # (e.g., Japan, Korea) are very high, the producer prices can be significantly higher than the consumer prices. In
    # these cases, the secondary output feedcakes (e.g., soybean feedcakes) would be priced higher than the input
    # feedstock (e.g. soybeans), causing the production technology's costs to potentially be negative. The method below
    # first computes the average price for the "traded" commodity (weighted avg of exporting regions), and then computes
    # the weighted average consumer prices in each region.
    L202.ag_prP_R_C_75USDkg <- L1321.ag_prP_R_C_75USDkg %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID")

    L202.GlobalExportPrices <- L202.ag_prP_R_C_75USDkg %>%
      filter(GCAM_commodity %in% A_Crushing_technology_coef$primary.crop) %>%
      left_join_error_no_match(select(L202.GrossTrade_Mt_R_C_Y, region, GCAM_commodity, GrossExp_Mt),
                               by = c("region", "GCAM_commodity")) %>%
      mutate(Revenue_bil75USD = value * GrossExp_Mt) %>%
      group_by(GCAM_commodity) %>%
      summarise(GrossExp_Mt = sum(GrossExp_Mt),
                Revenue_bil75USD = sum(Revenue_bil75USD)) %>%
      ungroup() %>%
      mutate(GlobalPrice_75USDkg = Revenue_bil75USD / GrossExp_Mt) %>%
      select(GCAM_commodity, GlobalPrice_75USDkg)

    # Compute the "regional" prices of each primary commodity, as the regional commodity price times the domestic
    # consumption plus the global price times the import comsumption, all divided by total consumption
    # This is done for the primary commodities that correspond to the feedcake commodities
    L202.RegionalConsumerPrices <- L202.ag_prP_R_C_75USDkg %>%
      inner_join(select(A_Crushing_technology_coef, GCAM_commodity = primary.crop, oil_commodity = supplysector),
                 by = "GCAM_commodity") %>%
      left_join_error_no_match(select(A_Crushing_technology_secout, oil_commodity = supplysector, feedcake_commodity = fractional.secondary.output),
                               by = "oil_commodity") %>%
      left_join_error_no_match(select(L202.GrossTrade_Mt_R_C_Y,
                                      region, year, GCAM_commodity, GrossImp_Mt, GrossExp_Mt),
                               by = c("region", "GCAM_commodity")) %>%
      left_join_error_no_match(select(L202.ag_ALL_Mt_R_C_Y, region, GCAM_commodity, year, Prod_Mt),
                               by = c("region", "year", "GCAM_commodity")) %>%
      left_join_error_no_match(L202.GlobalExportPrices, by = "GCAM_commodity") %>%
      mutate(TotalCost_bil75USD = GlobalPrice_75USDkg * GrossImp_Mt + (Prod_Mt - GrossExp_Mt) * value,
             Consumption_Mt = Prod_Mt + GrossImp_Mt - GrossExp_Mt,
             AvgPrice_75USDkg = if_else(Consumption_Mt == 0, GlobalPrice_75USDkg, TotalCost_bil75USD / Consumption_Mt)) %>%
      select(region, feedcake_commodity, AvgPrice_75USDkg)

    # 4/9/2021 gpk hack - the above table doesn't have Taiwan, which is missing from the price databases and the GDP deflators.
    # Just copy from China
    L202.RegionalConsumerPrices <- bind_rows(
      L202.RegionalConsumerPrices,
      filter(L202.RegionalConsumerPrices, region == "China") %>% mutate(region = "Taiwan")
    )

    L202.Rsrc %>%
      select(region, resource) %>%
      inner_join(L202.RegionalConsumerPrices, by = c("region", resource = "feedcake_commodity")) %>%
      mutate(price = round(AvgPrice_75USDkg, aglu.DIGITS_CALPRICE)) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      select(region, resource, year, price) ->
      L202.RsrcPrice

    L202.ag_Feed_Mt_R_C_Y.mlt %>%
      filter(GCAM_commodity %in% A_agRenewRsrcCurves$sub.renewable.resource) %>%
      group_by(region, GCAM_region_ID, GCAM_commodity) %>%
      summarise(maxSubResource = max(value)) %>%
      # bind the two tables together, re-name the columns to the appropriate headers, and add in a sub.renewable.resource category
      ungroup %>%
      mutate(sub.renewable.resource = GCAM_commodity,
             maxSubResource = round(maxSubResource, aglu.DIGITS_CALOUTPUT),
             year.fillout = min(MODEL_BASE_YEARS)) %>%
      left_join_keep_first_only(select(A_agRenewRsrcCurves, sub.renewable.resource, renewresource), by = "sub.renewable.resource") %>%
      select(LEVEL2_DATA_NAMES[["maxSubResource"]]) ->
      L202.maxSubResource

    # L202.RenewRsrcCurves
    A_agRenewRsrcCurves %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["RenewRsrcCurves"]], GCAM_region_names) ->
      L202.RenewRsrcCurves

    # L261.ResTechShrwt_C
    A_agSubRenewRsrc %>%
      rename(resource = renewresource, subresource = sub.renewable.resource) %>%
      repeat_add_columns(GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(technology = subresource,
             share.weight = 1.0) %>%
      select(LEVEL2_DATA_NAMES[["ResTechShrwt"]]) ->
    L202.ResTechShrwt

    # L202.UnlimitedRenewRsrcCurves
    A_agUnlimitedRsrcCurves %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["UnlimitRsrc"]], GCAM_region_names) ->
      L202.UnlimitedRenewRsrcCurves

    # L202.UnlimitedRenewRsrcPrice (105-112)
    L202.an_prP_R_C_75USDkg <- left_join_error_no_match(L1321.an_prP_R_C_75USDkg,
                                                         GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID)

    A_agUnlimitedRsrcCurves %>%
      select(unlimited.resource, price) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]], GCAM_region_names) %>%
      # replace these default prices with the prices calculated in L1321
      left_join(L202.an_prP_R_C_75USDkg, by = c("region", unlimited.resource = "GCAM_commodity")) %>%
      mutate(price = if_else(!is.na(value), value, price)) %>%
      select(LEVEL2_DATA_NAMES[["UnlimitRsrcPrice"]]) ->
      L202.UnlimitedRenewRsrcPrice

    # L202.Supplysector_oil: supplysectors of oil crushing
    A_Crushing_sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.Supplysector_oil

    # L202.SubsectorAll_oil: subsector info
    A_Crushing_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.SubsectorAll_oil

    # L202.StubTechShrwt_oil: stub technology share-weights of oil/feedcake production
    # Just default it to 1; whether the tech is available will be determined by the subsector share-weights
    A_Crushing_technology_coef %>%
      rename(sector.name = supplysector, subsector.name = subsector) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(share.weight = 1) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]]) ->
      L202.GlobalTechShrwt_oil

    A_Crushing_technology_cost %>%
      gather_years(value_col = "input.cost") %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input),
               year = sort(unique(c(year, MODEL_YEARS)))) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(sector.name = supplysector, subsector.name = subsector,
             input.cost = round(approx_fun(year, input.cost), aglu.DIGITS_CALPRICE)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L202.GlobalTechCost_oil

    # L202.GlobalTechCoef_oil: default coefficients for inputs to oil/feedcake production
    A_Crushing_technology_coef %>%
      gather_years(value_col = "coefficient") %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input),
               year = sort(unique(c(year, MODEL_YEARS)))) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(sector.name = supplysector, subsector.name = subsector,
             coefficient = approx_fun(year, coefficient)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L202.GlobalTechCoef_oil

    # L202.StubTechProd_oil: calibrated production of oil (Prod_Mt of oil)
    L202.ag_ALL_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% A_Crushing_technology_coef$supplysector) %>%
      rename(supplysector = GCAM_commodity) %>%
      left_join_error_no_match(select(A_Crushing_technology_coef, supplysector, subsector, technology),
                               by = "supplysector") %>%
      select(region, supplysector, subsector, stub.technology = technology, year, calOutputValue = Prod_Mt) %>%
      set_subsector_shrwt() %>%
      mutate(share.weight.year = year,
             tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L202.StubTechProd_oil

    # L202.StubTechFractSecOut_oil: calibrated secondary output production of feedcakes (Prod_Mt of feedcakes / Prod_Mt of oil)
    L202.ag_ALL_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% A_Crushing_technology_secout$fractional.secondary.output) %>%
      rename(fractional.secondary.output = GCAM_commodity) %>%
      left_join_error_no_match(select(A_Crushing_technology_secout, supplysector, subsector, technology, fractional.secondary.output),
                               by = "fractional.secondary.output") %>%
      select(region, supplysector, subsector, stub.technology = technology, fractional.secondary.output, year, FeedOut = Prod_Mt) %>%
      left_join_error_no_match(L202.StubTechProd_oil, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(output.ratio = round(FeedOut / calOutputValue, digits = aglu.DIGITS_CALOUTPUT)) %>%
      # no need to calibrate stub tech sec out ratio where there was no oil/meal production
      filter(!is.na(output.ratio)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFractSecOut"]]) ->
      L202.StubTechFractSecOut_oil

    # Repeat final cal year values to all future time periods
    L202.StubTechFractSecOut_oil <- bind_rows(
      L202.StubTechFractSecOut_oil,
      repeat_add_columns(L202.StubTechFractSecOut_oil %>%
                           filter(year == max(MODEL_BASE_YEARS)) %>%
                           select(-year),
                         tibble(year = MODEL_FUTURE_YEARS))
    )

    # L202.StubTechCoef_oil: calibrated IO coefficient of oil (Crushing_Mt of crop / Prod_Mt of oil)
    L202.ag_ALL_Mt_R_C_Y %>%
      filter(GCAM_commodity %in% A_Crushing_technology_coef$primary.crop) %>%
      rename(primary.crop = GCAM_commodity) %>%
      left_join_error_no_match(select(A_Crushing_technology_coef, supplysector, subsector, technology, minicam.energy.input, primary.crop),
                               by = "primary.crop") %>%
      select(region, supplysector, subsector, stub.technology = technology, minicam.energy.input, year, Input = Crushing_Mt) %>%
      left_join_error_no_match(L202.StubTechProd_oil, by = c("region", "supplysector", "subsector", "stub.technology", "year")) %>%
      mutate(coefficient = round(Input / calOutputValue, digits = aglu.DIGITS_CALOUTPUT),
             market.name = region) %>%
      # no need to calibrate stub tech IO coef where there was no oil/meal production
      filter(!is.na(coefficient)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L202.StubTechCoef_oil

    # # Repeat final cal year values to all future time periods
    # L202.StubTechCoef_oil <- bind_rows(
    #   L202.StubTechCoef_oil,
    #   repeat_add_columns(L202.StubTechCoef_oil %>%
    #                        filter(year == max(MODEL_BASE_YEARS)) %>%
    #                        select(-year),
    #                      tibble(year = MODEL_FUTURE_YEARS))
    # )


    # L202.StubTechFractProd_oil
    L202.StubTechFractSecOut_oil %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-output.ratio) %>%
      mutate(P0 = 0) %>%
      left_join_error_no_match(L202.RsrcPrice, by = c("region", fractional.secondary.output = "resource", "year")) %>%
      mutate(P1 = round(price * (1 - aglu.FEEDCAKE_PRICE_MARGIN), digits = energy.DIGITS_COST)) %>%
      gather(key = "variable", value = "price", P0, P1) %>%
      mutate(fraction.produced = as.numeric( sub("P", "", variable ) )) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFractProd"]]) ->
      L202.StubTechFractProd_oil

    # Repeat final cal year values to all future time periods
    L202.StubTechFractProd_oil <- bind_rows(
      L202.StubTechFractProd_oil,
      repeat_add_columns(L202.StubTechFractProd_oil %>%
                           filter(year == max(MODEL_BASE_YEARS)) %>%
                           select(-year),
                         tibble(year = MODEL_FUTURE_YEARS))
    )

    # L202.StubTechFractCalPrice_oil
    # Calibrate the price (as a fixed price, not a point on a supply curve) in the base year
    L202.StubTechFractSecOut_oil %>%
      select(-output.ratio) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L202.RsrcPrice, by = c("region", fractional.secondary.output = "resource", "year")) %>%
      mutate(calPrice = round(price, digits = energy.DIGITS_COST)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechFractCalPrice"]]) ->
      L202.StubTechFractCalPrice_oil

    # L202.Supplysector_in: generic supplysector info for inputs to animal production (114-122)
    A_an_input_supplysector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.Supplysector_in

    # L202.SubsectorAll_in: generic subsector info for inputs to animal production technologies
    A_an_input_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.SubsectorAll_in

    # If any subsectors have a to-value provided, generate another table: L202.SubsectorInterpTo_in
    #if(any(!is.na(A_an_input_subsector$to.value))) {
    # A_an_input_subsector %>%
    #   filter(!is.na(to.value)) %>%
    #   write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterpTo"]], GCAM_region_names = GCAM_region_names) ->
    #   L202.SubsectorInterpTo_in
    #}

    # 8/23/2019 hack - splitting up the DDGS regions, combined with many regions having zero corn
    # ethanol production in any years, means that some regions have no capability to produce DDGS. These regions
    # need to have their DDGS demand zeroed out. The steps below identify these regions, and over-write the
    # default share-weight interpolation rules to keep the demand to zero.

    # 09/27/2019 JS: I delete these assumption, as the DDG trade allows these regions to have some DDGS on the feed mix
    #no_ddgs_regions <- unique(A_regions$region[A_regions$ethanol != "corn ethanol"])
    #L202.SubsectorAll_in$share.weight[L202.SubsectorAll_in$region %in% no_ddgs_regions &
                                        #L202.SubsectorAll_in$subsector == "DDGS"] <- 0


    # L202.StubTech_in: identification of stub technologies for inputs to animal production (124-140)
    A_an_input_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTech_in

    # L202.StubTechInterp_in: generic technology info for inputs to animal production
    A_an_input_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTechInterp_in

    # L202.GlobalTechCoef_in: coefficients for inputs to animal production
    A_an_input_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sector.name = supplysector, subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L202.GlobalTechCoef_in

    # L202.GlobalTechShrwt_in: Default shareweights for inputs to animal production
    A_an_input_globaltech_shrwt %>%
      gather_years(value_col = "share.weight") %>%
      complete(nesting(supplysector, subsector, technology), year = sort(unique(c(year, MODEL_YEARS)))) %>%
      mutate(share.weight = approx_fun(year, share.weight, rule = 2),
             sector.name = supplysector, subsector.name = subsector) %>%
      filter(year %in% MODEL_YEARS) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L202.GlobalTechShrwt_in

    # L202.StubTechProd_in: base year output of the inputs (feed types) to animal production (142-149)
    A_an_input_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      mutate(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      # not every region/technology/year has a match, so need to use left_join
      left_join(L202.ag_Feed_Mt_R_C_Y.mlt, by = c("region", "technology" = "GCAM_commodity", "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      # subsector and technology shareweights (subsector requires the year as well)
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L202.StubTechProd_in

    # L202.Supplysector_an: generic animal production supplysector info (159-162)
    A_an_supplysector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.Supplysector_an

    # L202.SubsectorAll_an: generic animal production subsector info (164-167)
    A_an_subsector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorAll"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L202.SubsectorAll_an

    # L202.GlobalTechShrwt_an: global technology default share-weights
    A_an_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(sector.name = supplysector,
             subsector.name = subsector,
             share.weight = 1) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["GlobalTechShrwt"]], GCAM_region_names) ->
      L202.GlobalTechShrwt_an

    # L202.StubTechInterp_an: shareweight interpolation for animal production technologies (173-175)
    A_an_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["TechInterp"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L202.StubTechInterp_an

    # L202.StubTechProd_an: animal production by technology and region (177-199)
    A_an_technology %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      left_join_error_no_match(L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt,
                               by = c("region", "supplysector" = "GCAM_commodity",
                                      "subsector" = "system", "stub.technology" = "feed",
                                      "year")) %>%
      mutate(calOutputValue = round(value, aglu.DIGITS_CALOUTPUT),
             # subsector and technology shareweights (subsector requires the year as well)
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechProd"]]) ->
      L202.StubTechProd_an

    # In general, technologies need to be aggregated to compute subsector share-weights. If any technology
    # within a subsector has a value > 0, then the subsector share-weight should be 1.
    # Some subsectors have multiple technologies, so shareweights should be derived from aggregation
    L202.StubTechProd_an %>%
      group_by(region, supplysector, subsector, year) %>%
      summarise(subs.share.weight = sum(calOutputValue)) %>%
      ungroup %>%
      mutate(subs.share.weight = if_else(subs.share.weight > 0, 1, 0)) ->
      L202.an_subs_sw

    # Override the share weights in the production table
    L202.StubTechProd_an %>%
      select(-subs.share.weight) %>%
      left_join_error_no_match(L202.an_subs_sw, by = c("region", "supplysector", "subsector", "year")) ->
      L202.StubTechProd_an

    # L202.StubTechCoef_an: animal production input-output coefficients by technology and region (201-214)
    A_an_technology %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Tech"]], "minicam.energy.input", "market.name"), GCAM_region_names) %>%
      rename(stub.technology = technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS))) %>%
      # not everything has a match so need to use left_join
      left_join(L202.an_FeedIO_R_C_Sys_Fd_Y.mlt,
                by = c("region", "supplysector" = "GCAM_commodity",
                       "subsector" = "system", "minicam.energy.input" = "feed",
                       "year")) %>%
      mutate(coefficient = round(value, aglu.DIGITS_CALOUTPUT)) %>%
      select(-value, -GCAM_region_ID) ->
      L202.StubTechCoef_an

    # For values beyond the coefficient time series, use the final available year
    final_coef_year <- max(L202.an_FeedIO_R_C_Sys_Fd_Y.mlt$year)
    final_coef_year_data <- filter(L202.StubTechCoef_an, year == final_coef_year) %>% select(-year)
    L202.StubTechCoef_an %>%
      filter(year > final_coef_year) %>%
      select(-coefficient) %>%
      left_join(final_coef_year_data, by = c("region", "supplysector", "subsector", "stub.technology", "minicam.energy.input", "market.name")) %>%
      bind_rows(filter(L202.StubTechCoef_an, ! year > final_coef_year)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]]) ->
      L202.StubTechCoef_an

    # Supplemental calculation of non-input cost of animal production (216-261)
    # Calculate non-feed costs of animal production based on regional producer prices and feed costs
    # First, calculate the weighted average price across the different feed types (supplysectors)
    L202.StubTechProd_in %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(region, supplysector, subsector, stub.technology, calOutputValue) ->
      L202.ag_Feed_P_share_R_C

    # Use the producer prices of crops in each exporting region to calculate the global "traded" crop prices
    L202.ag_tradedP_C_75USDkg <- filter(L1091.GrossTrade_Mt_R_C_Y, year == max(MODEL_BASE_YEARS)) %>%
      inner_join(L1321.ag_prP_R_C_75USDkg, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(Exp_wtd_price = GrossExp_Mt * value) %>%
      group_by(GCAM_commodity) %>%
      summarise(GrossExp_Mt = sum(GrossExp_Mt),
                Exp_wtd_price = sum(Exp_wtd_price)) %>%
      ungroup() %>%
      mutate(tradedP = Exp_wtd_price / GrossExp_Mt) %>%
      select(GCAM_commodity, tradedP)

    # Calculate the share of domestic supply (i.e., total consumption) that is from imports
    L202.ag_ImpShare_Mt_R_C_Y <- filter(L109.ag_ALL_Mt_R_C_Y, year == max(MODEL_BASE_YEARS)) %>%
      select(GCAM_region_ID, GCAM_commodity, year, Supply_Mt) %>%
      left_join(L1091.GrossTrade_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      mutate(ImpShare = if_else(is.na(GrossImp_Mt) | Supply_Mt == 0, 0, GrossImp_Mt / Supply_Mt)) %>%
      select(GCAM_region_ID, GCAM_commodity, ImpShare)

    # Calculate the weighted average regional crop prices, as the global traded crop price times the
    # import share plus the local producer price times the domestic source share (1 - ImpShare)
    L202.ag_consP_R_C_75USDkg <- L1321.ag_prP_R_C_75USDkg %>%
      rename(PrP = value) %>%
      left_join_error_no_match(L202.ag_ImpShare_Mt_R_C_Y, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      left_join_error_no_match(L202.ag_tradedP_C_75USDkg, by = "GCAM_commodity", ignore_columns = "tradedP") %>%
      mutate(value = if_else(is.na(tradedP),
                             PrP,
                             PrP * (1 - ImpShare) + tradedP * ImpShare)) %>%
      select(GCAM_region_ID, GCAM_commodity, value)

    # Remove meat prices here since meat is not used as feed. And even if it does, regional prices should be used!
    # This will need to be udpated if meat outputs are included in the feed.
    #L202.ag_prP_R_C_75USDkg <- bind_rows(L202.ag_consP_R_C_75USDkg, L1321.an_prP_R_C_75USDkg) %>%
    L202.ag_prP_R_C_75USDkg <- bind_rows(L202.ag_consP_R_C_75USDkg) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, price = value)
    L202.rsrcP_R_C_75USDkg <- filter(A_agRenewRsrcCurves, grade == "grade 2") %>%
      select(GCAM_commodity = sub.renewable.resource, calPrice = extractioncost)

    # Here we are using USA prices for all regions for FodderHerb, Residue, Scavenging_Other
    L202.ag_Feed_Prices <- L132.ag_an_For_Prices %>%
      filter(GCAM_commodity %in% unique(L202.ag_Feed_P_share_R_C$subsector)) %>%
      bind_rows(L202.rsrcP_R_C_75USDkg) %>%
      write_to_all_regions(c("region", "GCAM_commodity", "calPrice"), GCAM_region_names) %>%
      rename(default_price = calPrice) %>%
      left_join(L202.ag_prP_R_C_75USDkg, by = c("region", "GCAM_commodity")) %>%
      mutate(price = if_else(is.na(price), default_price, price)) %>%
      select(region, GCAM_commodity, price)

    L202.ag_Feed_P_share_R_C %>%
      # not all stub.technology values are present as commodities in the price data; DDGS and feedcakes return NA and are dropped
      left_join(L202.ag_Feed_Prices, by = c("region", "stub.technology" = "GCAM_commodity")) %>%
      drop_na(price) ->
      L202.ag_Feed_P_share_R_C

    L202.ag_Feed_P_share_R_C %>%
      group_by(region, supplysector) %>%
      summarise(revenue = sum(price * calOutputValue),
                weight = sum(calOutputValue)) %>%
      ungroup() %>%
      mutate(price = revenue / weight) %>%
      select(region, supplysector, price) ->
      L202.ag_FeedCost_USDkg_R_F

    # gpk 8/5/2020 - compute default animal commodity prices in case of missing values
    # China prices, to be used in Taiwan
    L202.ChinaAnPrices <- L1321.an_prP_R_C_75USDkg %>%
      filter(GCAM_region_ID == GCAM_region_names$GCAM_region_ID[GCAM_region_names$region == "China"]) %>%
      select(GCAM_commodity, ChinaCommodityPrice_USDkg = value)

    # Median prices, to be used elsewhere
    L202.DefaultAnPrices <- L1321.an_prP_R_C_75USDkg %>%
      group_by(GCAM_commodity) %>%
      summarise(DefaultCommodityPrice_USDkg = median(value)) %>%
      ungroup()

    # Calculate the total cost of all inputs, for each animal commodity, first matching in the feed quantity and the price
    L202.an_Prod_Mt_R_C_Sys_Fd_Y.mlt %>%
      filter(year == max(MODEL_BASE_YEARS),
             !region %in% aglu.NO_AGLU_REGIONS) %>%
      rename(Prod_Mt = value) %>%
      left_join_error_no_match(select(L202.an_Feed_Mt_R_C_Sys_Fd_Y.mlt, GCAM_region_ID, GCAM_commodity, system, feed, year, Feed_Mt = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "system", "feed", "year")) %>%
      left_join_error_no_match(L202.ag_FeedCost_USDkg_R_F, by = c("region", "feed" = "supplysector")) %>%
      rename(FeedPrice_USDkg = price) %>%
      left_join(L1321.an_prP_R_C_75USDkg, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      # gpk 2020-08-05 We don't have animal commodity price data for Taiwan here. Set that to China's data, and for any
      # other regions that may have missing data, use the median values computed above as defaults
      left_join_error_no_match(L202.ChinaAnPrices, by = "GCAM_commodity") %>%
      left_join_error_no_match(L202.DefaultAnPrices, by = "GCAM_commodity") %>%
      mutate(CommodityPrice_USDkg = if_else(region == "Taiwan" & is.na(value), ChinaCommodityPrice_USDkg, value),
             CommodityPrice_USDkg = if_else(is.na(CommodityPrice_USDkg), DefaultCommodityPrice_USDkg, CommodityPrice_USDkg)) %>%
      # multiply prices by quantities to calculate feed expenditures and commodity sales revenues
      mutate(SalesRevenue_bilUSD = Prod_Mt * CommodityPrice_USDkg,
             FeedCost_bilUSD = Feed_Mt * FeedPrice_USDkg) %>%
      # group by region, meat commodity, and system (i.e., assuming the same non-feed cost for the 'mixed' and 'pastoral' systems
      # irrespective of the feed type, but the costs can differ by region, commodity, and system)
      group_by(region, GCAM_commodity, system) %>%
      summarise(Prod_Mt = sum(Prod_Mt),
                SalesRevenue_bilUSD = sum(SalesRevenue_bilUSD),
                FeedCost_bilUSD = sum(FeedCost_bilUSD)) %>%
      ungroup() %>%
      mutate(nonFeedCost = if_else(Prod_Mt == 0, 0, (SalesRevenue_bilUSD - FeedCost_bilUSD) / Prod_Mt)) ->
      L202.an_nonFeedCost_R_C

    # L202.StubTechCost_an: costs of animal production technologies (263-270)
    A_an_technology %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(!region %in% aglu.NO_AGLU_REGIONS) %>%
      mutate(stub.technology = technology,
             minicam.non.energy.input = "non-feed") %>%
      left_join_error_no_match(select(L202.an_nonFeedCost_R_C, region, GCAM_commodity, system, nonFeedCost),
                               by = c("region", supplysector = "GCAM_commodity", subsector = "system")) %>%
      mutate(input.cost = round(nonFeedCost, aglu.DIGITS_CALPRICE)) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
      L202.StubTechCost_an

    # Also, remove DDGS and feedcake subsectors and technologies in regions where these commodities are not available
    #10/10 JS, updated 4/9/2021 gpk: DDGS can be imported to all the regions, so we need to leave these technologies intact

    # Produce outputs
    L202.RenewRsrc %>%
      add_title("Generic resource attributes") %>%
      add_units("NA") %>%
      add_comments("A_agRenewRsrc written to all regions") %>%
      add_legacy_name("L202.RenewRsrc") %>%
      add_precursors("aglu/A_agRenewRsrc", "common/GCAM_region_names") ->
      L202.RenewRsrc

    L202.RenewRsrcPrice %>%
      add_title("Resource prices") %>%
      add_units("1975$/kg") %>%
      add_comments("A_agRenewRsrc written to all regions with `year` set to first model base year and `price` = 1") %>%
      add_legacy_name("L202.RenewRsrcPrice") %>%
      add_precursors("aglu/A_agRenewRsrc", "common/GCAM_region_names") ->
      L202.RenewRsrcPrice

    L202.Rsrc %>%
      add_title("Generic resource attributes") %>%
      add_units("NA") %>%
      add_comments("A_agRsrc written to all regions") %>%
      add_precursors("aglu/A_agRsrc", "common/GCAM_region_names") ->
      L202.Rsrc

    L202.RsrcPrice %>%
      add_title("Resource prices") %>%
      add_units("1975$/kg") %>%
      add_comments("A_agRsrc written to all regions with `year` set to first model base year and `price` based on feed prices") %>%
      add_precursors("aglu/A_agRsrc", "common/GCAM_region_names", "L1091.GrossTrade_Mt_R_C_Y", "L1321.ag_prP_R_C_75USDkg") ->
      L202.RsrcPrice

    L202.maxSubResource %>%
      add_title("Maximum amount of resource production allowed in any period") %>%
      add_units("Mt/yr") %>%
      add_comments("Computed as the maximum of all base periods, for each region and resource") %>%
      add_legacy_name("L202.maxSubResource") %>%
      add_precursors("L109.an_ALL_Mt_R_C_Y", "L108.ag_Feed_Mt_R_C_Y", "aglu/A_agRenewRsrcCurves", "common/GCAM_region_names") ->
      L202.maxSubResource

    L202.RenewRsrcCurves %>%
      add_title("Renewable resource curves") %>%
      add_units("available: Unitless fraction of maxSubResource; extractioncost: 1975$/kg") %>%
      add_comments("A_agRenewRsrcCurves written to all regions") %>%
      add_legacy_name("L202.RenewRsrcCurves") %>%
      add_precursors("aglu/A_agRenewRsrcCurves", "common/GCAM_region_names") ->
      L202.RenewRsrcCurves

    L202.ResTechShrwt %>%
      add_title("Technology share-weights for the renewable resources") %>%
      add_units("NA") %>%
      add_comments("Mostly just to provide a shell of a technology for the resource to use") %>%
      add_precursors("aglu/A_agSubRenewRsrc", "common/GCAM_region_names") ->
      L202.ResTechShrwt

    L202.UnlimitedRenewRsrcCurves %>%
      add_title("Unlimited renewable resource curves") %>%
      add_units("Unitless") %>%
      add_comments("A_agUnlimitedRsrcCurves written to all regions") %>%
      add_legacy_name("L202.UnlimitedRenewRsrcCurves") %>%
      add_precursors("aglu/A_agUnlimitedRsrcCurves", "common/GCAM_region_names") ->
      L202.UnlimitedRenewRsrcCurves

    L202.UnlimitedRenewRsrcPrice %>%
      add_title("Unlimited renewable resource price") %>%
      add_units("1975$/kg") %>%
      add_comments("A_agUnlimitedRsrcCurves written to all regions") %>%
      add_legacy_name("L202.UnlimitedRenewRsrcPrice") %>%
      add_precursors("aglu/A_agUnlimitedRsrcCurves", "common/GCAM_region_names") ->
      L202.UnlimitedRenewRsrcPrice

    L202.Supplysector_oil %>%
      add_title("Generic supplysector info for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_Crushing_sector written to all regions") %>%
      add_precursors("aglu/A_Crushing_sector", "common/GCAM_region_names") ->
      L202.Supplysector_oil

    L202.SubsectorAll_oil %>%
      add_title("Generic subsector info for inputs to animal production technologies") %>%
      add_units("NA") %>%
      add_comments("A_Crushing_subsector written to all regions") %>%
      add_precursors("aglu/A_Crushing_subsector", "common/GCAM_region_names") ->
      L202.SubsectorAll_oil

    L202.GlobalTechShrwt_oil %>%
      add_title("Global tech default share-weights for crop oil production") %>%
      add_units("NA") %>%
      add_comments("Within these sectors, the competition happens only at the subsector level") %>%
      add_precursors("aglu/A_Crushing_technology_coef") ->
      L202.GlobalTechShrwt_oil

    L202.GlobalTechCost_oil %>%
      add_title("Global tech default costs for crop oil production") %>%
      add_units("1975$ / kg of oil") %>%
      add_comments("Includes costs of oil crop crushing and handling") %>%
      add_precursors("aglu/A_Crushing_technology_cost") ->
      L202.GlobalTechCost_oil

    L202.GlobalTechCoef_oil %>%
      add_title("Global tech default coefficients for crop oil production") %>%
      add_units("Unitless mass of feed per mass of oil") %>%
      add_comments("Coefs are generally over-written by calibration data") %>%
      add_precursors("aglu/A_Crushing_technology_coef") ->
      L202.GlobalTechCoef_oil

    L202.StubTechProd_oil %>%
      add_title("Calibrated production of crop oil") %>%
      add_units("Mt/yr") %>%
      add_comments("Calculated from FAOSTAT SUA data on crop oil production") %>%
      add_precursors("aglu/A_Crushing_technology_coef", "L109.ag_ALL_Mt_R_C_Y") ->
      L202.StubTechProd_oil

    L202.StubTechFractSecOut_oil %>%
      add_title("Calibrated secondary output ratios of feedcakes (feed / oil)") %>%
      add_units("Unitless feed / oil") %>%
      add_comments("Calculated from FAOSTAT SUA data on crop oil and crop meal production") %>%
      add_precursors("aglu/A_Crushing_technology_secout", "L109.ag_ALL_Mt_R_C_Y") ->
      L202.StubTechFractSecOut_oil

    L202.StubTechCoef_oil %>%
      add_title("Calibrated input-output coefficients of crop oil (input crop / output oil)") %>%
      add_units("Unitless crop / oil") %>%
      add_comments("Calculated from FAOSTAT SUA data on crop oil and crop meal production, plus assumed losses") %>%
      add_precursors("aglu/A_Crushing_technology_coef", "L109.ag_ALL_Mt_R_C_Y") ->
      L202.StubTechCoef_oil

    L202.StubTechFractProd_oil %>%
      add_title("Price and production fraction for secondary feedcake outputs of crop oil production") %>%
      add_units("1975$/kg, unitless fraction") %>%
      add_comments("Prices at P1 from L1321.ag_prP_R_C_75USDkg by way of L202.RsrcPrice") %>%
      same_precursors_as(L202.RsrcPrice) %>%
      add_precursors("aglu/A_Crushing_technology_coef") ->
      L202.StubTechFractProd_oil

    L202.StubTechFractCalPrice_oil %>%
      add_title("Calibrated prices of secondary output feedcakes from crop oil production") %>%
      add_units("1975$/kg") %>%
      add_comments("Prices from L1321.ag_prP_R_C_75USDkg by way of L202.RsrcPrice") %>%
      same_precursors_as(L202.RsrcPrice) %>%
      add_precursors("aglu/A_Crushing_technology_coef") ->
      L202.StubTechFractCalPrice_oil

    L202.Supplysector_in %>%
      add_title("Generic supplysector info for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_supplysector written to all regions") %>%
      add_legacy_name("L202.Supplysector_in") %>%
      add_precursors("aglu/A_an_input_supplysector", "common/GCAM_region_names") ->
      L202.Supplysector_in

    L202.SubsectorAll_in %>%
      add_title("Generic subsector info for inputs to animal production technologies") %>%
      add_units("NA") %>%
      add_comments("A_an_input_subsector written to all regions") %>%
      add_legacy_name("L202.SubsectorAll_in") %>%
      add_precursors("aglu/A_an_input_subsector", "energy/A_regions", "common/GCAM_region_names") ->
      L202.SubsectorAll_in

    if(exists("L202.SubsectorInterpTo_in")) {
      L202.SubsectorInterpTo_in %>%
        add_title("Subsector interpolation rules with to-value specified") %>%
        add_units("NA") %>%
        add_comments("From A_an_input_subsector, written to all regions if used") %>%
        same_precursors_as(L202.SubsectorAll_in) ->
        L202.SubsectorInterpTo_in
    } else {
      missing_data()  %>%
        add_comments("Empty data table") ->
        L202.SubsectorInterpTo_in
    }

    L202.StubTech_in %>%
      add_title("Identification of stub technologies for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_technology written to all regions") %>%
      add_legacy_name("L202.StubTech_in") %>%
      add_precursors("aglu/A_an_input_technology", "energy/A_regions", "common/GCAM_region_names") ->
      L202.StubTech_in

    L202.StubTechInterp_in %>%
      add_title("Generic technology info for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_technology written to all regions") %>%
      add_legacy_name("L202.StubTechInterp_in") %>%
      add_precursors("aglu/A_an_input_technology", "energy/A_regions", "common/GCAM_region_names") ->
      L202.StubTechInterp_in

    L202.GlobalTechCoef_in %>%
      add_title("Coefficients for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_technology across all base and future years") %>%
      add_comments("These technologies are pass-through, used for competing different primary sources for animal feed commodities.
                   No transformations are taking place, and the coefficients are 1 in all years.") %>%
      add_legacy_name("L202.GlobalTechCoef_in") %>%
      add_precursors("aglu/A_an_input_technology", "common/GCAM_region_names") ->
      L202.GlobalTechCoef_in

    L202.GlobalTechShrwt_in %>%
      add_title("Default shareweights for inputs to animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_input_globaltech_shrwt interpolated to all model years") %>%
      add_legacy_name("L202.GlobalTechShrwt_in") %>%
      add_precursors("aglu/A_an_input_globaltech_shrwt", "common/GCAM_region_names") ->
      L202.GlobalTechShrwt_in

    L202.StubTechProd_in %>%
      add_title("Base year output of the inputs (feed types) to animal production") %>%
      add_units("Mt/yr") %>%
      add_comments("Calibrated primary sources of animal feed commodities, specific to each region and time period.") %>%
      add_legacy_name("L202.StubTechProd_in") %>%
      add_precursors("aglu/A_an_input_technology", "L107.an_FeedIO_R_C_Sys_Fd_Y",
                     "energy/A_regions", "common/GCAM_region_names") ->
      L202.StubTechProd_in

    L202.Supplysector_an %>%
      add_title("Generic animal production supplysector info") %>%
      add_units("NA") %>%
      add_comments("A_an_supplysector written to all regions") %>%
      add_legacy_name("L202.Supplysector_an") %>%
      add_precursors("aglu/A_an_supplysector", "common/GCAM_region_names") ->
      L202.Supplysector_an

    L202.SubsectorAll_an %>%
      add_title("Generic animal production subsector info") %>%
      add_units("NA") %>%
      add_comments("A_an_subsector written to all regions") %>%
      add_legacy_name("L202.SubsectorAll_an") %>%
      add_precursors("aglu/A_an_subsector", "common/GCAM_region_names") ->
      L202.SubsectorAll_an

    L202.GlobalTechShrwt_an %>%
      add_title("Default share-weights of global technologies for animal production") %>%
      add_units("NA") %>%
      add_comments("A_an_technology written to all regions") %>%
      add_precursors("aglu/A_an_technology", "common/GCAM_region_names") ->
      L202.GlobalTechShrwt_an

    L202.StubTechInterp_an %>%
      add_title("Shareweight interpolation for animal production technologies") %>%
      add_units("NA") %>%
      add_comments("A_an_technology written to all regions") %>%
      add_legacy_name("L202.StubTechInterp_an") %>%
      add_precursors("aglu/A_an_technology", "common/GCAM_region_names") ->
      L202.StubTechInterp_an

    L202.StubTechProd_an %>%
      add_title("Calibrated animal commodity production by technology") %>%
      add_units("Unitless") %>%
      add_comments("Animal commodity production by subsector (mixed vs pastoral system) and technology (modeled feed commodity).") %>%
      add_legacy_name("L202.StubTechProd_an") %>%
      add_precursors("aglu/A_an_technology", "L107.an_Prod_Mt_R_C_Sys_Fd_Y", "common/GCAM_region_names") ->
      L202.StubTechProd_an

    L202.StubTechCoef_an %>%
      add_title("Animal production input-output coefficients by technology and region") %>%
      add_units("kg of dry feed per kg of produced animal commodity") %>%
      add_comments("Animal production input-output coefficients written across model base years and regions") %>%
      add_legacy_name("L202.StubTechCoef_an") %>%
      add_precursors("aglu/A_an_technology", "L107.an_Prod_Mt_R_C_Sys_Fd_Y", "common/GCAM_region_names") ->
      L202.StubTechCoef_an

    L202.StubTechCost_an %>%
      add_title("Costs of animal production technologies") %>%
      add_units("1975$/kg") %>%
      add_comments("Animal feed cost, prices, and technology") %>%
      add_comments("This is the non-feed cost; i.e., all costs of producing animal commodities except for the feed.") %>%
      add_legacy_name("L202.StubTechCost_an") %>%
      same_precursors_as(L202.StubTechCoef_an) %>%
      add_precursors("L132.ag_an_For_Prices", "L1321.ag_prP_R_C_75USDkg", "L1321.an_prP_R_C_75USDkg",
                     "L107.an_Feed_Mt_R_C_Sys_Fd_Y", "L1091.GrossTrade_Mt_R_C_Y", "L109.ag_ALL_Mt_R_C_Y") ->
      L202.StubTechCost_an

    # Return also the consumer prices, to be made available elsewhere
    L202.ag_consP_R_C_75USDkg %>%
      add_title("Consumer costs of crops") %>%
      add_units("1975$/kg") %>%
      add_comments("Computed from weighted average of domestically sourced crops (which use producer prices) and imports") %>%
      add_comments("Imported crop prices are computed from weighted average of producer prices of exporting countries") %>%
      add_precursors("L1321.ag_prP_R_C_75USDkg", "L109.ag_ALL_Mt_R_C_Y", "L1091.GrossTrade_Mt_R_C_Y") ->
      L202.ag_consP_R_C_75USDkg

    return_data(L202.RenewRsrc, L202.RenewRsrcPrice, L202.Rsrc, L202.RsrcPrice, L202.maxSubResource, L202.RenewRsrcCurves, L202.ResTechShrwt,
                L202.UnlimitedRenewRsrcCurves, L202.UnlimitedRenewRsrcPrice, L202.Supplysector_oil,
                L202.SubsectorAll_oil, L202.GlobalTechShrwt_oil, L202.GlobalTechCost_oil, L202.GlobalTechCoef_oil,
                L202.StubTechProd_oil, L202.StubTechFractSecOut_oil, L202.StubTechFractProd_oil, L202.StubTechFractCalPrice_oil,
                L202.StubTechCoef_oil, L202.Supplysector_in,
                L202.SubsectorAll_in, L202.SubsectorInterpTo_in, L202.StubTech_in, L202.StubTechInterp_in, L202.GlobalTechCoef_in,
                L202.GlobalTechShrwt_in, L202.StubTechProd_in, L202.Supplysector_an, L202.SubsectorAll_an,
                L202.GlobalTechShrwt_an, L202.StubTechInterp_an, L202.StubTechProd_an, L202.StubTechCoef_an,
                L202.StubTechCost_an, L202.ag_consP_R_C_75USDkg)
  } else {
    stop("Unknown command")
  }
}
