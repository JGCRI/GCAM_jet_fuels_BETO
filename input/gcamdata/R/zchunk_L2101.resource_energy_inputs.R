#' module_energy_L2101.resource_energy_inputs
#'
#' Resource market information, prices, TechChange parameters, supply curves, and environmental costs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2101.ResTechCoef_fos}, \code{L2101.SubRsrcDummyInfo_fos}.
#' @details Resource production input/output coefficients and dummy input flag.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author GPK March 2019
module_energy_L2101.resource_energy_inputs <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/A_regions",
             FILE = "energy/mappings/fuel_energy_input",
             FILE = "energy/A10.subrsrc_coef",
             "L132.in_EJ_R_rsrcenergy_F_Yh",
             "L210.RsrcCalProd"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2101.ResTechCoef_fos",
             "L2101.ResTechCoefAdj_fos"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    year <- resource <- subresource <- technology <- minicam.energy.input <-
      coefficient <- GCAM_region_ID <- region <- cal.production <- resource_class <-
      energy_input <- energy_total <- share <- value <- sector <- fuel <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions <- get_data(all_data, "energy/A_regions")
    fuel_energy_input <- get_data(all_data, "energy/mappings/fuel_energy_input")
    A10.subrsrc_coef <- get_data(all_data, "energy/A10.subrsrc_coef")
    L132.in_EJ_R_rsrcenergy_F_Yh <- get_data(all_data, "L132.in_EJ_R_rsrcenergy_F_Yh", strip_attributes = TRUE)
    L210.RsrcCalProd <- get_data(all_data, "L210.RsrcCalProd", strip_attributes = TRUE)

    # #=======#=======#=======#=======#=======#=======#=========

    L2101.ResTechCoef_fos <- gather_years(A10.subrsrc_coef, value_col = "coefficient") %>%
      complete(nesting(resource,subresource,technology,minicam.energy.input),
               year = c(year, MODEL_YEARS)) %>%
      group_by(resource,subresource,technology,minicam.energy.input) %>%
      mutate(coefficient = round(approx_fun(year, coefficient), energy.DIGITS_COEFFICIENT)) %>%
      ungroup() %>%
      repeat_add_columns(GCAM_region_names) %>%
      filter(year %in% MODEL_YEARS) %>%
      arrange(GCAM_region_ID, year) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["ResTechCoef"]])

    # These are default coefficients that apply on in future years. Base years are calculated from base-year reported
    # energy consumption divided by resource production
    # Because oil and gas are together in the base-year inventory data, they are separated according to bottom-up shares
    L2101.oilgas_shares_numerator <- L210.RsrcCalProd %>%
      # gets rid of unconventional oil if not in L2101.ResTechCoef_fos
      filter(subresource %in% L2101.ResTechCoef_fos$subresource) %>%
      left_join(L2101.ResTechCoef_fos, by = c("region", "resource", "subresource", "year")) %>%
      mutate(energy_input = cal.production * coefficient,
             resource_class = if_else(resource %in% c("crude oil", "natural gas"), "oilgas", resource))
    L2101.oilgas_shares_denominator <- L2101.oilgas_shares_numerator %>%
      group_by(region, resource_class, minicam.energy.input, year) %>%
      summarise(energy_total = sum(energy_input)) %>%
      ungroup()
    L2101.oilgas_shares <- L2101.oilgas_shares_numerator %>%
      left_join(L2101.oilgas_shares_denominator, by = c("region", "resource_class", "minicam.energy.input", "year")) %>%
      mutate(share = if_else(energy_total > 0, energy_input / energy_total, 0)) %>%
      select(region, resource, subresource, resource_class, minicam.energy.input, year, share) %>%
      left_join(fuel_energy_input, by = "minicam.energy.input")

    # Calculate historical coefficients as energy_input * share / cal.production
    L2101.RsrcClassInput <-
      filter(L132.in_EJ_R_rsrcenergy_F_Yh, year %in% MODEL_BASE_YEARS) %>%
      rename(energy_input = value) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(resource_class = sub("industry_", "", sector)) %>%
      select(region, resource_class, fuel, year, energy_input)

    L2101.ResTechCoef_fos_hist <-
      L210.RsrcCalProd %>%
      # gets rid of unconventional oil if not in L2101.oilgas_shares
      filter(subresource %in% L2101.oilgas_shares$subresource) %>%
      left_join(L2101.oilgas_shares, by = c("region", "resource", "subresource", "year")) %>%
      left_join(L2101.RsrcClassInput, by = c("region", "resource_class", "fuel", "year")) %>%
      mutate(coefficient = if_else(cal.production > 0,
                                   round(energy_input * share / cal.production, energy.DIGITS_CALOUTPUT),
                                   0),
             technology = resource,
             market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["ResTechCoef"]])

    L2101.ResTechCoef_fos <- bind_rows(L2101.ResTechCoef_fos_hist,
                                       subset( L2101.ResTechCoef_fos, year %in% MODEL_FUTURE_YEARS)) %>%
      rename(reserve.subresource = subresource, resource.reserve.technology = technology)

    # L2101.ResTechCoefAdj_fos - change the coefs of historical vintages during subsequent years
    L2101.ResTechCoefAdj_fos <- L2101.ResTechCoef_fos_hist %>%
      select(-coefficient) %>%
      rename(reserve.subresource = subresource, resource.reserve.technology = technology) %>%
      repeat_add_columns(tibble(adj_coef_year = MODEL_YEARS)) %>%
      filter(adj_coef_year > year & adj_coef_year - year < 100,
             reserve.subresource != "unconventional oil") %>%
      left_join_error_no_match(L2101.ResTechCoef_fos,
                               by = c("region", "resource", "reserve.subresource", "resource.reserve.technology",
                                      adj_coef_year = "year", "minicam.energy.input")) %>%
      rename(current.coef = coefficient)


    # #=======#=======#=======#=======#=======#=======#=========

    # Produce outputs

    L2101.ResTechCoef_fos %>%
      add_title("Energy consumption per unit resource production") %>%
      add_units("Unitless input/output") %>%
      add_comments("historical values calibrated based on IEA energy balances; future years are default assumptions") %>%
      add_precursors("common/GCAM_region_names", "energy/A_regions", "energy/mappings/fuel_energy_input",
                     "energy/A10.subrsrc_coef", "L132.in_EJ_R_rsrcenergy_F_Yh", "L210.RsrcCalProd" ) ->
      L2101.ResTechCoef_fos

    L2101.ResTechCoefAdj_fos %>%
      add_title("Adjusted input-output coefficients of long-lived vintages of energy resource production") %>%
      add_units("Unitless input/output") %>%
      add_comments("necessary for calibration; without these adjustments the energy demands of resource production would not match the inventories") %>%
      same_precursors_as(L2101.ResTechCoef_fos) ->
      L2101.ResTechCoefAdj_fos

    return_data(L2101.ResTechCoef_fos,
                L2101.ResTechCoefAdj_fos)
  } else {
    stop("Unknown command")
  }
}
