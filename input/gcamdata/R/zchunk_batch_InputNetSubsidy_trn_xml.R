# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_InputNetSubsidy_trn_xml
#'
#' Construct XML data structure for \code{InputNetSubsidy.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{InputNetSubsidy.xml}.
module_energy_batch_InputNetSubsidy_trn_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.StubTranTechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "InputNetSubsidy.xml"))
  } else if(command == driver.MAKE) {

    ## silence package check.
    all_data <- list(...)[[1]]

    # Load required inputs
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")

    L254.StubTranTechCoef %>%
      filter(minicam.energy.input %in% c("refined liquids enduse", "refined liquids gasoline pool"),
             year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = minicam.energy.input) %>%
      mutate(input.net.subsidy = if_else(sector.name == "refined liquids enduse",
                                         "Biodiesel-Net-Subsidy", "Ethanol-Net-Subsidy")) %>%
      select(LEVEL2_DATA_NAMES[["StubTranTechInputNetSubsidy"]]) ->
      L254.StubTranTechInputNetSubsidy

    # #=======#=======#=======#=======#=======#=======#=========

    # Produce outputs
    create_xml("InputNetSubsidy.xml") %>%
      add_xml_data(L254.StubTranTechInputNetSubsidy, "StubTranTechInputNetSubsidy") %>%
      add_precursors("L254.StubTranTechCoef") ->
      InputNetSubsidy.xml

    return_data(InputNetSubsidy.xml)
  } else {
    stop("Unknown command")
  }
}
