# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_batch_res_policies_xml
#'
#' Construct XML data structure for \code{aviationbio_cons_mandate_res.xml}, \code{marinebio_cons_mandate_res_24.xml},
#' \code{marinebio_cons_mandate_res.xml}, and \code{roadbio_cons_mandate_res.xml}
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated XML outputs including \code{aviationbio_cons_mandate_res.xml}, \code{marinebio_cons_mandate_res_24.xml},
#' \code{marinebio_cons_mandate_res.xml}, and \code{roadbio_cons_mandate_res.xml}
module_energy_batch_res_policies_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "energy/A26.res_policies"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "aviationbio_cons_mandate_res.xml",
             XML = "marinebio_cons_mandate_res_24.xml",
             XML = "marinebio_cons_mandate_res.xml",
             XML = "roadbio_cons_mandate_res.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A26.res_policies <- get_data(all_data, "energy/A26.res_policies")

    # Process data
    StubTechRESSecOut_trn <- filter(A26.res_policies, !is.na(res.secondary.output)) %>%
      gather_years() %>%
      rename(stub.technology = technology) %>%
      select(LEVEL2_DATA_NAMES[["StubTechRESSecOut"]])

    StubTechCoef_trn <- filter(A26.res_policies, !is.na(minicam.energy.input)) %>%
      select(-res.secondary.output, -output.ratio) %>%
      gather_years(value_col = "coefficient") %>%
      rename(stub.technology = technology) %>%
      mutate(market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["StubTechCoef"]])

    PortfolioStdConstraint_trn <- StubTechCoef_trn %>%
      select(region, policy.portfolio.standard = minicam.energy.input, year) %>%
      mutate(market = region,
             policyType = "RES",
             constraint = 1,
             min.price = 0)

    PortfolioStdConstraint_shipbiodiesel <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "biodiesel consumption constraint") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]])
    PortfolioStdMinPrice_shipbiodiesel <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "biodiesel consumption constraint") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdMinPrice"]])
    StubTechRESSecOut_shipbiodiesel <- filter(StubTechRESSecOut_trn, res.secondary.output == "biodiesel consumption constraint")
    StubTechCoef_shipbiodiesel <- filter(StubTechCoef_trn, minicam.energy.input == "biodiesel consumption constraint")

    PortfolioStdConstraint_shipbiofuels <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "marine biofuel consumption credit") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]])
    PortfolioStdMinPrice_shipbiofuels <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "marine biofuel consumption credit") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdMinPrice"]])
    StubTechRESSecOut_shipbiofuels <- filter(StubTechRESSecOut_trn, res.secondary.output == "marine biofuel consumption credit")
    StubTechCoef_shipbiofuels <- filter(StubTechCoef_trn, minicam.energy.input == "marine biofuel consumption credit")

    PortfolioStdConstraint_aviation <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "biojet consumption credit") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]])
    PortfolioStdMinPrice_aviation <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "biojet consumption credit") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdMinPrice"]])
    StubTechRESSecOut_aviation <- filter(StubTechRESSecOut_trn, res.secondary.output == "biojet consumption credit")
    StubTechCoef_aviation <- filter(StubTechCoef_trn, minicam.energy.input == "biojet consumption credit")

    PortfolioStdConstraint_road <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "road biofuel consumption credit") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]])
    PortfolioStdMinPrice_road <- filter(PortfolioStdConstraint_trn, policy.portfolio.standard == "road biofuel consumption credit") %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdMinPrice"]])
    StubTechRESSecOut_road <- filter(StubTechRESSecOut_trn, res.secondary.output == "road biofuel consumption credit")
    StubTechCoef_road <- filter(StubTechCoef_trn, minicam.energy.input == "road biofuel consumption credit")

    # Produce XML outputs
    create_xml("marinebio_cons_mandate_res_24.xml") %>%
      add_xml_data(PortfolioStdConstraint_shipbiodiesel, "PortfolioStdConstraint") %>%
      add_xml_data(PortfolioStdMinPrice_shipbiodiesel, "PortfolioStdMinPrice") %>%
      add_xml_data(StubTechRESSecOut_shipbiodiesel, "StubTechRESSecOut") %>%
      add_xml_data(StubTechCoef_shipbiodiesel, "StubTechCoef") %>%
      add_precursors("energy/A26.res_policies") ->
      marinebio_cons_mandate_res_24.xml

    create_xml("marinebio_cons_mandate_res.xml") %>%
      add_xml_data(PortfolioStdConstraint_shipbiofuels, "PortfolioStdConstraint") %>%
      add_xml_data(PortfolioStdMinPrice_shipbiofuels, "PortfolioStdMinPrice") %>%
      add_xml_data(StubTechRESSecOut_shipbiofuels, "StubTechRESSecOut") %>%
      add_xml_data(StubTechCoef_shipbiofuels, "StubTechCoef") %>%
      add_precursors("energy/A26.res_policies") ->
      marinebio_cons_mandate_res.xml

    create_xml("aviationbio_cons_mandate_res.xml") %>%
      add_xml_data(PortfolioStdConstraint_aviation, "PortfolioStdConstraint") %>%
      add_xml_data(PortfolioStdMinPrice_aviation, "PortfolioStdMinPrice") %>%
      add_xml_data(StubTechRESSecOut_aviation, "StubTechRESSecOut") %>%
      add_xml_data(StubTechCoef_aviation, "StubTechCoef") %>%
      add_precursors("energy/A26.res_policies") ->
      aviationbio_cons_mandate_res.xml

    create_xml("roadbio_cons_mandate_res.xml") %>%
      add_xml_data(PortfolioStdConstraint_road, "PortfolioStdConstraint") %>%
      add_xml_data(PortfolioStdMinPrice_road, "PortfolioStdMinPrice") %>%
      add_xml_data(StubTechRESSecOut_road, "StubTechRESSecOut") %>%
      add_xml_data(StubTechCoef_road, "StubTechCoef") %>%
      add_precursors("energy/A26.res_policies") ->
      roadbio_cons_mandate_res.xml

    return_data(marinebio_cons_mandate_res_24.xml,
                marinebio_cons_mandate_res.xml,
                aviationbio_cons_mandate_res.xml,
                roadbio_cons_mandate_res.xml)
  } else {
    stop("Unknown command")
  }
}
