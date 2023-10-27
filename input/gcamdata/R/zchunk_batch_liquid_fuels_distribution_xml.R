#' module_energy_batch_liquid_fuels_distribution_xml
#'
#' Construct XML data structure for \code{liquid_fuels_distribution.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{liquid_fuels_distribution.xml}.
module_energy_batch_liquid_fuels_distribution_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2261.Supplysector_en",
             "L2261.SubsectorLogit_en",
             "L2261.SubsectorShrwtFllt_en",
             "L2261.SubsectorInterp_en",
             "L2261.StubTech_en",
             "L2261.GlobalTechEff_en",
             "L2261.GlobalTechCost_en",
             "L2261.GlobalTechShrwt_en",
             "L2261.StubTechInterp_en",
             "L2261.StubTechProd_gsln",
             "L2261.StubTechProd_refliqend",
             "L2261.StubTechFixOut_otherBiod"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "liquid_fuels_distribution.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2261.Supplysector_en <- get_data(all_data, "L2261.Supplysector_en")
    L2261.SubsectorLogit_en <- get_data(all_data, "L2261.SubsectorLogit_en")
    L2261.SubsectorShrwtFllt_en <- get_data(all_data, "L2261.SubsectorShrwtFllt_en")
    L2261.SubsectorInterp_en <- get_data(all_data, "L2261.SubsectorInterp_en")
    L2261.StubTech_en <- get_data(all_data, "L2261.StubTech_en")
    L2261.GlobalTechEff_en <- get_data(all_data, "L2261.GlobalTechEff_en")
    L2261.GlobalTechCost_en <- get_data(all_data, "L2261.GlobalTechCost_en")
    L2261.GlobalTechShrwt_en <- get_data(all_data, "L2261.GlobalTechShrwt_en")
    L2261.StubTechInterp_en <- get_data(all_data, "L2261.StubTechInterp_en")
    L2261.StubTechProd_gsln <- get_data(all_data, "L2261.StubTechProd_gsln")
    L2261.StubTechProd_refliqend <- get_data(all_data, "L2261.StubTechProd_refliqend")
    L2261.StubTechFixOut_otherBiod <- get_data(all_data, "L2261.StubTechFixOut_otherBiod")

    # #=======#=======#=======#=======#=======#=======#=========

    # Produce outputs
    create_xml("liquid_fuels_distribution.xml") %>%
      add_logit_tables_xml(L2261.Supplysector_en, "Supplysector") %>%
      add_logit_tables_xml(L2261.SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L2261.SubsectorShrwtFllt_en, "SubsectorShrwtFllt") %>%
      add_xml_data(L2261.SubsectorInterp_en, "SubsectorInterp") %>%
      add_xml_data(L2261.StubTech_en, "StubTech") %>%
      add_xml_data(L2261.GlobalTechEff_en, "GlobalTechEff") %>%
      add_xml_data(L2261.GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L2261.GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L2261.StubTechInterp_en, "StubTechInterp") %>%
      add_xml_data(L2261.StubTechProd_gsln, "StubTechProd") %>%
      add_xml_data(L2261.StubTechProd_refliqend, "StubTechProd") %>%
      add_xml_data(L2261.StubTechFixOut_otherBiod, "StubTechFixOut_otherBiod") %>%
      add_precursors("L2261.Supplysector_en",
                     "L2261.SubsectorLogit_en",
                     "L2261.SubsectorShrwtFllt_en",
                     "L2261.SubsectorInterp_en",
                     "L2261.StubTech_en",
                     "L2261.GlobalTechEff_en",
                     "L2261.GlobalTechCost_en",
                     "L2261.GlobalTechShrwt_en",
                     "L2261.StubTechInterp_en",
                     "L2261.StubTechProd_gsln",
                     "L2261.StubTechProd_refliqend",
                     "L2261.StubTechFixOut_otherBiod") ->
      liquid_fuels_distribution.xml

    return_data(liquid_fuels_distribution.xml)
  } else {
    stop("Unknown command")
  }
}
