#' module_energy_batch_refined_liquids_trade_xml
#'
#' Construct XML data structure for \code{refined_liquids_trade.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{refined_liquids_trade.xml}.
module_energy_batch_refined_liquids_trade_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L2262.Supplysector_refinedLiquids_tra",
             "L2262.SectorUseTrialMarket_refinedLiquids_tra",
             "L2262.SubsectorAll_refinedLiquids_tra",
             "L2262.TechShrwt_refinedLiquids_tra",
             "L2262.TechCost_refinedLiquids_tra",
             "L2262.TechCoef_refinedLiquids_tra",
             "L2262.Production_refinedLiquids_tra",
             "L2262.Supplysector_refinedLiquids_reg",
             "L2262.SubsectorAll_refinedLiquids_reg",
             "L2262.TechShrwt_refinedLiquids_reg",
             "L2262.TechCoef_refinedLiquids_reg",
             "L2262.Production_refinedLiquids_reg_imp",
             "L2262.Production_refinedLiquids_reg_dom"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "refined_liquids_trade.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L2262.Supplysector_refinedLiquids_tra <- get_data(all_data, "L2262.Supplysector_refinedLiquids_tra")
    L2262.SectorUseTrialMarket_refinedLiquids_tra <- get_data(all_data, "L2262.SectorUseTrialMarket_refinedLiquids_tra")
    L2262.SubsectorAll_refinedLiquids_tra <- get_data(all_data, "L2262.SubsectorAll_refinedLiquids_tra")
    L2262.TechShrwt_refinedLiquids_tra <- get_data(all_data, "L2262.TechShrwt_refinedLiquids_tra")
    L2262.TechCost_refinedLiquids_tra <- get_data(all_data, "L2262.TechCost_refinedLiquids_tra")
    L2262.TechCoef_refinedLiquids_tra <- get_data(all_data, "L2262.TechCoef_refinedLiquids_tra")
    L2262.Production_refinedLiquids_tra <- get_data(all_data, "L2262.Production_refinedLiquids_tra")
    L2262.Supplysector_refinedLiquids_reg <- get_data(all_data, "L2262.Supplysector_refinedLiquids_reg")
    L2262.SubsectorAll_refinedLiquids_reg <- get_data(all_data, "L2262.SubsectorAll_refinedLiquids_reg")
    L2262.TechShrwt_refinedLiquids_reg <- get_data(all_data, "L2262.TechShrwt_refinedLiquids_reg")
    L2262.TechCoef_refinedLiquids_reg <- get_data(all_data, "L2262.TechCoef_refinedLiquids_reg")
    L2262.Production_refinedLiquids_reg_imp <- get_data(all_data, "L2262.Production_refinedLiquids_reg_imp")
    L2262.Production_refinedLiquids_reg_dom <- get_data(all_data, "L2262.Production_refinedLiquids_reg_dom")

    # #=======#=======#=======#=======#=======#=======#=========

    # Produce outputs
    create_xml("refined_liquids_trade.xml") %>%
      add_logit_tables_xml(L2262.Supplysector_refinedLiquids_tra, "Supplysector") %>%
      add_xml_data(L2262.SectorUseTrialMarket_refinedLiquids_tra, "SectorUseTrialMarket") %>%
      add_logit_tables_xml(L2262.SubsectorAll_refinedLiquids_tra, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2262.TechShrwt_refinedLiquids_tra, "TechShrwt") %>%
      add_xml_data(L2262.TechCost_refinedLiquids_tra, "TechCost") %>%
      add_xml_data(L2262.TechCoef_refinedLiquids_tra, "TechCoef") %>%
      add_xml_data(L2262.Production_refinedLiquids_tra, "Production") %>%
      add_logit_tables_xml(L2262.Supplysector_refinedLiquids_reg, "Supplysector") %>%
      add_logit_tables_xml(L2262.SubsectorAll_refinedLiquids_reg, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
      add_xml_data(L2262.TechShrwt_refinedLiquids_reg, "TechShrwt") %>%
      add_xml_data(L2262.TechCoef_refinedLiquids_reg, "TechCoef") %>%
      add_xml_data(L2262.Production_refinedLiquids_reg_imp, "Production") %>%
      add_xml_data(L2262.Production_refinedLiquids_reg_dom, "Production") %>%
      add_precursors("L2262.Supplysector_refinedLiquids_tra",
                     "L2262.SectorUseTrialMarket_refinedLiquids_tra",
                     "L2262.SubsectorAll_refinedLiquids_tra",
                     "L2262.TechShrwt_refinedLiquids_tra",
                     "L2262.TechCost_refinedLiquids_tra",
                     "L2262.TechCoef_refinedLiquids_tra",
                     "L2262.Production_refinedLiquids_tra",
                     "L2262.Supplysector_refinedLiquids_reg",
                     "L2262.SubsectorAll_refinedLiquids_reg",
                     "L2262.TechShrwt_refinedLiquids_reg",
                     "L2262.TechCoef_refinedLiquids_reg",
                     "L2262.Production_refinedLiquids_reg_imp",
                     "L2262.Production_refinedLiquids_reg_dom") ->
      refined_liquids_trade.xml

    return_data(refined_liquids_trade.xml)
  } else {
    stop("Unknown command")
  }
}
