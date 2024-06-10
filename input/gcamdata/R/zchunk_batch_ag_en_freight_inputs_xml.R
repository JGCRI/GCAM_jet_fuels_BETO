#' module_energy_batch_ag_en_freight_inputs_xml
#'
#' Construct XML data structure for \code{ag_en_freight_inputs.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{ag_en_freight_inputs.xml}.
module_energy_batch_ag_en_freight_inputs_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L271.StubTechCoef_freight_FoodDemand",
             "L271.StubTechCoef_freight_NonFoodDemand",
             "L271.TechCoef_freight_en_ag_other",
             "L271.StubTechInputPMult_freight_FoodDemand",
             "L271.StubTechInputPMult_freight_NonFoodDemand",
             "L271.TechInputPMult_freight_en_ag_other",
             "L271.TechCost_freight",
             "L271.BaseService_freightNetEnAg",
             "L222.biofuel_type_filter_R"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "ag_en_freight_inputs.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L271.StubTechCoef_freight_FoodDemand <- get_data(all_data, "L271.StubTechCoef_freight_FoodDemand")
    L271.StubTechCoef_freight_NonFoodDemand <- get_data(all_data, "L271.StubTechCoef_freight_NonFoodDemand")
    L271.TechCoef_freight_en_ag_other <- get_data(all_data, "L271.TechCoef_freight_en_ag_other")
    L271.StubTechInputPMult_freight_FoodDemand <- get_data(all_data, "L271.StubTechInputPMult_freight_FoodDemand")
    L271.StubTechInputPMult_freight_NonFoodDemand <- get_data(all_data, "L271.StubTechInputPMult_freight_NonFoodDemand")
    L271.TechInputPMult_freight_en_ag_other <- get_data(all_data, "L271.TechInputPMult_freight_en_ag_other")
    L271.TechCost_freight <- get_data(all_data, "L271.TechCost_freight")
    L271.BaseService_freightNetEnAg <- get_data(all_data, "L271.BaseService_freightNetEnAg")

    L222.biofuel_type_filter_R <- get_data(all_data, "L222.biofuel_type_filter_R")

    # TRW 5/8/24: filter out biojet technologies that are not produced domestically
    # since we are turning off biojet trade

    # first make a filter for the biojet technologies
    biofuel_type_filter_reg_biojet <- L222.biofuel_type_filter_R %>%
      filter(grepl("HEFA|ETJ", supplysector),
             grepl("joint", supplysector)) %>%
      mutate(technology = gsub(" joint", "", supplysector),
             supplysector = "aviation fuels",
             subsector = if_else(grepl("HEFA", technology), "HEFA", "ETJ"))

    # then add in the corn ETJ renewable diesel co-product (it has a different
    # name than the biojet ETJ technology)
    biofuel_type_filter_reg_biojet_all <- biofuel_type_filter_reg_biojet %>%
      rbind(biofuel_type_filter_reg_biojet %>%
              filter(technology == "corn ETJ") %>%
              mutate(technology = "corn ethanol renewable diesel"))

    L271.TechCoef_freight_en_ag_other <- anti_join(L271.TechCoef_freight_en_ag_other,
                                                   biofuel_type_filter_reg_biojet,
                                                   by = c("region", "supplysector", "technology"))

    L271.TechInputPMult_freight_en_ag_other <- anti_join(L271.TechInputPMult_freight_en_ag_other,
                                                         biofuel_type_filter_reg_biojet_all,
                                                         by = c("region", "supplysector", "technology"))

    # #=======#=======#=======#=======#=======#=======#=========

    # Produce outputs
    create_xml("ag_en_freight_inputs.xml") %>%
      add_xml_data_generate_levels(L271.StubTechCoef_freight_FoodDemand, "StubTechCoef", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data_generate_levels(L271.StubTechInputPMult_freight_FoodDemand, "StubTechInputPmult", "subsector","nesting-subsector",1,FALSE) %>%
      add_xml_data(L271.StubTechCoef_freight_NonFoodDemand, "StubTechCoef") %>%
      add_xml_data(L271.StubTechInputPMult_freight_NonFoodDemand, "StubTechInputPmult") %>%
      add_xml_data(L271.TechCoef_freight_en_ag_other, "TechCoef") %>%
      add_xml_data(L271.TechInputPMult_freight_en_ag_other, "TechInputPMult") %>%
      add_xml_data(L271.TechCost_freight, "TechCost") %>%
      add_xml_data(L271.BaseService_freightNetEnAg, "BaseService") %>%
      add_precursors("L271.StubTechCoef_freight_FoodDemand",
                     "L271.StubTechCoef_freight_NonFoodDemand",
                     "L271.TechCoef_freight_en_ag_other",
                     "L271.StubTechInputPMult_freight_FoodDemand",
                     "L271.StubTechInputPMult_freight_NonFoodDemand",
                     "L271.TechInputPMult_freight_en_ag_other",
                     "L271.TechCost_freight",
                     "L271.BaseService_freightNetEnAg") ->
      ag_en_freight_inputs.xml

    return_data(ag_en_freight_inputs.xml)
  } else {
    stop("Unknown command")
  }
}
