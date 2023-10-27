#' module_aglu_LA1061.DDGS_GrossTrade
#'
#' Calculate primary agricultural good and animal product mass balances, by region / commodity / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1061.GrossTrade_Mt_R_C_Y}, \code{L1061.ag_NetExp_Mt_R_C_Y} (aglu level1).
#' @details This chunk processes the bi-lateral trade flow data matrix from FAOSTAT, in order to differentiate trade of
#'   agricultural commodities between GCAM regions.
#' @importFrom dplyr anti_join bind_rows distinct filter group_by inner_join left_join mutate rename select ungroup
#' @importFrom tidyr complete drop_na replace_na spread
#' @author GPK/RC/STW February 2019
module_aglu_LA1061.DDGS_GrossTrade <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "aglu/AGLU_ctry",
             FILE = "aglu/FAO/FAO_ag_items_TRADE",
             "FAO_BilateralTrade",
             "L122.FeedOut_Mt_R_C_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1061.ALL_Mt_R_DDGS_Y"))
  } else if(command == driver.MAKE) {

    year <- value <- Element <- Reporter.Countries <- Partner.Countries <-
      Item.Code <- Item <- item.code <- bitrade_commod <- GCAM_commodity <-
      FAO_country <- iso <- iso.partner <- iso.reporter <- GCAM_region_ID <-
      GCAMreg.partner <- var <- export <- import <- NetExp_Mt <- net_trade <-
      GrossExp_Mt <- Prod_Mt <- GrossImp_Mt <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    AGLU_ctry <- get_data(all_data, "aglu/AGLU_ctry")
    FAO_ag_items_TRADE <- get_data(all_data, "aglu/FAO/FAO_ag_items_TRADE")
    FAO_BilateralTrade <- get_data(all_data, "FAO_BilateralTrade")
    L122.FeedOut_Mt_R_C_Yh <- get_data(all_data, "L122.FeedOut_Mt_R_C_Yh")

    # 1: Filter and prepare the bi-lateral trade flow volume data by country and FAO commodity

    # The bilateral trade dataset is massive (286m data points). The order of the steps below is intended to minimize
    # processing time (i.e., the dataset is filtered before performing operations)

    # Select columns, filter to the import and export quantity variables, and filter to traded GCAM commodities
    # left_join because many of the trade commodities don't map to GCAM commodities (e.g., silk worm cocoons)
    L1061.BiTrade_t_ctry_item <- filter(FAO_BilateralTrade, Element %in% c( "Import Quantity", "Export Quantity"),
                                        # Removing animal trade that has duplicates
                                        !Item %in% c("Buffaloes", "Turkeys", "Camels", "Cattle", "Chickens", "Horses",
                                                     "Pigeons, other birds", "Pigs",  "Sheep",  "Ducks", "Goats", "Mules",
                                                    "Camelids, other", "Rabbits and hares", "Asses", "Rodents, other" )) %>%
      distinct() %>%
      spread(year,value) %>%
      select(Reporter.Countries, Partner.Countries, Item.Code, Item, Element, as.character(aglu.TRADE_CAL_YEARS)) %>%
      left_join(select(FAO_ag_items_TRADE, item.code, bitrade_commod, GCAM_commodity),
                by = c(Item.Code = "item.code")) %>%
      filter(!is.na(GCAM_commodity),
             GCAM_commodity =="DDGS") %>%
      # Join the reporter and partner countries. 10/17/2017 this does produce some missing iso codes for partner
      # countries but they're all tiny. Many  (e.g., "Unspecified Area") do not have 3-digit iso codes anyway. They are
      # dropped by drop_na().
      # Note also - this uses left_join_keep_first_only for countries like the USSR with multiple associated present-day
      # iso codes. We wouldn't want to repeat the trade data by each post-dissolution country, and since none of these
      # actually exist during the time frame for which gross trade is being assessed, there's no benefit to downscaling.
      left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                                by = c( Reporter.Countries = "FAO_country")) %>%
      rename(iso.reporter = iso) %>%
      left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                                by = c( Partner.Countries = "FAO_country")) %>%
      rename(iso.partner = iso) %>%
      drop_na(iso.partner) %>%
      gather_years() %>%
      drop_na(value)

    #2. Re-balancing bilateral trade data
    # The bilateral trade data are not symmetrical - some countries are partner countries but not reporter countries
    # (e.g. Vietnam does not report trading with others, but appears as a partner countries for a number of others). In
    # those missing cases, we use what's already available in the bilateral trade data, and simply flip reporter/partner
    # countries, as well as export/import to get a symmetrical dataset.

    # First, we find those missing cases by comparing the export and import lists
    # The export list - reporter countries, partner countries, commodities, and year
    exp.list <- L1061.BiTrade_t_ctry_item %>%
      filter(Element == "Export Quantity") %>%
      select(Reporter.Countries, Partner.Countries, bitrade_commod, year) %>%
      distinct()
    # The import list - reporter countries, partner countries, commodities, and year
    imp.list <- L1061.BiTrade_t_ctry_item %>%
      filter(Element == "Import Quantity") %>%
      select(Reporter.Countries, Partner.Countries, bitrade_commod, year) %>%
      distinct()

    # The two lists should have the same number of observations -
    # Partner countries in the export list should be reporter countries in the import list, and vice versa

    # Find the partner countries that are missing to report import
    L1061.BiTrade_t_ctry_item_missing_exp <- exp.list %>%
      anti_join(imp.list, by = c("bitrade_commod", "year",
                                 "Reporter.Countries" = "Partner.Countries",
                                 "Partner.Countries" = "Reporter.Countries")) %>%
      mutate(Element = "Export Quantity")
    # Find the partner countries that are missing to report export
    L1061.BiTrade_t_ctry_item_missing_imp <- imp.list %>%
      anti_join(exp.list, by = c("bitrade_commod", "year",
                                 "Reporter.Countries" = "Partner.Countries",
                                 "Partner.Countries" = "Reporter.Countries")) %>%
      mutate(Element = "Import Quantity")

    # Filter those asymetric obersvations in the original data, and flip reporter/partner, export/import
    L1061.BiTrade_t_ctry_item_full <- L1061.BiTrade_t_ctry_item_missing_exp %>%
      bind_rows(L1061.BiTrade_t_ctry_item_missing_imp) %>%
      inner_join(L1061.BiTrade_t_ctry_item,
                 by = c("Reporter.Countries", "Partner.Countries", "Element", "bitrade_commod", "year")) %>%
      mutate(Element = if_else(Element == "Import Quantity", "Export Quantity", "Import Quantity")) %>%
      select(Reporter.Countries = Partner.Countries, Partner.Countries = Reporter.Countries,
             iso.reporter = iso.partner, iso.partner = iso.reporter,
             Item.Code, Item, Element, bitrade_commod, GCAM_commodity, year, value) %>%
      bind_rows(L1061.BiTrade_t_ctry_item)

    # 3. Deriving extra-regional trade flows by GCAM region and traded commodity, filtering out within-region trade

    # Method: filter only the crops considered, join in the reporting region and partner region (from reporting and partner
    # country), filter out where reporting and partner region are the same, convert units, aggregate by GCAM region and
    # commodity, and calculate the net trade, and take an unweighted average of the years.
    # Explanation: In multi-country GCAM regions, within-region trade is excluded.
    iso_mapping_partner <- select(iso_GCAM_regID, iso, GCAM_region_ID) %>%
      rename(iso.partner = iso,
             GCAMreg.partner = GCAM_region_ID)
    L1061.XregTrade_Mt_R_C <- L1061.BiTrade_t_ctry_item_full %>%
      left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                               by = c("iso.reporter" = "iso")) %>%
      left_join_error_no_match(iso_mapping_partner,
                               by = "iso.partner") %>%
      filter(GCAM_region_ID != GCAMreg.partner) %>%
      mutate(value = value * CONV_T_MT,
             var = tolower(sub(" Quantity", "", Element))) %>%
      group_by(GCAM_region_ID, GCAM_commodity, var, year) %>%
      summarise(value = sum(value)) %>%
      group_by(GCAM_region_ID, GCAM_commodity, var, .add = FALSE) %>%
      summarise(value = mean(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(GCAM_region_ID),
               GCAM_commodity = unique(GCAM_commodity),
               var = unique(var)) %>%
      replace_na(list(value = 0)) %>%
      spread(var, value) %>%
      mutate(net_trade = export - import)

    # 4. Deriving scaled gross trade flows by GCAM region and traded commodity

    #DDGS data is not in the SUA database, so we will use scaled FAO bi-lateral trade data and join the data frames
    # In order to re-set export vols to the max of the production, pull in the production quantities
    L1061.Prod_Mt_DDGS <- filter(L122.FeedOut_Mt_R_C_Yh, year == aglu.TRADE_FINAL_BASE_YEAR,
                                 fractional.secondary.output == "DDGS") %>%
      rename(Prod_Mt = value) %>%
      select(GCAM_region_ID, GCAM_commodity = fractional.secondary.output, year, Prod_Mt) %>%
      complete(nesting(GCAM_region_ID = sort(unique(iso_GCAM_regID$GCAM_region_ID))),
               GCAM_commodity, year) %>%
      replace_na(list(Prod_Mt = 0))
    DDG.trade<-L1061.XregTrade_Mt_R_C %>%
      filter(GCAM_commodity=='DDGS') %>%
      left_join_error_no_match(L1061.Prod_Mt_DDGS, by = c("GCAM_region_ID", "GCAM_commodity")) %>%
      mutate(export.adap= if_else(Prod_Mt > export, export, Prod_Mt),
             NetExp_Mt=net_trade,
             NetExp_Mt.adap=export.adap-import,
             Supply_Mt=Prod_Mt-NetExp_Mt,
             Supply_Mt.adap=Prod_Mt-NetExp_Mt.adap,
             import.adap=import*(sum(export.adap)/sum(import))) %>%

      #select the adapted values
      select("GCAM_region_ID", "GCAM_commodity","export.adap","import.adap", "net_trade" ,"year", "Prod_Mt","NetExp_Mt.adap","Supply_Mt.adap") %>%
      rename(export = export.adap,
             import = import.adap,
             NetExp_Mt = NetExp_Mt.adap,
             Supply_Mt = Supply_Mt.adap ) %>%
      #mutate again for making DDG.trade consistent with the whole dataframe
      mutate(net_trade=export-import,
             GrossExp_Mt=export,
             GrossImp_Mt=import,
             NetExp_Mt=net_trade)
    #Feed_Mt=Supply_Mt

    #Validation of the calculations
    tot.exp<-sum(DDG.trade$GrossExp_Mt)
    tot.imp<-sum(DDG.trade$GrossImp_Mt)
    tot.prod<-sum(DDG.trade$Prod_Mt)
    tot.dom.supply<-sum(DDG.trade$Supply_Mt)
    tot.trade<-sum(DDG.trade$NetExp_Mt)

    #check balance imports and exports
    if(tot.exp-tot.imp < 1E-6){
      print('OK! Exports and imports are identical')
    } else {
      print('Warning! Something wrong')
    }

    #trade balance
    if(tot.trade < 1e-10){
      print('OK! Sum of the net trade (exp) is 0')
    } else {
      print('Warning! Something wrong')
    }

    #Production and domestic supplies
    if(tot.prod-tot.dom.supply< 1e-10){
      print('OK! Total production matches with the sum of domestic supplies')
    } else {
      print('Warning! Something wrong')
    }

    # Fill out all historical years, using zeroes for all years other than the aglu trade year
    L1061.GrossTrade_Mt_R_DDGS_Y <- select(DDG.trade,
                                           GCAM_region_ID, GCAM_commodity, year,NetExp_Mt, GrossExp_Mt, GrossImp_Mt) %>%
      complete(nesting(year = HISTORICAL_YEARS), GCAM_region_ID, GCAM_commodity) %>%
      replace_na(list(NetExp_Mt = 0, GrossExp_Mt = 0, GrossImp_Mt = 0))

    # To build the full mass balance, start with the production data
    L1061.ALL_Mt_R_DDGS_Y <- filter(L122.FeedOut_Mt_R_C_Yh,
                                    fractional.secondary.output == "DDGS") %>%
      rename(Prod_Mt = value) %>%
      select(GCAM_region_ID, GCAM_commodity = fractional.secondary.output, year, Prod_Mt) %>%
      complete(nesting(GCAM_region_ID = sort(unique(L1061.XregTrade_Mt_R_C$GCAM_region_ID))),
               GCAM_commodity, year) %>%
      replace_na(list(Prod_Mt = 0)) %>%
      left_join_error_no_match(L1061.GrossTrade_Mt_R_DDGS_Y, by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
      mutate(Supply_Mt = Prod_Mt - NetExp_Mt,
             Food_Mt = 0,
             Feed_Mt = Supply_Mt,
             Biofuels = 0,
             OtherUses_Mt = 0)

    # Produce outputs
    L1061.ALL_Mt_R_DDGS_Y %>%
      add_title("Mass balances (including gross trade) of DDGS, by region / commodity / year.") %>%
      add_units("Mt") %>%
      add_comments("Trade determined from bi-lateral trade flows; only includes trade between countries in different GCAM regions") %>%
      add_comments("All but the specified gross trade year are set to zero") %>%
      add_comments("Production is equal to the secondary output from ethanol production in the energy module") %>%
      add_precursors("common/iso_GCAM_regID",
                     "aglu/AGLU_ctry",
                     "aglu/FAO/FAO_ag_items_TRADE",
                     "FAO_BilateralTrade",
                     "L122.FeedOut_Mt_R_C_Yh") ->
      L1061.ALL_Mt_R_DDGS_Y

    return_data(L1061.ALL_Mt_R_DDGS_Y)
  } else {
    stop("Unknown command")
  }
}
