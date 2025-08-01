###---------Script to generate agroforestry xmls---------------------

##This script generates xmls to implement agroforestry across basins in GCAM. The current script works with GCAM 7.1
## but can be used with any version of GCAM. User will need to install gcamdata from that GCAM version, initiate renv() and
## build driver_drake(). Ideally save this file to the gcamdata root folder.

devtools::load_all(".")


## Scalars for yield and carbon
carbon_scalar <-5
yield_scalar <- 0.75

## Part 1 : Generate land xmls
L2252.LN5_Logit <- as.data.frame(load_from_cache(c("L2252.LN5_Logit")))
colnames(L2252.LN5_Logit)<- gsub("L2252.LN5_Logit.","",colnames(L2252.LN5_Logit))

L2252.LN5_HistMgdAllocation_crop <- as.data.frame(load_from_cache(c("L2252.LN5_HistMgdAllocation_crop")))
colnames(L2252.LN5_HistMgdAllocation_crop)<- gsub("L2252.LN5_HistMgdAllocation_crop.","",colnames(L2252.LN5_HistMgdAllocation_crop))
L2252.LN5_HistMgdAllocation_crop %>%
  mutate(LandLeaf=paste0(LandLeaf,"_AgroForest")) %>%
  mutate(allocation=0)->L2252.LN5_HistMgdAllocation_crop

L2252.LN5_HistMgdAllocation_crop <- as_tibble(L2252.LN5_HistMgdAllocation_crop)

#These are the model base years
years <- c(1975,1990,2005,2010,2015)

L2252.LN5_HistMgdAllocation_crop %>%
  select(-year,-allocation) %>%
  distinct() %>%
  repeat_add_columns(tibble(year=years)) %>%
  mutate(allocation=0)->L2252.LN5_MgdAllocation_crop



# L2252.LN5_MgdAllocation_crop <- as.data.frame(load_from_cache(c("L2252.LN5_MgdAllocation_crop")))
# colnames(L2252.LN5_MgdAllocation_crop)<- gsub("L2252.LN5_MgdAllocation_crop.","",colnames(L2252.LN5_MgdAllocation_crop))
# L2252.LN5_MgdAllocation_crop %>%
#   mutate(LandLeaf=paste0(LandLeaf,"_AgroForest")) %>%
#   mutate(allocation=0)->L2252.LN5_MgdAllocation_crop



L2231.LN3_MgdCarbon_noncrop <- as.data.frame(load_from_cache(c("L2231.LN3_MgdCarbon_noncrop")))
colnames(L2231.LN3_MgdCarbon_noncrop)<- gsub("L2231.LN3_MgdCarbon_noncrop.","",colnames(L2231.LN3_MgdCarbon_noncrop))

L2231.LN3_MgdCarbon_noncrop %>%
  filter(grepl("Forest",LandLeaf)) %>%
  select(-LandNode2,-LandNode3,-LandLeaf) %>%
  group_by(region,LandAllocatorRoot,LandNode1) %>%
  summarize(hist.veg.carbon.density=mean(hist.veg.carbon.density),
            hist.soil.carbon.density=mean(hist.soil.carbon.density),
            veg.carbon.density=mean(veg.carbon.density),
            soil.carbon.density=mean(soil.carbon.density),
            mature.age.year.fillout=mean(mature.age.year.fillout),
            mature.age=mean(mature.age))->L2231.LN3_MgdCarbon_noncrop

L2252.LN5_MgdCarbon_crop <- as.data.frame(load_from_cache(c("L2252.LN5_MgdCarbon_crop")))
colnames(L2252.LN5_MgdCarbon_crop)<- gsub("L2252.LN5_MgdCarbon_crop.","",colnames(L2252.LN5_MgdCarbon_crop))

L2252.LN5_MgdCarbon_crop %>%
  mutate(LandLeaf=paste0(LandLeaf,"_AgroForest")) %>%
  select(-hist.veg.carbon.density,-hist.soil.carbon.density,
         -veg.carbon.density,-soil.carbon.density,-mature.age.year.fillout,
         -mature.age) %>%
  left_join(L2231.LN3_MgdCarbon_noncrop) %>%
  mutate(veg.carbon.density=veg.carbon.density*yield_scalar,
         soil.carbon.density=soil.carbon.density*yield_scalar)->L2252.LN5_MgdCarbon_crop

L2252.LN5_MgdCarbon_crop[is.na(L2252.LN5_MgdCarbon_crop)] <- 0

L2252.LN5_MgdAllocation_crop %>%
  select(-year,-allocation) %>%
  distinct() %>%
  mutate(year=2020,
         ghost.unnormalized.share=0.05)->L2252.LN5_LeafGhostShare



create_xml("land_input_5_IRR_MGMT_agroforest.xml") %>%
  add_logit_tables_xml(L2252.LN5_Logit, "LN5_Logit") %>%
  add_xml_data(L2252.LN5_HistMgdAllocation_crop, "LN5_HistMgdAllocation") %>%
  add_xml_data(L2252.LN5_MgdAllocation_crop, "LN5_MgdAllocation") %>%
  add_xml_data(L2252.LN5_MgdCarbon_crop, "LN5_MgdCarbon") %>%
  add_xml_data(L2252.LN5_LeafGhostShare, "LN5_LeafGhostShare") %>%
  add_rename_landnode_xml() %>%
  gcamdata::run_xml_conversion()



years <- c(2025,2030,2035,2040,2045,2050,2060,2070,2080,2090,2100)

L2252.LN5_MgdAllocation_crop <- as_tibble(L2252.LN5_MgdAllocation_crop)

#The policy weights determine how aggressive user wants the agroforestry implementation
L2252.LN5_MgdAllocation_crop %>%
  select(-year,-allocation) %>%
  distinct() %>%
  repeat_add_columns(tibble(year=years)) %>%
  left_join(read.csv("policy_weights.csv"))->L2252.LN5_LeafGhostShare


create_xml("land_input_5_IRR_MGMT_agroforest_policy.xml") %>%
  add_logit_tables_xml(L2252.LN5_Logit, "LN5_Logit") %>%
  add_xml_data(L2252.LN5_HistMgdAllocation_crop, "LN5_HistMgdAllocation") %>%
  add_xml_data(L2252.LN5_MgdAllocation_crop, "LN5_MgdAllocation") %>%
  add_xml_data(L2252.LN5_MgdCarbon_crop, "LN5_MgdCarbon") %>%
  add_xml_data(L2252.LN5_LeafGhostShare, "LN5_LeafGhostShare") %>%
  add_rename_landnode_xml() %>%
  gcamdata::run_xml_conversion()

#Part 2: Create ag production xml


L2012.AgSupplySector <- as.data.frame(load_from_cache(c("L2012.AgSupplySector")))
colnames(L2012.AgSupplySector)<- gsub("L2012.AgSupplySector.","",colnames(L2012.AgSupplySector))



L2012.AgSupplySubsector <- as.data.frame(load_from_cache(c("L2012.AgSupplySubsector")))
colnames(L2012.AgSupplySubsector)<- gsub("L2012.AgSupplySubsector.","",colnames(L2012.AgSupplySubsector))



L2012.AgProduction_ag_irr_mgmt <- as.data.frame(load_from_cache(c("L2012.AgProduction_ag_irr_mgmt")))
colnames(L2012.AgProduction_ag_irr_mgmt)<- gsub("L2012.AgProduction_ag_irr_mgmt.","",colnames(L2012.AgProduction_ag_irr_mgmt))

L2012.AgProduction_ag_irr_mgmt %>%
  group_by(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology) %>%
  mutate(sumcalout=sum(calOutputValue)) %>%
  ungroup() %>%
  filter(sumcalout >0) %>%
  select(-sumcalout) %>%
  mutate(AgProductionTechnology=paste0(AgProductionTechnology,"_AgroForest"),
         calOutputValue=0,
         tech.share.weight=0)->L2012.AgProduction_ag_irr_mgmt





L2012.AgProduction_ag_irr_mgmt_old <- as.data.frame(load_from_cache(c("L2012.AgProduction_ag_irr_mgmt")))
colnames(L2012.AgProduction_ag_irr_mgmt_old)<- gsub("L2012.AgProduction_ag_irr_mgmt.","",colnames(L2012.AgProduction_ag_irr_mgmt_old))

L2252.LN5_MgdAllocation_crop_old <- as.data.frame(load_from_cache(c("L2252.LN5_MgdAllocation_crop")))
colnames(L2252.LN5_MgdAllocation_crop_old)<- gsub("L2252.LN5_MgdAllocation_crop.","",colnames(L2252.LN5_MgdAllocation_crop_old))

L2012.AgProduction_ag_irr_mgmt_old %>%
  left_join(L2252.LN5_MgdAllocation_crop_old, by = c("AgSupplySubsector" = "LandNode4", "AgProductionTechnology" = "LandLeaf","region", "year")) %>%
  mutate(yield = calOutputValue / allocation) %>%
  na.omit() %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,yield)->old_yields

#Note that zero yield basins are dropped here
L2012.AgProduction_ag_irr_mgmt %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year) %>%
  left_join(old_yields %>% mutate(AgProductionTechnology=paste0(AgProductionTechnology,"_AgroForest"))) %>%
  na.omit() %>%
  mutate(yield=if_else(is.na(yield),0,yield),
         yield=yield*yield_scalar)->L2012.AgYield_agroforest



create_xml("ag_For_Past_bio_base_IRR_MGMT_agroforest.xml") %>%
  add_logit_tables_xml(L2012.AgSupplySector, "AgSupplySector") %>%
  add_logit_tables_xml(L2012.AgSupplySubsector, "AgSupplySubsector") %>%
  add_xml_data(L2012.AgProduction_ag_irr_mgmt, "AgProduction") %>%
  add_xml_data(L2012.AgYield_agroforest, "AgYield") %>%
  gcamdata::run_xml_conversion()


#Part 3 : ag_prod_change_xmls


L2052.AgProdChange_ag_irr_ref <- as.data.frame(load_from_cache(c("L2052.AgProdChange_ag_irr_ref")))
colnames(L2052.AgProdChange_ag_irr_ref)<- gsub("L2052.AgProdChange_ag_irr_ref.","",colnames(L2052.AgProdChange_ag_irr_ref))

L2052.AgProdChange_ag_irr_ref %>%
  mutate(AgProductionTechnology=paste0(AgProductionTechnology,"_AgroForest"))->L2052.AgProdChange_ag_irr_ref

#We need to keep consistent with basins that we dropped
L2052.AgProdChange_ag_irr_ref %>%
  left_join(L2012.AgYield_agroforest %>% mutate(yield=1) %>% select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,yield)) %>%
  distinct() %>%
  na.omit() %>%
  select(-yield)->L2052.AgProdChange_ag_irr_ref

create_xml("ag_prodchange_agroforest.xml") %>%
  add_xml_data(L2052.AgProdChange_ag_irr_ref, "AgProdChange") %>%
  gcamdata::run_xml_conversion()
