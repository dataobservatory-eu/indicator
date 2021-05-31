library(indicators)
library(eurostat)
library(dplyr)

# BUSINESS DEMOGRAPHY====
# Business demography and high growth enterprise by NACE Rev. 2 and NUTS 3 regions [BD_HGNACE2_R3]
# GBARD by socioeconomic objectives (NABS 2007) (gba_nabsfin07)
# - Economical indicator for structural business statistics [indic_sb]:
# [V11910] Population of active enterprises in t - number;
# [V11920] Births of enterprises in t - number;
# [V11930] Deaths of enterprises in t - number;
# [V11960] High growth enterprises measured in employment (growth by 10% or more) - number;
# - Classification of economic activities - NACE Rev.2 [nace_r2]:
# [B-S_X_K642] Industry, construction and services except insurance activities of holding companies
business_demography <- eurostat::get_eurostat(id = "bd_hgnace2_r3")

#business_demography_raw <- eurostat::get_eurostat(id = "bd_hgnace2_r3") %>%
#  filter (.data$indic_sb %in% c("V11910", "V11920", "V11930", "V11960"),
#          .data$nace_r2 == "B-S_X_K642")

business_demography_raw <- eurostat::get_eurostat(id = "bd_hgnace2_r3") %>%
  filter (.data$indic_sb %in% c("V11920", "V11930"),
          .data$nace_r2 == "B-S_X_K642",
          .data$geo %in% unique(
            business_demography$geo[which(nchar(business_demography$geo) == 2)])
          )

# HIGH-TECH PATENT APPLICATIONS=====
# High-tech patent applications to the EPO by priority year by NUTS 3 regions [PAT_EP_RTEC]
# Unit of measure [unit]:
# [NR] Number; [P_MACT] Per million of active population; [P_MHAB] Per million inhabitants
# International patent classification (IPC) [ipc]: (all)
# [HT] High tech - total; [AVI] Aviation;
# [CAB] Computer and automated business equipment; [CTE] Communication technology
# [LSR] Laser; [MGE] Micro-organism and genetic engineering; [SMC] Semiconductors
# budgets (NABS 2007) [nabs07]: [NABS02] Environment
# [TOTAL] Total government budget allocations for R&D

high_tech_patents <- eurostat::get_eurostat(id = "pat_ep_rtec")

high_tech_patents_raw <- eurostat::get_eurostat(id = "pat_ep_rtec") %>%
  filter (.data$unit %in% c("NR", "P_MHAB"),
          .data$geo %in% unique(high_tech_patents$geo[which(nchar(high_tech_patents$geo) == 2)]))

# PATENT APPLICATIONS====
#Patent applications to the EPO by priority year by NUTS 3 regions, international patent classification (IPC) sections and classes [PAT_EP_RIPC]
# Unit of measure [unit]: [NR] Number; [P_MHAB] Per million inhabitants
# (Not used: [P_MACT] Per million of active population)
# International patent classification (IPC) [ipc]:
# [IPC] International patent classification (IPC) - total
# [A] Section A - Human necessities
# [B] Section B - Performing operations; transporting
# [C] Section C - Chemistry; metallurgy
# [D] Section D - Textiles; paper
# [E] Section E - Fixed constructions
# [F] Section F - Mechanical engineering; lighting; heating; weapons; blasting
# [G] Section G - Physics
# [H] Section H - Electricity


patents_nuts3 <- eurostat::get_eurostat(id = "pat_ep_ripc")

patents_nuts3_raw <- eurostat::get_eurostat(id = "pat_ep_ripc") %>%
  filter (.data$unit %in% c("NR", "P_MHAB"),
          .data$ipc %in% c("IPC", LETTERS[1:8]),
          .data$geo %in% unique(patents_nuts3$geo[which(nchar(patents_nuts3$geo) == 2)]))

# European Union trade mark (EUTM) applications by NUTS 3 regions [IPR_TA_REG]====
# Per million population

eutm_nuts3 <- eurostat::get_eurostat(id = "ipr_ta_popr")

eutm_nuts3_raw <- eurostat::get_eurostat(id = "ipr_ta_popr") %>%
  filter (.data$unit %in% c("P_MHAB"),
          .data$geo %in% unique(eutm_nuts3$geo[which(nchar(eutm_nuts3$geo) == 2)]))

# Community design (CD) applications per million population by NUTS 3 regions (ipr_da_popr) ====
# Per million population

cda_nuts3 <- eurostat::get_eurostat(id = "ipr_da_popr")

cda_nuts3_raw <- eurostat::get_eurostat(id = "ipr_da_popr") %>%
  filter (.data$unit %in% c("P_MHAB"),
          .data$geo %in% unique(cda_nuts3$geo[which(nchar(cda_nuts3$geo) == 2)]))

# GDP NUTS3====
# DOES NOT WORK FOR SOME REASON
# Gross domestic product (GDP) at current market prices by NUTS 3 regions [NAMA_10R_3GDP]
# Unit of measure [unit]: [MIO_EUR] Million euro;  [EUR_HAB] Euro per inhabitant

gdp_nuts3 <- eurostat::get_eurostat(id = "nama_10r_3gdp")

#gdp_nuts3_raw <- eurostat::get_eurostat(id = "nama_10r_3gdp") %>%
#  filter (.data$unit %in% c("MIO_EUR", "EUR_HAB"))

gdp_nuts3_raw <- eurostat::get_eurostat(id = "nama_10r_3gdp") %>%
  filter (.data$unit %in% c("EUR_HAB"),
          .data$geo %in% unique(gdp_nuts3$geo[which(nchar(gdp_nuts3$geo) == 2)]))
# ======================

# business_demography_indicators <- get_eurostat_indicator(
#  preselected_indicators = business_demography_raw,
#  id = "bd_hgnace2_r3")

high_tech_patents_indicators <- get_eurostat_indicator(
  preselected_indicators = high_tech_patents_raw,
  id = "pat_ep_rtec")

patents_nuts3_indicators <- get_eurostat_indicator(
  preselected_indicators = patents_nuts3_raw,
  id = "pat_ep_ripc")

#eutm_nuts3_indicators <- get_eurostat_indicator(
#  preselected_indicators = eutm_nuts3_raw,
#  id = "ipr_ta_popr")

#cda_nuts3_indicators <- get_eurostat_indicator(
#  preselected_indicators = cda_nuts3_raw,
#  id = "ipr_da_popr")


#gdp_nuts3_indicators <- get_eurostat_indicator(
 # preselected_indicators = gdp_nuts3_raw,
  #id = "nama_10r_3gdp")


edo_indicators_to_impute <- high_tech_patents_indicators$indicator %>%
  bind_rows ( patents_nuts3_indicators$indicator )

edo_metadata_to_update <- high_tech_patents_indicators$metadata %>%
  bind_rows ( patents_nuts3_indicators$metadata )

edo_labelling_bind <- high_tech_patents_indicators$labelling %>%
  bind_rows ( patents_nuts3_indicators$labelling )

edo_imp <- impute_indicators ( indic = edo_indicators_to_impute %>%
                             filter ( time > as.Date("1990-01-01")))

edo_updated_metadata <- update_metadata(
  edo_imp,
  metadata = edo_metadata_to_update )

edo_updated_metadata %>%
  select ( all_of ( c("indicator_code", "actual", "missing",
                      "nocb", "locf", "approximate", "forecast")))

edo_keywords <- add_keywords (description_table = high_tech_patents_indicators$descriptions,
                          keywords = list( "economy", "ipr", "supply", "rd")) %>%
  bind_rows ( add_keywords (patents_nuts3_indicators$descriptions,
                            list( "economy", "ipr", "supply", "rd")) )

edo_save_path <- ifelse ( dir.exists("data-raw"),
                              yes = file.path("data-raw", "economy.db"),
                              no = file.path("..", "data-raw", "economy.db"))

create_database (indicator_tables = edo_imp,
                 metadata_tables = edo_updated_metadata,
                 labelling_table = edo_labelling_bind,
                 description_table = edo_keywords,
                 db_path = edo_save_path)

file.exists(edo_save_path)
