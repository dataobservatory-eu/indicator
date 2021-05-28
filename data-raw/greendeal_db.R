library(indicators)
library(eurostat)
library(dplyr)

# Government budget allocations for R&D (GBARD) (gba)
# GBARD by socioeconomic objectives (NABS 2007) (gba_nabsfin07)
# Unit of measure: [EUR_HAB] Euro per inhabitant
# Nomenclature for the analysis and comparison of scientific programmes and
# budgets (NABS 2007) [nabs07]: NABS02 Environment
gbard_expenditure <- eurostat::get_eurostat(id = "gba_nabsfin07")

gbard_expenditure_raw <- eurostat::get_eurostat(id = "gba_nabsfin07") %>%
  filter (.data$nabs07 %in% c("NABS02", "TOTAL"),
          .data$unit == "EUR_HAB")

# Gross domestic expenditure on R&D (GERD) at national and regional level (rd_e)
# GERD by sector of performance and socioeconomic objectives (NABS 2007)
# [RD_E_GERDSOBJ07]
# Unit of measure: [EUR_HAB] Euro per inhabitant
# Nomenclature for the analysis and comparison of scientific programmes and
# budgets (NABS 2007) [nabs07]: [NABS02] Environment
# [TOTAL] Total government budget allocations for R&D

gerd_expenditure <- eurostat::get_eurostat(id = "rd_e_gerdsobj07")

gerd_expenditure_raw <- eurostat::get_eurostat(id = "rd_e_gerdsobj07") %>%
  filter (.data$nabs07 %in% c("NABS02", "TOTAL"),
          .data$unit == "EUR_HAB")

# Climate change: Mitigation
# Production, value added and exports in the environmental goods and
# services sector (env_ac_egss2)
# Classification of economic activities - NACE Rev.2 [nace_r2]:
# [TOTAL] Total - all NACE activities
# Classification of environmental protection activities (CEPA) and
# classification of resource management activities (CReMA) [ceparema]:
# [TOTAL] Total environmental protection and resource management activities
# Type of expenditure [ty]: [TOT_EGSS] Total environmental goods and services sector
# Unit of measure [unit]:  [PC_GDP] Percentage of gross domestic product (GDP)

mitigation_pc_gdp <- eurostat::get_eurostat(id = "env_ac_egss2")

mitigation_pc_gdp_raw <- eurostat::get_eurostat(id = "env_ac_egss2") %>%
  filter (.data$nace_r2 %in% c("TOTAL"),
          .data$ceparema == "TOTAL",
          .data$ty == "TOT_EGSS",
          .data$unit == "PC_GDP")


gbard_expenditure_indicators <- get_eurostat_indicator(
  preselected_indicators = gbard_expenditure_raw,
  id = "gba_nabsfin07")

gerd_expenditure_indicators <- get_eurostat_indicator(
  preselected_indicators = gerd_expenditure_raw,
  id = "rd_e_gerdsobj07")

mitigation_pc_gdp_indicators <- get_eurostat_indicator(
  preselected_indicators = mitigation_pc_gdp_raw,
  id = "env_ac_egss2")

indicators_to_impute <- gbard_expenditure_indicators$indicator %>%
  bind_rows ( gerd_expenditure_indicators$indicator ) %>%
  bind_rows ( mitigation_pc_gdp_indicators$indicator)

metadata_to_update <- gbard_expenditure_indicators$metadata %>%
  bind_rows ( gerd_expenditure_indicators$metadata )  %>%
  bind_rows ( mitigation_pc_gdp_indicators$metadata)

labelling_bind <- gbard_expenditure_indicators$labelling %>%
  bind_rows ( gerd_expenditure_indicators$labelling )  %>%
  bind_rows ( mitigation_pc_gdp_indicators$labelling )

imp <- impute_indicators ( indic = indicators_to_impute )

updated_metadata <- update_metadata(imp, metadata = metadata_to_update )

set.seed(2021)
updated_metadata %>%
  select ( all_of ( c("indicator_code", "actual", "missing", "nocb", "locf", "approximate", "forecast")))

keywords <- add_keywords (gbard_expenditure_indicators$metadata, list( "greendeal", "economy", "supply", "rd")) %>%
  bind_rows ( add_keywords (gerd_expenditure_indicators$metadata, list( "greendeal", "economy", "supply", "rd")) ) %>%
  bind_rows ( add_keywords (mitigation_pc_gdp_indicators$metadata, list( "greendeal", "economy", "supply", "general")) ) %>%
  select ( all_of(c("indicator_code", "keyword_1", "keyword_2", "keyword_3", "keyword_4", "further_keywords")))

not_included_path <- ifelse ( dir.exists("data-raw"),
                              yes = file.path("data-raw", "greendeal.db"),
                              no = file.path("..", "data-raw", "greendeal.db"))

create_database (indicator_tables = imp,
                 metadata_tables = updated_metadata,
                 labelling_table = labelling_bind,
                 keywords_table = keywords,
                 db_path = not_included_path)




