library(eurostat)
library(tidyverse)
library(ggrepel)

cofog <- get_eurostat(id = "gov_10a_exp")

cultural_spending <- cofog %>%
  filter ( grepl("GF08",.data$cofog99) )

cultural_spending_selection <- cultural_spending %>%
  filter ( .data$cofog99 %in% c("GF0802", "GF0803"),   # relevant cultural spending categories
           .data$unit %in% c("MIO_EUR", "PC_GDP") ,    # total number or per total spending
           .data$na_item %in% c("TE", "D3"))           # total expenditure, subsidies


govt_cultural_spending <- cultural_spending_selection  %>%
  filter ( .data$cofog99 == "GF0802",
           .data$na_item %in% c("TE"),
           .data$sector %in% c("S13", "S1311")) %>%
  get_eurostat_indicator(id = "gov_10a_exp")


imputed_govt_spending <- govt_cultural_spending$indicator %>%
  impute_indicators()

## Here I create an "own" indicator, the ration of centralization in government cultural
## spending

central_cultural_spending <- imputed_govt_spending %>%
  filter ( .data$indicator_code %in%
             c(
               "eurostat_gov_10_a_exp_te_s_1311_gf_0802_mio_eur"
             )
  ) %>%
  select ( all_of(c("geo", "time", "unit", "value", "estimate", "method"))) %>%
  rename ( value_c = .data$value,
           estimate_c = .data$estimate,
           method_c  = .data$method )

total_cultural_spending <- imputed_govt_spending %>%
  filter ( .data$indicator_code == "eurostat_gov_10_a_exp_te_s_13_gf_0802_mio_eur"
  )


## Whenever not actual / actual values are used, but estimates, the
## ratio must be an estimate, too !

central_government_cultural_spending_ratio <- total_cultural_spending  %>%
  left_join ( central_cultural_spending,
              by = c("unit", "time", "geo")) %>%
  mutate ( estimate  = ifelse(.data$estimate =="actual" & .data$estimate_c != "actual",
                              yes = .data$estimate_c,
                              no = .data$estimate ),
           method  = ifelse(.data$method =="actual" & .data$method_c != "actual",
                            yes = .data$method_c,
                            no = .data$method ),
           value = .data$value_c / .data$value ) %>%
  mutate ( indicator_code = "music_observatory_gov_10_a_exp_te_gf_0802_central_govt_ratio",
           description_indicator = "Central government cultural spending in total government cultural spending",
           db_source_code = "digital_music_observatory_based_on_eurostat") %>%
  mutate ( method = ifelse (is.na(.data$value), "missing", .data$method),
           estimate = ifelse(is.na(.data$value), "missing", .data$estimate )
  ) %>%
  select ( -ends_with("_c"))


## I make a preselection to avoid plotting empty group geo concepts such as EA19 --

to_plot <- central_government_cultural_spending_ratio %>%
  select ( all_of(c("geo", "time", "value"))) %>%
  left_join ( add_country_groups(), by = 'geo' )  %>%
  filter ( !is.na(.data$value),
           !.data$country_group %in% c("EU", "Eurozone"))


central_gov_cultural_spending_plot <- to_plot %>%
  ggplot ( aes ( x = .data$time,
                 y = .data$value,
                 label = .data$geo,
                 color = .data$geo)) +
  geom_line() +
  scale_x_date() +
  scale_y_continuous( labels = scales::percent ) +
  scale_color_manual( values = create_discrete_observatory_palette() ) +
  geom_text_repel(data = filter(to_plot, time == max(time)),
                  aes(label = .data$geo ),
                  hjust = 0, nudge_x = 0.1) +
  # Allow labels to bleed past the canvas boundaries
  coord_cartesian(clip = 'off') +
  facet_wrap ( facets = "country_group") +
  labs ( title = "Central Government Share in Government Spending on Cultural Activities",
         x = NULL, y = "% of total government cultural spending",
         caption = "Digital Music Observatory, curated by Daniel Antal, \u00a9 2021,
         get the data: music.dataobseratory.eu") +
  theme ( legend.position = "none",
          plot.margin = margin(0.1, 2, 0.1, 0.1, "cm"))

ggsave ( filename = "not_included/central_gov_cultural_spending_plot.png",
         plot = central_gov_cultural_spending_plot, units = "cm",
         width = 15, height = 10, dpi = 350)


write.csv ( x = central_government_cultural_spending_ratio,
            file = file.path ( 'not_included', "central_government_cultural_spending_ratio.csv"),
            row.names = FALSE)

read.csv (
  file = file.path ( 'not_included', "central_government_cultural_spending_ratio.csv" )
)
