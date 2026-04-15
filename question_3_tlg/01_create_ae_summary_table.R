library(tidyverse)
library(ggplot2)
library(gtsummary)
library(pharmaverseadam)

adae<-pharmaverseadam::adae
# Create tables
# - Treatment-emergent AE records will have TRTEMFL == "Y" in pharmaverseadam::adae
# - Rows: AETERM or AESOC
# - Columns: Treatment groups (ACTARM)
# - Cell values: Count (n) and percentage (%)
# - Include total column with all subjects
# - Sort by descending frequency

# only treatment emergent adverse events
adae_teae <- pharmaverseadam::adae %>% 
  filter(TRTEMFL == "Y")

# to get subject counts per arms 
adsl <- pharmaverseadam::adsl

# Create table (based on example FDA Table 10)
tbl <- adae_teae |>
  tbl_hierarchical(
    variables = c(AESOC, AEDECOD),      # Rows: SOC → PT
    by = ACTARM,                        # Columns: Treatment arms
    id = USUBJID,                       # Count unique subjects
    denominator = adsl,                 # N per treatment arm from ADSL
    overall_row = TRUE,            # Sort by frequency
    label = list(AESOC ~ "System Organ Class", 
                 AEDECOD ~ "Preferred Term")
  ) |>
  sort_hierarchical() |>
  modify_header(label = "**Adverse Event**")
tbl |> as_gt() |> gt::gtsave("table_TEAE_FDA.html")



