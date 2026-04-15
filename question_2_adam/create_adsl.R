library(admiral)
library(tidyverse)
library(pharmaversesdtm)
library(lubridate)
library(stringr)

dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae
lb <- pharmaversesdtm::lb

#Handling NA values
dm <- convert_blanks_to_na(dm)

# Derive age categories from AGE
agegr9_lookup <- exprs(
  ~condition,            ~AGEGR9, ~AGEGR9N,
  is.na(AGE),          "Missing",        4,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 64),  "18-64",        2,
  !is.na(AGE),             ">64",        3
)

adsl<- derive_vars_cat(
  dataset = dm,
  definition = agegr9_lookup
)

# Derive treatment start date-time only to patients received PLACEBO or dose>0


#ITTFL variable; Set to "Y" if [DM.ARM] not equal to missing Else set to "N"
adsl<-adsl%>%mutate(ITTFL=ifelse((is.na(dm$ARM)| dm$ARM!="missing"),"Y","N"))

#last date patient has showed to be alive from
# [VS.VSSTRESN] and [VS.VSSTRESC] not both missing) and datepart of [VS.VSDTC] non missing
# last complete onset date of AEs (datepart of Start Date/Time ofAdverse Event [AE.AESTDTC])
# last complete disposition date (datepart of Start Date/Time of Disposition Event [DS.DSSTDTC]).
# last date of treatment administration: [ADSL.TRTEDTM]
lastDateAlive <- bind_rows(
  vs %>% transmute(USUBJID, date = VSDTC),
  ae %>% transmute(USUBJID, date = AESTDTC),
  ds %>% transmute(USUBJID, date = DSSTDTC), 
  ex %>% transmute(USUBJID, date = EXSTDTC)
) %>%
  filter(!is.na(date)) %>%
  slice_max(date, by = USUBJID)

adsl <- adsl %>% 
  left_join(lastDateAlive %>% transmute(USUBJID, LSTAVLDT = date), by = "USUBJID")

adsl <- adsl %>%
  select(USUBJID, STUDYID, SUBJID, AGE, AGEGR9, AGEGR9N, 
         ITTFL, LSTAVLDT, ARM, ARMCD, everything()) %>%
  arrange(USUBJID)

write.csv(adsl, "create_adsl.csv", row.names = FALSE, na = "")

  