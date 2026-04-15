
library(tidyverse)
library(pharmaverseraw)
library(admiral)
library(sdtm.oak)
library(lubridate)

#Open mapping file
study_ct<-read_csv("/home/anissa/sdtm_ct.csv")

data <- pharmaverseraw::ds_raw
data$INSTANCE[data$INSTANCE == "Ambul Ecg Removal"] <- "Ambul ECG Removal"

# Create df and generate ids for mapping with study_ct
raw.data <- data %>%
  mutate(STUDYID = STUDY) %>%
  generate_oak_id_vars(pat_var = "PATNUM", raw_src = "ds_raw")

print(oak_id_vars())

# Create DS variables
ds.df <- raw.data %>%
  mutate(
    DOMAIN = "DS",
    USUBJID = paste0(STUDYID, "-", PATNUM),
    DSCAT = "DISPOSITION EVENT"
  ) %>%
  dplyr::select(STUDYID, USUBJID, DOMAIN, DSCAT, oak_id, raw_source, patient_number) 
# Map with study_ct
ds.df <- assign_no_ct(
  tgt_dat = ds.df,                  
  raw_dat = raw.data,                  
  raw_var = "IT.DSTERM",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars())

# DSDECOD variable: use the codelist for the Controlled terminology from study_ct
ds.df <- assign_ct(
  tgt_dat = ds.df,
  raw_dat = raw.data,
  raw_var = "IT.DSTERM",
  tgt_var = "DSDECOD",
  ct_spec = study_ct,
  ct_clst = "C66727",
  id_vars = oak_id_vars())

#VISIT variable
ds.df <- assign_ct(
  tgt_dat = ds.df,
  raw_dat = raw.data,
  raw_var = "INSTANCE",
  tgt_var = "VISIT",
  ct_spec = study_ct,
  ct_clst = "VISIT",
  id_vars = oak_id_vars())

ds.df <- assign_ct(
  tgt_dat = ds.df,
  raw_dat = raw.data,
  raw_var = "INSTANCE",
  tgt_var = "VISITNUM",
  ct_spec = study_ct,
  ct_clst = "VISITNUM",
  id_vars = oak_id_vars())

# DSDTCOL variable
ds.df <- assign_no_ct(
  tgt_dat = ds.df,
  raw_dat = raw.data,
  raw_var = "DSDTCOL",
  tgt_var = "DSDTC",
  id_vars = oak_id_vars())

# DSSTDTC variable
ds.df <- assign_no_ct(
  tgt_dat = ds.df,
  raw_dat = raw.data,
  raw_var = "IT.DSSTDAT",
  tgt_var = "DSSTDTC",
  id_vars = oak_id_vars())


#DSSTDY variable: no study reference data, so NA here
ds.df<-mutate(ds.df,DSSTDY=NA_real_)


# Handling variables not mapped
ds.df<-ds.df%>%
  mutate(
    DSDECOD = case_when(
      is.na(DSDECOD) & str_detect(DSTERM, "Randomized") ~ "RANDOMIZED",
      is.na(DSDECOD) & str_detect(DSTERM, "Protocol Completed") ~ "COMPLETED",
      TRUE ~ coalesce(DSDECOD, toupper(DSTERM))))%>%
  #group_by(USUBJID) %>%
  #arrange(DSDTC, DSSTDTC)%>%
  mutate(DSSEQ = row_number())%>%
  #ungroup() %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, 
         VISITNUM, VISIT, DSDTC, DSSTDTC)

write.csv(ds.df, "DS_dataset.csv", row.names = FALSE)


