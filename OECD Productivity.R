rm(list = ls())
library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(fixest)
target_country<-c("United Kingdom","United States","Canada","France","Germany","Italy","Spain","Denmark","Sweden","Netherlands")
target_iso3c<-countrycode(target_country,origin = "country.name",destination = "iso3c")
print(target_iso3c)
stan_data<-read.csv("C:/Users/hp/Desktop/Thesis/data/oecd_stan.csv.csv")
stan_filtered <- stan_data %>%filter(REF_AREA %in% target_iso3c)%>%filter(TIME_PERIOD >= 2013 & TIME_PERIOD <= 2023)%>%filter(MEASURE %in% c("B1G", "EMP"))%>%mutate(OBS_VALUE = as.numeric(OBS_VALUE))%>%select(REF_AREA,ISIC_letter=ACTIVITY,TIME_PERIOD,MEASURE,OBS_VALUE)%>%pivot_wider(names_from=MEASURE,values_from=OBS_VALUE,values_fn=mean)%>%filter(B1G>0&EMP>0)%>%mutate(ln_Y=log(B1G),ln_L=log(EMP))%>%drop_na(ln_Y,ln_L)
dim(stan_filtered)
capital_data<-stan_data%>%filter(REF_AREA%in%target_iso3c)%>%filter(TIME_PERIOD>=2013&TIME_PERIOD<=2023)%>%filter(MEASURE%in%c("N11GA","N11GA_ICT"))%>%mutate(OBS_VALUE=as.numeric(OBS_VALUE))%>%select(REF_AREA,MEASURE,TIME_PERIOD,ISIC_letter=ACTIVITY,OBS_VALUE)%>%pivot_wider(names_from = MEASURE,values_from = OBS_VALUE,values_fn = mean)%>%mutate(Adoption_Index=N11GA_ICT/N11GA)%>%select(REF_AREA,ISIC_letter,TIME_PERIOD,Adoption_Index)
stan_combine<-stan_filtered%>%left_join(capital_data,by=c("REF_AREA","ISIC_letter","TIME_PERIOD"))
dim(stan_combine)
summary(stan_combine$Adoption_Index)
stan_final_panel<-stan_combine%>%filter(nchar(ISIC_letter)==1&grepl("^[A-Z]$",ISIC_letter))%>%mutate(Country_Industry=paste(REF_AREA,ISIC_letter,sep="_"))
dim(stan_final_panel)
length(unique(stan_final_panel$Country_Industry))
epl_raw<-read_excel("C:/Users/hp/Desktop/Thesis/data/EPL.xlsx")
clean_epl <- epl_raw %>%select(country_raw = 1, 3:13)%>%setNames(c("country_raw", as.character(2013:2023))) %>%slice(-(1:5)) %>%filter(!is.na(country_raw)) %>%pivot_longer(cols = -country_raw,names_to = "TIME_PERIOD",values_to = "EPL_score")%>%mutate(TIME_PERIOD=as.numeric(TIME_PERIOD),EPL_score= as.numeric(EPL_score),REF_AREA= countrycode(country_raw, origin = "country.name", destination = "iso3c"))%>%filter(REF_AREA %in% target_iso3c)%>%select(REF_AREA, TIME_PERIOD, EPL_score)%>%drop_na(EPL_score)
table(clean_epl$REF_AREA)
dim(clean_epl)
head(clean_epl)
aiie<-"C:/Users/hp/Desktop/Thesis/data/AIOE-main/AIOE_DataAppendix.xlsx"
excel_sheets(aiie)
read_excel(aiie,sheet = "Index")
head(read_excel(aiie,sheet = "Appendix A",n_max = 3))
head(read_excel(aiie,sheet = "Appendix B",n_max = 3))
aiie_clean <- aiie_raw %>%rename(NAICS = 1, AIIE = 3) %>%mutate(naics_2digit = as.numeric(substr(as.character(NAICS), 1, 2)),ISIC_letter = case_when(naics_2digit == 11 ~ "A",
      +             naics_2digit == 21 ~ "B",
      +             naics_2digit %in% c(31, 32, 33) ~ "C",
      +             naics_2digit == 22 ~ "D",
      +             naics_2digit == 23 ~ "F",
      +             naics_2digit %in% c(42, 44, 45) ~ "G",
      +             naics_2digit %in% c(48, 49) ~ "H",
      +             naics_2digit == 72 ~ "I",
      +             naics_2digit == 51 ~ "J",
      +             naics_2digit == 52 ~ "K",naics_2digit == 53 ~ "L",naics_2digit == 54 ~ "M",naics_2digit == 56 ~ "N",naics_2digit == 92 ~ "O",naics_2digit == 61 ~ "P",naics_2digit == 62 ~ "Q",naics_2digit == 71 ~ "R",naics_2digit == 81 ~ "S",TRUE ~ NA_character_))%>%filter(!is.na(ISIC_letter))%>%group_by(ISIC_letter) %>%summarise(AIIE_final=mean(AIIE,na.rm =TRUE),.groups = "drop")
final_modal_data<-stan_final_panel%>%left_join(clean_epl,by=c("REF_AREA","TIME_PERIOD"))%>%left_join(aiie_clean,by="ISIC_letter")%>%mutate(interaction_AIOE_EPL=AIIE_final*EPL_score,Effective_AIOE=AIIE_final*Adoption_Index,interaction_Eff_EPL=Effective_AIOE*EPL_score
dim(final_modal_data)
head(final_modal_data)
library(fixest)
final_modal_data<-final_modal_data%>%mutate(lp=ln_Y-ln_L)
model_input<-feols(ln_L~interaction_AIOE_EPL|Country_Industry+TIME_PERIOD,data = final_modal_data,cluster = ~REF_AREA
model_output<-feols(ln_Y~interaction_AIOE_EPL|Country_Industry+TIME_PERIOD,data = final_modal_data,cluster = ~REF_AREA
model_both<-feols(lp~interaction_AIOE_EPL|Country_Industry+TIME_PERIOD,data = final_modal_data,cluster = ~REF_AREA)
etable("Input(ln_L)" = model_input,"Output(ln_Y)" = model_output,"Both(ln_Y-ln_L)" = model_both)
model_input_eff <- feols(
  ln_L ~ interaction_Eff_EPL + Effective_AIOE | Country_Industry + TIME_PERIOD, 
  data = final_modal_data, 
  cluster = ~REF_AREA
)

model_output_eff <- feols(
  ln_Y ~ interaction_Eff_EPL + Effective_AIOE | Country_Industry + TIME_PERIOD, 
  data = final_modal_data, 
  cluster = ~REF_AREA
)

model_both_eff <- feols(
  lp ~ interaction_Eff_EPL + Effective_AIOE | Country_Industry + TIME_PERIOD, 
  data = final_modal_data, 
  cluster = ~REF_AREA
)

etable(
  "Input(Eff)" = model_input_eff, 
  "Output(Eff)" = model_output_eff, 
  "Productivity(Eff)" = model_both_eff
)