library(haven)
library(dplyr)
library(naniar)

input_pl <- "data/soep/cs-transfer/Stata/pl.dta"
input_pgen <- "data/soep/cs-transfer/Stata/pgen.dta"
input_bio <- "data/soep/cs-transfer/Stata/biol.dta"
input_hgen <- "data/soep/cs-transfer/Stata/hgen.dta"
input_ppathl <- "data/soep/cs-transfer/Stata/ppathl.dta"
output <- "Outputs/main.csv"

pl <- read_dta(input_pl, col_select = c(plh0129, plh0130, plh0131_v1, plh0131_v2, plh0132, plh0133,
                                        pli0095_h, pli0096_h, plj0438, plj0442, plj0440,
                                        plj0439, plj0441, plj0443, plh0212, plh0213, plh0214, plh0215, 
                                        plh0216, plh0217, plh0218, plh0219, plh0220, plh0221, plh0222, 
                                        plh0223, plh0224, plh0225, plh0226, plh0255, pid, syear, hid, 
                                        plh0258_h, plh0004, ple0010_h, ple0008, plh0197, plh0198,
                                        plh0199, plh0200, plh0201, plh0202, plh0203, plh0204_h, plh0253,
                                        plh0135, plh0105, plh0111, plh0258_v9))

#personal status: nationality, marital status, income, job status and years of education
pgen <- read_dta(input_pgen, col_select = c(pid, syear, pgnation, pgfamstd, pglabgro, pglabnet,
                                            pgsndjob, pgstib, pgemplst, pgbilzeit))


## Adding the sex variable
ppathl <- read_dta(input_ppathl, col_select = c(pid, syear, sex))

## Adding hh net income
hgen <- read_dta(input_hgen, col_select = c(hid, syear, hghinc))

########## DATA CLEANING AND RESHAPING ##########

pl_clean <- pl %>%
  mutate(health = ifelse(ple0008<0, NA, ple0008),
         birth_year = ifelse(ple0010_h<0, NA, ple0010_h),
         political_attitude = ifelse(plh0004<0, NA, plh0004),
         money_subscribe = ifelse(plh0129<0, NA, plh0129),
         money_subscribe_amount = ifelse(plh0130<0, NA, plh0130),
         blood_donation_fiveyears = ifelse(plh0131_v2<0, NA, plh0131_v2),
         blood_donation_tenyears = ifelse(plh0131_v1<0, NA, plh0131_v1),
         blood_donation_lastyear = ifelse(plh0132<0, NA, plh0132),
         medicalreason_no_donation = ifelse(plh0133<0, NA, plh0133),
         risk_drive = ifelse(plh0197<0, NA, plh0197),
         risk_financial = ifelse(plh0198<0, NA, plh0198),
         risk_leisure_sport = ifelse(plh0199<0, NA, plh0199),
         risk_job = ifelse(plh0200<0, NA, plh0200),
         risk_health = ifelse(plh0201<0, NA, plh0201),
         risk_trust = ifelse(plh0202<0, NA, plh0202),
         risk_after_lottery = ifelse(plh0203<0, NA, plh0203),
         risk_personal = ifelse(plh0204_h<0, NA, plh0204_h),
         patience = ifelse(plh0253<0, NA, plh0253),
         religion = ifelse(plh0258_h<0, NA, plh0258_h),
         religion_status = ifelse(plh0258_v9<0, NA, plh0258_v9),
         help_friends = ifelse(pli0095_h<0, NA, pli0095_h),
         volunteer_work = ifelse(pli0096_h<0, NA, pli0096_h),
         refu_money_donation_last_year = ifelse(plj0438<0, NA, plj0438),
         refu_money_donation_future = ifelse(plj0439<0, NA, plj0439),
         refu_work_with_last_year = ifelse(plj0440<0, NA, plj0440),
         refu_work_with_future = ifelse(plj0441<0, NA, plj0441),
         refu_demonstrations_last_year = ifelse(plj0442<0, NA, plj0442),
         refu_demonstrations_future = ifelse(plj0443<0, NA, plj0443),
         giveaway = ifelse(plh0135<0, NA, plh0135),
         lifegoal_help = ifelse(plh0105<0, NA, plh0105),
         lifegoal_so_po = ifelse(plh0111<0, NA, plh0111)
  ) %>%
  select(!c(ple0008, ple0010_h, plh0004, plh0129, plh0130,plh0131_v1, plh0131_v2,
            plh0132, plh0133, plh0197, plh0198, plh0199, plh0200, plh0201, 
            plh0202, plh0203, plh0204_h, plh0253, plh0258_h, pli0095_h, pli0096_h,
            plj0438, plj0439, plj0440, plj0441, plj0442, plj0443, plh0135,
            plh0105, plh0111, plh0258_v9))

pgen_clean <- pgen %>%
  mutate(emp_status = ifelse(pgemplst<0, NA, pgemplst),
         marriage = ifelse(pgfamstd<0, NA, pgfamstd),
         net_income = ifelse(pglabnet<0, NA, pglabnet),
         job_position = ifelse(pgstib<0, NA, pgstib),
         education = ifelse(pgbilzeit<0, NA, pgbilzeit)) %>%
  select(!c(pgemplst, pgfamstd, pglabnet, pgstib, pgbilzeit))


ppathl_clean <- ppathl %>%
  replace_with_na(replace = list(sex = c(-3,-1)))

hgen_clean <- hgen %>%
  mutate(hh_netincome = ifelse(hghinc %in% c(-3, -2, -1), NA, hghinc),
         hh_netincome = ifelse(hghinc %in% c(-3000, -400), 0, hh_netincome)) %>%
  select(!c(hghinc))


########## DATA MERGING ##########

dataSOEP <- pl_clean %>%
  left_join(pgen_clean, by = c("pid", "syear"))

dataSOEP <- dataSOEP %>%
  left_join(ppathl_clean, by = c("pid", "syear")) 

dataSOEP <- dataSOEP %>%
  left_join(hgen_clean, by = c("hid", "syear")) 

write.csv(dataSOEP, output, row.names = FALSE)




