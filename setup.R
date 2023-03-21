library(dplyr)

input <- "Outputs/main.csv"
output <- "Outputs/main_2.csv"

dataSOEP <- read.csv(file=input)

dataSOEP_new <- subset(dataSOEP, dataSOEP$birth_year >0 & dataSOEP$syear >0)

# reshape the variables

# employment status
# not employed as 0 and others are all 1
#dataSOEP_new$emp_status[dataSOEP_new$emp_status %in% c(1, 2, 3, 4, 6) ] <- 1
#dataSOEP_new$emp_status[dataSOEP_new$emp_status == 5 ] <- 0

# religion
# no preference to 0 and others are 1
dataSOEP_new$religion_status[dataSOEP_new$religion_status %in% c(4, 5, 7) ] <- 1
dataSOEP_new$religion_status[dataSOEP_new$religion_status ==6 ] <- 0

# Creating NEW VARS: giveaway_2010, giveaway_2017, giveaway_avg
data_2010A <- subset(dataSOEP_new, dataSOEP_new$syear==2010 & is.na(dataSOEP_new$giveaway)==FALSE, select= c("pid", "giveaway"))
data_2017A <- subset(dataSOEP_new, dataSOEP_new$syear==2017 & is.na(dataSOEP_new$giveaway)==FALSE, select= c("pid", "giveaway"))
corrdata1 <- full_join(data_2010A, data_2017A, by= "pid", suffix= c("_2010", "_2017"))
dataSOEP_new <- left_join(dataSOEP_new, corrdata1, by= "pid")
dataSOEP_new$giveaway_avg <- rowMeans(dataSOEP_new[,c("giveaway_2010", "giveaway_2017")], na.rm=TRUE)
dataSOEP_new$diff_giveaway <- dataSOEP_new$giveaway_2017- dataSOEP_new$giveaway_2010

###### Life Goal Quesitons 2008 AND 2016 ###### 
data_2008A <- subset(dataSOEP_new, dataSOEP_new$syear==2008 & is.na(dataSOEP_new$lifegoal_help)==FALSE & is.na(dataSOEP_new$lifegoal_so_po)==FALSE, select= c("pid", "lifegoal_help", "lifegoal_so_po"))
data_2016A <- subset(dataSOEP_new, dataSOEP_new$syear==2016 & is.na(dataSOEP_new$lifegoal_help)==FALSE & is.na(dataSOEP_new$lifegoal_so_po)==FALSE, select= c("pid", "lifegoal_help", "lifegoal_so_po"))
corrdata2 <- full_join(data_2008A, data_2016A, by= "pid", suffix= c("_2008", "_2016"))
dataSOEP_new <- left_join(dataSOEP_new, corrdata2, by= "pid")

###### Political Attitude from 2014 and 2019 ###### 
data_2014A <- subset(dataSOEP_new, dataSOEP_new$syear==2014 & is.na(dataSOEP_new$political_attitude)==FALSE, select= c("pid", "political_attitude"))
data_2019A <- subset(dataSOEP_new, dataSOEP_new$syear==2019 & is.na(dataSOEP_new$political_attitude)==FALSE, select= c("pid", "political_attitude"))
corrdata3 <- full_join(data_2014A, data_2019A, by= "pid", suffix= c("_2014", "_2019"))

dataSOEP_new <- left_join(dataSOEP_new, corrdata3, by= "pid")

###### Personal Patience from 2013 and 2018 ###### 
data_2013A <- subset(dataSOEP_new, dataSOEP_new$syear==2013 & is.na(dataSOEP_new$patience)==FALSE, select= c("pid", "patience"))
data_2018A <- subset(dataSOEP_new, dataSOEP_new$syear==2018 & is.na(dataSOEP_new$patience)==FALSE, select= c("pid", "patience"))
corrdata4 <- full_join(data_2013A, data_2018A, by= "pid", suffix= c("_2013", "_2018"))
dataSOEP_new <- left_join(dataSOEP_new, corrdata4, by= "pid")

###### risk_financial and risk_health from 2014 ###### 
corrdata5 <- dataSOEP_new %>%
  select(risk_financial, pid, syear) %>% 
  filter(syear==2014) %>%
  mutate(risk_financial_2014 = risk_financial)%>%
  select(risk_financial_2014, pid) %>%
  na.omit()
corrdata6 <- dataSOEP_new %>%
  select(risk_health, pid, syear) %>% 
  filter(syear==2014) %>%
  mutate(risk_health_2014 = risk_health)%>%
  select(risk_health_2014, pid) %>%
  na.omit()

dataSOEP_new <- left_join(dataSOEP_new, corrdata5, by= "pid")
dataSOEP_new <- left_join(dataSOEP_new, corrdata6, by= "pid")


#Creating Gender Dummy and reverse coding life_goal and volunteer
dataSOEP_new <- dataSOEP_new %>% 
  mutate(female = ifelse(sex == 2, 1, 0),
         lifegoal_help_r = case_when(lifegoal_help==1 ~ 4,
                                     lifegoal_help==2 ~ 3,
                                     lifegoal_help==3 ~ 2,
                                     lifegoal_help==4 ~ 1),
         lifegoal_so_po_r = case_when(lifegoal_so_po==1 ~ 4,
                                      lifegoal_so_po==2 ~ 3,
                                      lifegoal_so_po==3 ~ 2,
                                      lifegoal_so_po==4 ~ 1),
         volunteer_work_r = case_when(volunteer_work==1 ~ 5,
                                      volunteer_work==2 ~ 4,
                                      volunteer_work==3 ~ 3,
                                      volunteer_work==4 ~ 2,
                                      volunteer_work==5 ~ 1),
         lifegoal_help_2008_r = case_when(lifegoal_help_2008==1 ~ 4,
                                          lifegoal_help_2008==2 ~ 3,
                                          lifegoal_help_2008==3 ~ 2,
                                          lifegoal_help_2008==4 ~ 1),
         lifegoal_so_po_2008_r = case_when(lifegoal_so_po_2008==1 ~ 4,
                                           lifegoal_so_po_2008==2 ~ 3,
                                           lifegoal_so_po_2008==3 ~ 2,
                                           lifegoal_so_po_2008==4 ~ 1),
         lifegoal_help_2016_r = case_when(lifegoal_help_2016==1 ~ 4,
                                          lifegoal_help_2016==2 ~ 3,
                                          lifegoal_help_2016==3 ~ 2,
                                          lifegoal_help_2016==4 ~ 1),
         lifegoal_so_po_2016_r = case_when(lifegoal_so_po_2016==1 ~ 4,
                                           lifegoal_so_po_2016==2 ~ 3,
                                           lifegoal_so_po_2016==3 ~ 2,
                                           lifegoal_so_po_2016==4 ~ 1),
         health_r = case_when(health==1 ~ 5,
                              health==2 ~ 4,
                              health==3 ~ 3,
                              health==4 ~ 2,
                              health==5 ~ 1),
         marriage_status = case_when(marriage==3 ~ 1,
                                     marriage==1 |marriage==7 ~ 2,
                                     marriage==2 |marriage==6 |marriage==8 ~ 3,
                                     marriage==4 ~ 4,
                                     marriage==5 ~ 5)) # single, married, married but separated, divorced, widowed

#divide the giveaway and income variable by 1000 for ease of interpretation
dataSOEP_new$giveaway_divided<-dataSOEP_new$giveaway/1000
dataSOEP_new$giveaway_avg_divided<- dataSOEP_new$giveaway_avg/1000
dataSOEP_new$giveaway_2010_divided<- dataSOEP_new$giveaway_2010/1000
dataSOEP_new$giveaway_2017_divided<- dataSOEP_new$giveaway_2017/1000
dataSOEP_new$diff_giveaway_divided<- dataSOEP_new$diff_giveaway/1000
dataSOEP_new$netincome_divided <- dataSOEP_new$net_income/1000
dataSOEP_new$hh_netincome_divided <- dataSOEP_new$hh_netincome/1000

#Standardize the measures
dataSOEP_new$s_giveaway_divided <- (dataSOEP_new$giveaway_divided - mean(dataSOEP_new$giveaway_divided, na.rm=TRUE)) / sd(dataSOEP_new$giveaway_divided, na.rm = TRUE)
dataSOEP_new$s_giveaway <- (dataSOEP_new$giveaway - mean(dataSOEP_new$giveaway, na.rm = TRUE)) / sd(dataSOEP_new$giveaway, na.rm = TRUE)
dataSOEP_new$s_giveaway_avg_divided <- (dataSOEP_new$giveaway_avg_divided - mean(dataSOEP_new$giveaway_avg_divided, na.rm = TRUE)) / sd(dataSOEP_new$giveaway_avg_divided, na.rm = TRUE)
dataSOEP_new$s_giveaway_2010_divided <- (dataSOEP_new$giveaway_2010_divided - mean(dataSOEP_new$giveaway_2010_divided, na.rm = TRUE)) / sd(dataSOEP_new$giveaway_2010_divided, na.rm = TRUE)
dataSOEP_new$s_giveaway_2017_divided <- (dataSOEP_new$giveaway_2017_divided - mean(dataSOEP_new$giveaway_2017_divided, na.rm = TRUE)) / sd(dataSOEP_new$giveaway_2017_divided, na.rm = TRUE)
dataSOEP_new$s_lifegoal_help_r <- (dataSOEP_new$lifegoal_help_r - mean(dataSOEP_new$lifegoal_help_r, na.rm = TRUE)) / sd(dataSOEP_new$lifegoal_help_r, na.rm = TRUE)
dataSOEP_new$s_lifegoal_so_po_r <- (dataSOEP_new$lifegoal_so_po_r - mean(dataSOEP_new$lifegoal_so_po_r, na.rm = TRUE)) / sd(dataSOEP_new$lifegoal_so_po_r, na.rm = TRUE)
dataSOEP_new$s_lifegoal_help_2008_r <- (dataSOEP_new$lifegoal_help_2008_r - mean(dataSOEP_new$lifegoal_help_2008_r, na.rm = TRUE)) / sd(dataSOEP_new$lifegoal_help_2008_r, na.rm = TRUE)
dataSOEP_new$s_lifegoal_so_po_2008_r <- (dataSOEP_new$lifegoal_so_po_2008_r - mean(dataSOEP_new$lifegoal_so_po_2008_r, na.rm = TRUE)) / sd(dataSOEP_new$lifegoal_so_po_2008_r, na.rm = TRUE)
dataSOEP_new$s_lifegoal_help_2016_r <- (dataSOEP_new$lifegoal_help_2016_r - mean(dataSOEP_new$lifegoal_help_2016_r, na.rm = TRUE)) / sd(dataSOEP_new$lifegoal_help_2016_r, na.rm = TRUE)
dataSOEP_new$s_lifegoal_so_po_2016_r <- (dataSOEP_new$lifegoal_so_po_2016_r - mean(dataSOEP_new$lifegoal_so_po_2016_r, na.rm = TRUE)) / sd(dataSOEP_new$lifegoal_so_po_2016_r, na.rm = TRUE)
dataSOEP_new$s_risk_personal <- (dataSOEP_new$risk_personal - mean(dataSOEP_new$risk_personal, na.rm = TRUE)) / sd(dataSOEP_new$risk_personal, na.rm = TRUE)
dataSOEP_new$s_risk_financial_2014 <- (dataSOEP_new$risk_financial_2014 - mean(dataSOEP_new$risk_financial_2014, na.rm = TRUE)) / sd(dataSOEP_new$risk_financial_2014, na.rm = TRUE)
dataSOEP_new$s_risk_health_2014 <- (dataSOEP_new$risk_health_2014 - mean(dataSOEP_new$risk_health_2014, na.rm = TRUE)) / sd(dataSOEP_new$risk_health_2014, na.rm = TRUE)
dataSOEP_new$s_patience_2013 <- (dataSOEP_new$patience_2013  - mean(dataSOEP_new$patience_2013 , na.rm = TRUE)) / sd(dataSOEP_new$patience_2013 , na.rm = TRUE)
dataSOEP_new$s_patience_2018 <- (dataSOEP_new$patience_2018 - mean(dataSOEP_new$patience_2018, na.rm = TRUE)) / sd(dataSOEP_new$patience_2018, na.rm = TRUE)


write.csv(dataSOEP_new, output, row.names = FALSE)
