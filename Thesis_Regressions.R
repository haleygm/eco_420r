#Thesis Regressions Using R 
#Import Packages
library(tidyverse)
library(readstata13)
library(foreign)
library(writexl)
library(AER)
library(lmtest)
library(sandwich)
library(stargazer)
library(margins)
library(mfx)
library(sampleSelection)
library(tinytex)

options(scipen = 100)
options(digits = 5)
options(width = 100)

#Import data 
data = read.dta13("thesis_r_data.dta")


# Cleaning Data -----------------------------------------------------------
names(data)[names(data) == "T"] <- "Trend"
head(data)
# Medicaid Adoption -------------------------------------------------------

ma_data <- data[which(data$ABB!= "AZ" & data$YEAR > 1965 & data$YEAR <= 1970), ]
ma_data_az <- data[which(data$YEAR > 1965 & data$YEAR <= 1970), ]

#Year FE 
m1 = lm(MED_PRG~ LAG_D_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
        MAA_EXP_65+log_lrgdp_pc+
        WEST+MIDWEST+NORTHEAST+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+PERC_AFDCP65+year_fe3+year_fe4+year_fe5+year_fe6,
        data = ma_data)
summary(m1)
het_corr_m1 = vcovHC(m1, type = "HC1" )
m1r = coeftest(m1, vcov = het_corr_m1)
m1r

#Trend 
m2 = lm(MED_PRG~ LAG_D_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
        MAA_EXP_65+log_lrgdp_pc+
        WEST+MIDWEST+NORTHEAST+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+PERC_AFDCP65+Trend,
        data = ma_data)
summary(m2)
het_corr_m2= vcovHC(m2, type = "HC1" )
m2r = coeftest(m2, vcov = het_corr_m2)
m2r

#Include AZ 
m3 = lm(MED_PRG~ LAG_D_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
         MAA_EXP_65+log_lrgdp_pc+
        WEST+MIDWEST+NORTHEAST+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+PERC_AFDCP65+year_fe3+year_fe4+year_fe5+year_fe6, 
        data = ma_data_az)
summary(m3)
het_corr_m3= vcovHC(m3, type = "HC1" )
m3r = coeftest(m3, vcov = het_corr_m3)
m3r 



#Probit 
probit_m4 <- glm(MED_PRG ~ LAG_D_GOV + LAG_D_LEG + LAG_SPLIT_LEG + LAG_SPLIT_STATE + 
                         MAA_EXP_65 + log_lrgdp_pc +
                         WEST + MIDWEST + NORTHEAST + LAG_PERC_BLACK + LAG_PERC_UNDER_65 + LAG_PERC_FEMALE +
                         LAG_PERC_URBAN + LAG_PERC_FOREIGN_BORN +
                         LAG_HBEDS_PER_1000 + LAG_AP_PER_1000 + PERC_AFDCP65 +
                         year_fe3 + year_fe4 + year_fe5 + year_fe6 , 
                 data = ma_data, 
                 family = binomial(link = "logit"))

m4 = probitmfx(MED_PRG ~ LAG_D_GOV + LAG_D_LEG + LAG_SPLIT_LEG + LAG_SPLIT_STATE + 
                  MAA_EXP_65 + log_lrgdp_pc +
                  WEST + MIDWEST + NORTHEAST + LAG_PERC_BLACK + LAG_PERC_UNDER_65 + LAG_PERC_FEMALE +
                  LAG_PERC_URBAN + LAG_PERC_FOREIGN_BORN +
                  LAG_HBEDS_PER_1000 + LAG_AP_PER_1000 + PERC_AFDCP65 +
                  year_fe3 + year_fe4 + year_fe5 + year_fe6 , data = ma_data, atmean = F, robust = T)
m4

#Extract estimated marginal effects and standard errors 
probit_mfx_coef <- m4$mfxest[,1]
probit_mfx_se <- m4$mfxest[,2]

#Logit 
logit_m5 <- glm(MED_PRG ~ LAG_D_GOV + LAG_D_LEG + LAG_SPLIT_LEG + LAG_SPLIT_STATE + 
                        MAA_EXP_65 + log_lrgdp_pc +
                        WEST + MIDWEST + NORTHEAST + LAG_PERC_BLACK + LAG_PERC_UNDER_65 + LAG_PERC_FEMALE +
                        LAG_PERC_URBAN + LAG_PERC_FOREIGN_BORN +
                        LAG_HBEDS_PER_1000 + LAG_AP_PER_1000 + PERC_AFDCP65 +
                        year_fe3 + year_fe4 + year_fe5 + year_fe6 , 
                data = ma_data,
                family = binomial(link = "probit"))

m5 = logitmfx(MED_PRG ~ LAG_D_GOV + LAG_D_LEG + LAG_SPLIT_LEG + LAG_SPLIT_STATE + 
                      MAA_EXP_65 + log_lrgdp_pc + 
                      WEST + MIDWEST + NORTHEAST + LAG_PERC_BLACK + LAG_PERC_UNDER_65 + LAG_PERC_FEMALE +
                      LAG_PERC_URBAN + LAG_PERC_FOREIGN_BORN +
                      LAG_HBEDS_PER_1000 + LAG_AP_PER_1000 + PERC_AFDCP65 + 
                      year_fe3 + year_fe4 + year_fe5 + year_fe6, data = ma_data, atmean = F, robust = T)
m5

#Extract estimated marginal effects and standard errors 
logit_mfx_coef <- m5$mfxest[,1]
logit_mfx_se <- m5$mfxest[,2]


# Eligibility: Payment to Need Standard Ratio -----------------------------

#AZ, YEAR FE 
m6 = lm(PMT_V_NEED~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe5+year_fe6+year_fe7+year_fe8+year_fe9+year_fe10+
                year_fe11+year_fe12+year_fe13+year_fe14+year_fe15+year_fe16+year_fe17+year_fe18+year_fe19+year_fe20+year_fe21
        +year_fe22+year_fe23+year_fe24+year_fe25+year_fe26+year_fe27+year_fe27+year_fe28+year_fe29+year_fe30+year_fe31+year_fe32,
        data = data[which(data$YEAR>=1968 & data$YEAR<=1996), ])
summary(m6)
het_corr_m6 = vcovHC(m6, type = "HC1" )
m6r = coeftest(m6, vcov = het_corr_m6)
m6r

#NO AZ, YEAR FE
m7 = lm(PMT_V_NEED~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe5+year_fe6+year_fe7+year_fe8+year_fe9+year_fe10+
                year_fe11+year_fe12+year_fe13+year_fe14+year_fe15+year_fe16+year_fe17+year_fe18+year_fe19+year_fe20+year_fe21
        +year_fe22+year_fe23+year_fe24+year_fe25+year_fe26+year_fe27+year_fe27+year_fe28+year_fe29+year_fe30+year_fe31+year_fe32,
        data = data[which(data$YEAR>=1968 & data$YEAR<=1996 & data$ABB != "AZ"), ])
summary(m7)
het_corr_m7 = vcovHC(m7, type = "HC1" )
m7r = coeftest(m7, vcov = het_corr_m7)
m7r 

#AZ, TREND
m8 = lm(PMT_V_NEED~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+Trend,
        data = data[which(data$YEAR>=1968 & data$YEAR<=1996), ])
summary(m8)
het_corr_m8 = vcovHC(m8, type = "HC1" )
m8r = coeftest(m8, vcov = het_corr_m8)
m8r

#Export Tables 
stargazer(m6r, m7r, m8r, title = "Medicaid Eligibility Generosity",
          dep.var.caption = "Payment to Need Standard Ratio", no.space = T)
# Eligibility: Above Mandatory Minimum ------------------------------------
{##Infants/Pregnant Women ====
        #LPM with Year FE (preferred model) 
        m9 = lm(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ])
        summary(m9)
        het_corr_m9 = vcovHC(m9, type = "HC1" )
        m9r = coeftest(m9, vcov = het_corr_m9)
        m9r

        #LPM with Trend 
        m10 = lm(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+Trend,
        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ])
        summary(m10)
        het_corr_m10 = vcovHC(m10, type = "HC1" )
        m10r = coeftest(m10, vcov = het_corr_m10)
        m10r
        
        #Probit with year FE 
        m11_pr = glm(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                         log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                         LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                         LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                         year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                         year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                 data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], family = binomial("probit"))
       
         m11 = probitmfx(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                                year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                                year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], atmean = F, robust = T)
      m11
        probit_mfx_coef_0pw <- m11$mfxest[,1]
        probit_mfx_se_0pw <- m11$mfxest[,2]
        
        #Logit with year FE
        m12_lg = lm(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                         log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                         LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                         LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                         year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                         year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                 data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], family = binomial("logit"))
        
        m12 = logitmfx(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                                year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                                year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], atmean = F, robust = T)
        m12
        logit_mfx_coef_0pw <- m12$mfxest[,1]
        logit_mfx_se_0pw <- m12$mfxest[,2]
}
{##Children 1-5 ====
        #LPM with Year fE 
        m13 = lm(ABOVE_MANDATE_1_5~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ])
        summary(m13)
        het_corr_m13 = vcovHC(m13, type = "HC1" )
        m13r = coeftest(m13, vcov = het_corr_m13)
        m13r
        
        #LPM with Trend
        m14 = lm(ABOVE_MANDATE_1_5~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+Trend, 
                data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ])
        summary(m14)
        het_corr_m14 = vcovHC(m14, type = "HC1" )
        m14r = coeftest(m14, vcov = het_corr_m14)
        m14r
        
        #Probit with Year FE 
        m15_pr = glm(ABOVE_MANDATE_1_5~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                        year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                        year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], family = binomial("probit")) 
        
        m15 = probitmfx(ABOVE_MANDATE_1_5~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                          log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                          LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                          LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                          year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                          year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], atmean = F, robust = T)
        m15
        probit_mfx_coef_15 <- m15$mfxest[,1]
        probit_mfx_se_15 <- m15$mfxest[,2]
        
        #Logit with Year FE 
        m16_lg = glm(ABOVE_MANDATE_1_5~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                        year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                        year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], family = binomial("logit"))
        
        m16 = logitmfx(ABOVE_MANDATE_1_5~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                          log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                          LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                          LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                          year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                          year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], atmean = F, robust = T)
        m16
        logit_mfx_coef_15 <- m16$mfxest[,1]
        logit_mfx_se_15 <- m16$mfxest[,2]
        
}
{##Children 6-18 ====
        #LPM with Year FE
        m17 = lm(ABOVE_MANDATE_6_18~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe27+year_fe28+year_fe29+year_fe30+
                year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49+year_fe50,
        data = data[which(data$YEAR>=1990 & data$YEAR<=2014), ])
        summary(m17)
        het_corr_m17 = vcovHC(m17, type = "HC1" )
        m17r = coeftest(m17, vcov = het_corr_m17)
        m17r

        #LPM with Trend 
        m18 = lm(ABOVE_MANDATE_6_18~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+Trend,
                data = data[which(data$YEAR>= 1990 & data$YEAR<=2014), ])
        summary(m18)
        het_corr_m18 = vcovHC(m18, type = "HC1" )
        m18r = coeftest(m18, vcov = het_corr_m18)
        m18r
        
        #Probit with Year FE 
        m19_pr = glm(ABOVE_MANDATE_6_18~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                        year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                        year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], family = binomial("probit"))
        
        m19 = probitmfx(ABOVE_MANDATE_6_18~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                          log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                          LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                          LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                          year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                          year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], atmean = F, robust = T)
        m19
        probit_mfx_coef_618 <- m19$mfxest[,1]
        probit_mfx_se_618 <- m19$mfxest[,2]
        
        #Logit with Year FE 
        m20_lg = lm(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                        year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                        year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], family = binomial("logit"))
        
        m20 = probitmfx(ABOVE_MANDATE_6_18~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                          log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                          LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                          LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe24+year_fe25+year_fe26+year_fe27+year_fe28+year_fe29+year_fe30+
                          year_fe31+year_fe32+year_fe33+year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+
                          year_fe42+year_fe43+year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49,
                        data = data[which(data$YEAR>=1987 & data$YEAR<=2013), ], atmean = F, robust = T)
        m20
        logit_mfx_coef_618 <- m20$mfxest[,1]
        logit_mfx_se_618 <- m20$mfxest[,2]
}
{##Other Adults ====
       #LPM with Year FE 
        m21 = lm(ANY_OA~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe26+ year_fe27+year_fe28+year_fe29+year_fe30+year_fe31+year_fe32+year_fe33+
                year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+year_fe42+year_fe43+
                year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49+year_fe50+year_fe51+year_fe52+year_fe53+year_fe54,
        data = data[which(data$YEAR>=1989), ])
      summary(m21)
      het_corr_m21 = vcovHC(m21, type = "HC1" )
      m21r = coeftest(m21, vcov = het_corr_m21)
      m21r
  

        #LPM with Trend 
        m22 = lm(ANY_OA~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+Trend,
                data = data[which(data$YEAR>=1989), ])
        summary(m22)
        het_corr_m22 = vcovHC(m22, type = "HC1" )
        m22r = coeftest(m22, vcov = het_corr_m22)
        m22r
                #Probit with Year FE 
        m23_pr = glm(ANY_OA~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                   log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                   LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                   LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe26+ year_fe27+year_fe28+year_fe29+year_fe30+year_fe31+year_fe32+year_fe33+
                   year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+year_fe42+year_fe43+
                   year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49+year_fe50+year_fe51+year_fe52+year_fe53+year_fe54,
                 data = data[which(data$YEAR>=1989), ], family = binomial("probit"))
        
        m23 = probitmfx(ANY_OA~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                          log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                          LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                          LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe26+ year_fe27+year_fe28+year_fe29+year_fe30+year_fe31+year_fe32+year_fe33+
                          year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+year_fe42+year_fe43+
                          year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49+year_fe50+year_fe51+year_fe52+year_fe53+year_fe54,
                        data = data[which(data$YEAR>=1989), ], atmean = F, robust = T)
        m23
        probit_mfx_coef_oa <- m23$mfxest[,1]
        probit_mfx_se_oa <- m23$mfxest[,2]
        
        #Logit with Year FE 
        m24_lg = lm(ABOVE_MANDATE_0_PW~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                        log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                        LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                        LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe27+year_fe28+year_fe29+year_fe30+year_fe31+year_fe32+year_fe33+
                        year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+year_fe42+year_fe43+
                        year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49+year_fe50+year_fe51+year_fe51+year_fe53+year_fe54,
                data = data[which(data$YEAR>=1989), ], family = binomial("logit"))

        m24 =logitmfx(ANY_OA~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
                          log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
                          LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
                          LAG_HBEDS_PER_1000+LAG_AP_PER_1000+year_fe26+ year_fe27+year_fe28+year_fe29+year_fe30+year_fe31+year_fe32+year_fe33+
                          year_fe34+year_fe35+year_fe36+year_fe37+year_fe38+year_fe39+year_fe40+year_fe41+year_fe42+year_fe43+
                          year_fe44+year_fe45+year_fe46+year_fe47+year_fe48+year_fe49+year_fe50+year_fe51+year_fe52+year_fe53+year_fe54,
                        data = data[which(data$YEAR>=1989), ], atmean = F, robust = T)
        m24
        logit_mfx_coef_oa <- m24$mfxest[,1]
        logit_mfx_se_oa <- m24$mfxest[,2]
}        

# Exports to Stargazer ----------------------------------------------------
#Medicaid Adoption 
stargazer(m1r, m2r, m3r, probit_m4, logit_m5, title = "Medicaid Timing Model Robustness I", 
          dep.var.caption = "Indicator if State had Medicaid Program in Year Y", 
          align = T, no.space = T, digits = 4,
          column.labels= c("LPM", "LPM", "LPM"), 
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "State Expenditures for MAA in 1965", "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "% of Population Receiving AFDC in 1965", "Year"),
          coef = list(NULL, NULL, NULL, c(NA,probit_mfx_coef), c(NA,logit_mfx_coef)),
          se = list(NULL, NULL, NULL, c(NA, probit_mfx_se), c(NA, logit_mfx_se)),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors",
          add.lines = list(c("Year Fixed Effects", "Yes", "No", "Yes", "Yes", "Yes"), c("Arizona Included", "No", "Yes", "No", "No", "No"))
        )
#DON'T RUN        
stargazer(probit_m4, logit_m5, title = "Medicaid Timing Model Robustness II", 
          dep.var.caption = "Indicator if State had Medicaid Program in Year Y",
          align = T, no.space = T, digits = 4,
          column.labels = c("Probit", "Logit"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "State Expenditures for MAA in 1965", "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "% of Population Receiving AFDC in 1965", "Year"),
          omit = "year_fe", 
          notes.label = "Robust Standard Errors",
          coef = list(c(NA, probit_mfx_coef), c(NA, logit_mfx_coef)), 
          se = list(c(NA,probit_mfx_se), c(NA, logit_mfx_se)), 
          add.lines = list(c("Year Fixed Effects", "Yes", "Yes"), c("Arizona Included", "No", "No"))
          ) 

#Medicaid PMT to ND 
stargazer(m6r, m7r, m8r, title = "Medicaid Eligibility Generosity",
          dep.var.caption = "Payment to Need Standard Ratio", 
          align = T, no.space = T, digits = 4,
          column.labels = c("OLS", "OLS", "OLS"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Governor Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = "year_fe", 
          notes.label = "Robust Standard Errors",
          add.lines = list(c("Year Fixed Effects", "Yes", "Yes", "No"), c("Arizona Included", "No", "Yes", "No"))
          )

#Medicaid Infants/PW
stargazer(m9r, m10r, m11_pr, m12_lg, title = "Medicaid Eligibility Generosity- Infants and Pregnant Women",
          dep.var.caption = "Indicator Above Mandatory Minimum",
          coef = list(NULL, NULL, c(NA, probit_mfx_coef_0pw), c(NA, logit_mfx_coef_0pw)),
          se = list(NULL, NULL, c(NA, probit_mfx_se_0pw), c(NA, logit_mfx_se_0pw)),
          align = T, no.space = T, digits = 4, 
          column.labels = c("LPM", "LPM", "Probit", "Logit"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Govenror Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors", 
          add.lines = list(c("Year Fixed Effects", "Yes", "No", "Yes", "Yes"),c("Observations", 1296, 1296, 1296, 1296), c("R-squared", 0.401, 0.369)))

#Medicaid Children 1-5
stargazer(m13r, m14r, m15_pr, m16_lg, title = "Medicaid Eligibility Generosity- Children Ages 1-5",
          dep.var.caption = "Indicator Above Mandatory Minimum",
          coef = list(NULL, NULL, c(NA, probit_mfx_coef_15), c(NA, logit_mfx_coef_15)),
          se = list(NULL, NULL, c(NA, probit_mfx_se_15), c(NA, logit_mfx_se_15)),
          align = T, no.space = T, digits = 4, 
          column.labels = c("LPM", "LPM", "Probit", "Logit"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Govenror Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors", 
          add.lines = list(c("Year Fixed Effects", "Yes", "No", "Yes", "Yes"),c("Observations", 1296, 1296, 1296, 1296), c("R-squared", 0.366, 0.257)))

#Medicaid Children 6-18 
stargazer(m17r, m18r, m19_pr, m20_lg, title = "Medicaid Eligibility Generosity- Children Ages 6-18",
          dep.var.caption = "Indicator Above Mandatory Minimum",
          coef = list(NULL, NULL, c(NA, probit_mfx_coef_618), c(NA, logit_mfx_coef_618)),
          se = list(NULL, NULL, c(NA, probit_mfx_se_618), c(NA, logit_mfx_se_618)),
          align = T, no.space = T, digits = 4, 
          column.labels = c("LPM", "LPM", "Probit", "Logit"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Govenror Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors", 
          add.lines = list(c("Year Fixed Effects", "Yes", "No", "Yes", "Yes"),c("Observations", 1200, 1200, 1200, 1200), c("R-squared", 0.351, 0.302)))

#Medicaid Other Adults 
stargazer(m21r, m22r, m23_pr, m24_lg, title = "Medicaid Eligibility Generosity- Other Adults",
          dep.var.caption = "Indicator Any Assistance",
          coef = list(NULL, NULL, c(NA, probit_mfx_coef_oa), c(NA, logit_mfx_coef_oa)),
          se = list(NULL, NULL, c(NA, probit_mfx_se_oa), c(NA, logit_mfx_se_oa)),
          align = T, no.space = T, digits = 4, 
          column.labels = c("LPM", "LPM", "Probit", "Logit"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Govenror Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors", 
          add.lines = list(c("Year Fixed Effects", "Yes", "No", "Yes", "Yes"),c("Observations", 1428, 1428, 1428, 1428), c("R-squared", 0.353, 0.326)))


#Paper Tables
stargazer(m1r, title = "Factors Impacting States' Adoption of Medicaid", 
          dep.var.caption = "Indicator if State had Medicaid Program in Year Y", 
          align = T, no.space = T, digits = 4,
          column.labels= c("LPM"), 
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "State Expenditures for MAA in 1965", "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "% of Population Receiving AFDC in 1965", "Year"),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors",
          add.lines = list(c("Year Fixed Effects", "Yes"), c("Arizona Included", "No"))
)


stargazer(m6r, title = "Factors Impacting State 'Payment' to 'Need' Standard Ratios",
          dep.var.caption = "Payment to Need Standard Ratio", 
          align = T, no.space = T, digits = 4,
          column.labels = c("OLS"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Governor Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = "year_fe", 
          notes.label = "Robust Standard Errors",
          add.lines = list(c("Year Fixed Effects", "Yes"), c("Arizona Included", "Yes"), c("Observations", 1360), c("R-squared",0.397))
)

stargazer(m9r, m13r, m17r, title = "Factors Impacting State Financial Eligibility Generosity",
          dep.var.caption = "Indicator Above Mandatory Minimum",
          align = T, no.space = T, digits = 4, 
          column.labels = c("Infants/Pregnant Women", "Children 1-5", "Children 6-18"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Governor Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors", 
          add.lines = list(c("Years of Interest", "1987-2013", "1987-2013", "1990-2014"),c("Observations", 1296, 1296, 1152), c("R-squared", 0.401, 0.366, 0.353)))

stargazer(m21r, title = "Factors Impacting State Eligibility Generosity for Other Adults",
          dep.var.caption = "Indicator Any Assistance",
          align = T, no.space = T, digits = 4, 
          column.labels = c("LPM"),
          covariate.labels = c("Lag Democratic Governor Dummy", "Lag Independent Governor Dummy",
                               "Lag Majority Democratic Legislature Dummy", 
                               "Lag Split Party Legislature Dummy", "Lag Split Party State Dummy",
                               "Log of Lagged Real State GDP per Capita (2018$)",
                               "West", "Midwest", "Northeast", "Lag of % Poverty", "Lag of % Black", "Lag of % Under 65", 
                               "Lag of % Female", "Lag of % Urban", "Lag of % Foreign Born", 
                               "Lag of Hospital Beds per 1000 Population", "Lag of Active Physicians per 1000 Population",
                               "Year"),
          omit = c("year_fe"), 
          notes.label = "Robust Standard Errors", 
          add.lines = list(c("Years of Interest", "1989-2018"),c("Observations", 1428), c("R-squared", 0.353)))








YEAR <- unique(data$YEAR)
YEAR <- YEAR[4:32]
df <- c(YEAR)

for(i in 1:length(YEAR)){
  ma<- lm(PMT_V_NEED~LAG_D_GOV+LAG_I_GOV+LAG_D_LEG+LAG_SPLIT_LEG+LAG_SPLIT_STATE+
            log_lrgdp_pc+WEST+MIDWEST+NORTHEAST+
            LAG_PERC_POVERTY+LAG_PERC_BLACK+LAG_PERC_UNDER_65+LAG_PERC_FEMALE+LAG_PERC_URBAN+LAG_PERC_FOREIGN_BORN+
            LAG_HBEDS_PER_1000+LAG_AP_PER_1000, data = data[which(data$YEAR == YEAR[i]), ])
  df <- cbind(df, ma$coefficients)
  het_corr_ma<-vcovHC(ma, type = "HC1")
  coeftest(ma, vcov = het_corr_ma)
  print(summary(ma))
  cat("----------------------------------------------------------------", "\n")
}
  
