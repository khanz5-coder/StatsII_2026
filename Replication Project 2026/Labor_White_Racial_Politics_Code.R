setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
file.rename("/Users/zahrita/Downloads/Downloads/Replication Project/Tex/Labor_White_Racial_Politics_Code.R", "/Users/zahrita/Documents/Github/Replication Project 2026/Labor_White_Racial_Politics_Code.R")

sink(file = "log.txt", append = FALSE, type = c("output", "message"),
     split = FALSE)

library(foreign)
library(readstata13)
library(plyr)
library(broom)
library(Matching)
library(rgenoud)
library(mediation)
library(reshape2)
library(stargazer)
library(haven)
library(ggplot2)
#library(dgo)

library("arm")
library("lme4")
library("car")
#library("Hmisc") 
library("LCFdata")
library("LMERConvenienceFunctions")
#library("nlme")
library("reshape2")
library("zoo")
library("DataCombine")
install.packages('interflex', type = "source", repos = 'http://cran.us.r-project.org') 

library('interflex')
#############CCES 2010-2014 Panel#################

data_long <- readRDS(file = "cces_panel_replication.rds")

###CCES Panel Analysis###

lm_1 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag, data_long[data_long$race_10==1,])
lm_2 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + factor(year), data_long[data_long$race_10==1,])
lm_3 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + faminc_10 + factor(gender_10) + age_10 + educ_10 + factor(year), data_long[data_long$race_10==1,])
lm_4 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + dem_id_2010 + faminc_10 + factor(gender_10) + age_10 + educ_10 + factor(year), data_long[data_long$race_10==1,])

stargazer(lm_1, lm_2, lm_3, lm_4, style="ajps", star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_racialresentment_cces_panel.html")
stargazer(lm_1, lm_2, lm_3, lm_4, type="latex", style="ajps", omit=c('year'),
          add.lines = list(
            c("Year Fixed Effects", "No", "Yes", "Yes", "Yes")
          ))

stargazer(lm_bv, lm_controls, lm_controls_fe,
          type="latex",
          omit = c("state_pre", "year"),
          add.lines = list(
            c("State Fixed Effects", "No", "Yes", "Yes"),
            c("Year Fixed Effects", "No", "No", "Yes")
          ),
          out="union_white_racialresentment_cces_new.tex")

#### MY CODE
lm_1 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag, data_long[data_long$race_10==1,])
lm_2 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + factor(year), data_long[data_long$race_10==1,])
lm_3 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + faminc_10 + factor(gender_10) + age_10 + educ_10 + factor(year) + age_10*union_gained, data_long[data_long$race_10==1,])
lm_4 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + dem_id_2010 + faminc_10 + factor(gender_10) + age_10 + educ_10 + factor(year) + age_10*union_gained, data_long[data_long$race_10==1,])


stargazer(lm_1, lm_2, lm_3, lm_4, type="latex", style="ajps", omit=c('year'),
          add.lines = list(
            c("Year Fixed Effects", "No", "Yes", "Yes", "Yes")
          ))

data_long$age_factorg <- cut(data_long$age_10,
                             breaks = c(40, 60, 80, 100),
                             include.lowest = T,
                             right = F)



str(data_long)
lm_1 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag, data_long[data_long$race_10==1,])
lm_2 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + factor(year), data_long[data_long$race_10==1,])
lm_3 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + faminc_10 + factor(gender_10) + age_factorg + educ_10 + factor(year) +union_gained*age_factorg, data_long[data_long$race_10==1,])
lm_4 <- lm(racial_resentment ~ union_gained + union_lost + racial_resentment_lag + dem_id_2010 + faminc_10 + factor(gender_10) + age_factorg + educ_10 + factor(year) +union_gained*age_factorg , data_long[data_long$race_10==1,])

stargazer(lm_1, lm_2, lm_3, lm_4, type="latex", style="ajps", omit=c('year'),
          add.lines = list(
            c("Year Fixed Effects", "No", "Yes", "Yes", "Yes")
          ))

#############################CCES Cross-Sectional###########################

data <- readRDS(file="cces_crosssectional_replication.RDS")

####Regressions

lm_bv <- lm(racial_resentment ~ union_member + past_union_member, data = data[data$race==1,])
lm_controls <- lm(racial_resentment ~ union_member + past_union_member + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])
lm_controls_fe <- lm(racial_resentment ~ union_member + past_union_member + factor(gender) + famincome + age + educ + factor(year) + factor(state_pre), data[data$race==1,])

stargazer(lm_bv, lm_controls, lm_controls_fe, style="ajps", star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_white_racialresentment_cces_new.html")
#Table One
stargazer(lm_bv, lm_controls, lm_controls_fe,
          type="latex",
          omit = c("state_pre", "year"),
          add.lines = list(
            c("State Fixed Effects", "No", "Yes", "Yes"),
            c("Year Fixed Effects", "No", "No", "Yes")
          ),
          out="union_white_racialresentment_cces_new.tex")

##my code

data <- data %>%
  mutate(age_binned = cut(age,
                          breaks = c(18, 30, 50, 70, Inf),
                          labels = c("18-30","31-50", "51-70", "70+"),
                          include.lowest = TRUE))

lm_bv <- lm(racial_resentment ~ union_member + past_union_member, data[data$race==1,])
lm_controls1 <- lm(racial_resentment ~ union_member + past_union_member + factor(gender) + famincome + age_binned + educ + factor(state_pre) + age_binned*union_member, data[data$race==1,])
lm_controls_fe1 <- lm(racial_resentment ~ union_member + past_union_member + factor(gender) + famincome + age_binned + educ + factor(year) + factor(state_pre) + age_binned*union_member, data[data$race==1,])

stargazer(lm_bv, lm_controls1, lm_controls_fe1,
          type="latex",
          omit = c("state_pre", "year"),
          add.lines = list(
            c("State Fixed Effects", "No", "Yes", "Yes"),
            c("Year Fixed Effects", "No", "No", "Yes")
          ))
stargazer(as.data.frame(anova(lm_controls, lm_controls1)))
anova(lm_controls_fe, lm_controls_fe1)

out <- interflex(
  estimator = "binning",
  data = data,
  Y = "racial_resentment",
  D = "union_member",
  X = "age",
  Z = c("educ", 'famincome', "state_pre", "year", "gender", "past_union_member"),
  nbins = 4,
  na.rm = TRUE,
  Ylabel = "Racial Resenment",
  Dlabel = "Union Membership",
  Xlabel = "Age"
)
plot(out)
plotdata <- rbind(tidy(lm_bv)[2,], tidy(lm_controls)[2,], tidy(lm_controls_fe)[2,])

plotdata$model <- c("Bivariate", "Controls", "Controls +\nYear FEs")

plotdata$cilow <- plotdata$estimate - (1.96*plotdata$std.error)
plotdata$cihigh <- plotdata$estimate + (1.96*plotdata$std.error)

pd <- position_dodge(.2)

 pdf("union_white_racialresentment_cces.pdf", h=4, w=4)
 ggplot(plotdata, aes(x=model, y=estimate)) +
   geom_point(size=2, color="black") +
   geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=0, color="black") +
   scale_color_manual(values=c("grey","black"), name="Model") +
   ylab("Union Effect on Racial Resentment") +
   xlab("Model") +
   geom_hline(yintercept=0, linetype=2) +
   theme_classic()
 dev.off()


##Affirmative Action CCES

lm_bv <- lm(affirm_action ~ union_member + past_union_member, data[data$race==1,])
lm_controls <- lm(affirm_action ~ union_member + past_union_member + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])
lm_controls_fe <- lm(affirm_action ~ union_member + past_union_member+ factor(gender) + famincome + age + educ + factor(year) + factor(state_pre), data[data$race==1,])

stargazer(lm_bv, lm_controls, lm_controls_fe, style="ajps", type= 'latex',
          omit = c("state_pre", "year"),
          add.lines = list(
            c("State Fixed Effects", "No", "Yes", "Yes"),
            c("Year Fixed Effects", "No", "No", "Yes")
          ))
lm_bv1 <- lm(affirm_action ~ union_member + past_union_member, data[data$race==1,])
lm_controls1 <- lm(affirm_action ~ union_member + past_union_member + factor(gender) + famincome + age + educ + factor(state_pre) + age*union_member, data[data$race==1,])
lm_controls_fe1 <- lm(affirm_action ~ union_member + past_union_member+ factor(gender) + famincome + age + educ + factor(year) + factor(state_pre) + age*union_member, data[data$race==1,])
anova(lm_controls1, lm_controls)
anova(lm_controls_fe, lm_controls_fe1)
stargazer(lm_bv1, lm_controls1, lm_controls_fe1, style="ajps", type= 'latex',
          omit = c("state_pre", "year"),
          add.lines = list(
            c("State Fixed Effects", "No", "Yes", "Yes"),
            c("Year Fixed Effects", "No", "No", "Yes")
          ))

plotdata <- rbind(tidy(lm_bv)[2,], tidy(lm_controls)[2,], tidy(lm_controls_fe)[2,])

plotdata$model <- c("Bivariate", "Controls", "Controls +\nYear FEs")

plotdata$cilow <- plotdata$estimate - (1.96*plotdata$std.error)
plotdata$cihigh <- plotdata$estimate + (1.96*plotdata$std.error)

pd <- position_dodge(.2)

# pdf("union_white_affaction_cces.pdf", h=4, w=4)
# ggplot(plotdata, aes(x=model, y=estimate)) +
#   geom_point(size=2, color="black") +
#   geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=0, color="black") +
#   #scale_color_manual(values=c("grey","black"), name="Model") +
#   #ylim(0,1) +
#   ylab("Union Effect on Affirmative Action Support") +
#   xlab("Model") +
#   geom_hline(yintercept=0, linetype=2) +
#   theme_classic()
# dev.off()


##By professional/non-professional

lm_bv <- lm(racial_resentment ~ union_member*laborer, data[data$race==1,])
lm_bv2 <- lm(racial_resentment ~ union_member*professional, data[data$race==1,])
lm_controls <- lm(racial_resentment ~ union_member*laborer + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])
lm_controls2 <- lm(racial_resentment ~ union_member*professional + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])
lm_controls_full <- lm(racial_resentment ~ union_member*laborer + union_member*professional + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])

stargazer(lm_bv, lm_bv2, lm_controls, lm_controls2, lm_controls_full, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_white_racialresentment_cces_industry.html")




###################Causal Mediation Analysis###############



med_data <- model.frame(racial_resentment ~ union_member + dem_id + gender + famincome + age + educ + year + state_pre, data[data$race==1,])

model_m <- lm(dem_id ~ union_member, med_data)
model_y <- lm(racial_resentment ~ union_member + dem_id, med_data)
mediation_model <- mediate(model.m=model_m, model.y=model_y, treat="union_member", mediator="dem_id", sims=1000)

gc()

i <- "racial_resentment"

output_bv <- rbind(data.frame(estimate=mediation_model$d1, cilow=mediation_model$d1.ci[1], cihigh=mediation_model$d1.ci[2], 
                              type="Party ID Mediator", model="Bivariate"),
                   data.frame(estimate=mediation_model$z1, cilow=mediation_model$z1.ci[1], cihigh=mediation_model$z1.ci[2], 
                              type="Union (Direct Effect)", model="Bivariate"),
                   data.frame(estimate=mediation_model$tau.coef, cilow=mediation_model$tau.ci[1], cihigh=mediation_model$tau.ci[2], 
                              type="Union + Party ID Mediator", model="Bivariate"))



model_m <- lm(dem_id ~ union_member + factor(gender) + famincome + age + educ + factor(year) + factor(state_pre), med_data)
model_y <- lm(racial_resentment ~ union_member + dem_id + factor(gender) + famincome + age + educ + factor(year) + factor(state_pre), med_data)
mediation_model <- mediate(model.m=model_m, model.y=model_y, treat="union_member", mediator="dem_id", sims=1000)

gc()

i <- "racial_resentment"

output_full <- rbind(data.frame(estimate=mediation_model$d1, cilow=mediation_model$d1.ci[1], cihigh=mediation_model$d1.ci[2], 
                                type="Party ID Mediator", model="Full"),
                     data.frame(estimate=mediation_model$z1, cilow=mediation_model$z1.ci[1], cihigh=mediation_model$z1.ci[2], 
                                type="Union (Direct Effect)", model="Full"),
                     data.frame(estimate=mediation_model$tau.coef, cilow=mediation_model$tau.ci[1], cihigh=mediation_model$tau.ci[2], 
                                type="Union + Party ID Mediator", model="Full"))


write.csv(rbind(output_bv, output_full), "rr_mediation_cces.csv", na="")


pd <- position_dodge(.2)

pdf("rr_partyid_mediation_cces.pdf", h=4, w=4)
ggplot(rbind(output_bv, output_full), aes(x=type, y=estimate, color=model)) +
  geom_point(size=2, position=pd) +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=0, position=pd) +
  scale_color_manual(values=c("grey","black"), name="Model") +
  #ylim(0,1) +
  ylab("Union Effect on Affirmative Action Support") +
  xlab("Model") +
  geom_hline(yintercept=0, linetype=2) +
  theme_classic()
dev.off()



##############CCES Appendix Plots############## 

pd <- position_dodge(.5)

data$racial_resent_special_favors_rev <- (data$racial_resent_special_favors*-1)+6

data$racial_resentment_10 <- data$racial_resent_special_favors_rev + data$racial_resent_slavery

slice_1 <- data[data$race==1,c("union_member", "racial_resent_slavery")]
slice_1$question <- "Slavery &\nDiscrimination"
names(slice_1)[2] <- "est"

slice_2 <- data[data$race==1,c("union_member", "racial_resent_special_favors_rev")]
slice_2$question <- "Special Favors"
names(slice_2)[2] <- "est"

plotdata <- rbind(slice_1, slice_2)


pd <- position_dodge(.6)

pdf("racial_resentment_hist_cces.pdf", h=4, w=5)
ggplot(plotdata[!is.na(plotdata$union_member),], aes(x=est, y = ..density.., color=factor(union_member), fill=factor(union_member))) +
  geom_histogram(position=pd, binwidth=1) +
  #geom_density() +
  scale_fill_manual(values=c("grey","black"), name="Union\nMember", labels=c("No","Yes")) +
  scale_color_manual(values=c("black","black")) +
  guides(color=F) +
  facet_wrap(~question, dir="v") +
  ylab("Density") +
  xlab("Response (Higher = Conservative)") +
  theme_classic()
dev.off()


########Right to Work vs non RTW states###########

rtw <- read.dta("right_to_work_laws.dta", convert.factors = F)

names(rtw)[names(rtw)=="state_fips"] <- "state_pre"

data <- join(data, rtw)


##By RTW state
lm_bv <- lm(racial_resentment ~ union_member*labor_grtw, data[data$race==1,])
lm_bv2 <- lm(racial_resentment ~ union_member*labor_grtw, data[data$race==1,])
lm_controls <- lm(racial_resentment ~ union_member*laborer + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])
lm_controls2 <- lm(racial_resentment ~ union_member*labor_grtw + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])
lm_controls_full <- lm(racial_resentment ~ union_member*laborer + union_member*labor_grtw + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1,])

stargazer(lm_bv, lm_bv2, lm_controls, lm_controls2, lm_controls_full, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_white_racialresentment_cces_RTW.html")






##########################################################################################################################################################################
##########################################################################################################################################################################
##################################Matching###############################################################################################################################################################################################################
# ##########################################################################################################################################################################
##########################################################################################################################################################################

#Caution: This analysis has a long run time


########CCES###########

matchdata <- na.omit(data[data$race==1,c("union_member","past_union_member","state_pre","year","educ","famincome","gender","race","age",
                                        "racial_resentment")])

exact_vars <- c(FALSE, FALSE, FALSE, TRUE,
                TRUE,
                TRUE)

#save(gen, file=paste("genmatch_", j, "_", y, "_18.12.8.RData", sep=""))


#######union_member#######

gen = GenMatch(Tr=matchdata$union_member,X=with(matchdata,
                                                cbind(famincome, educ, age, gender,
                                                      year,
                                                      state_pre
                                                )),
               exact=exact_vars,
               max.generations = 1000, pop.size=1000,
               #cluster=cl,
               verbose=T)
gc()

###Now Run the Match
matchout = Match(Y=matchdata$racial_resentment, Tr=matchdata$union_member,X=with(matchdata,
                                                                             cbind(famincome, educ, age, gender,
                                                                                   year,
                                                                                   state_pre
                                                                             )),
                 exact=exact_vars, Weight.matrix=gen,
                 ties=T)
gc()
summary(matchout)

rm(gen); gc()

write.csv(c(as.numeric(matchout$est), as.numeric(matchout$se), as.numeric(matchout$nobs)), "matching_results_rr_cces.csv")

mb  <- MatchBalance(union_member~famincome+ educ+ age+ gender+ year +
                      state_pre, data=matchdata, match.out=matchout, nboots=1000)

output <- list()

for(i in 1:6){

  temp <- data.frame(cbind(mb$BeforeMatching[[i]]$mean.Co, mb$BeforeMatching[[i]]$mean.Tr,
                           mb$AfterMatching[[i]]$mean.Co, mb$AfterMatching[[i]]$mean.Tr, mb$AfterMatching[[i]]$p.value, c("inc", "edu", "age", "sex",
                                                                                                                          "year", "state")[i]))
  names(temp) <- c("bm_control", "bm_treated", "am_control", "am_treated", "ttest_pval", "variable")
  output[[i]] <- temp
}

balance_stats <- do.call(rbind, output)
gc()

write.csv(balance_stats, "matching_balance_rr_cces.csv")



#######past_union_member#######

gen = GenMatch(Tr=matchdata$past_union_member,X=with(matchdata,
                                                cbind(famincome, educ, age, gender,
                                                      year,
                                                      state_pre
                                                )),
               exact=exact_vars,
               max.generations = 1000, pop.size=1000,
               #cluster=cl,
               verbose=T)
gc()

###Now Run the Match
matchout = Match(Y=matchdata$racial_resentment, Tr=matchdata$past_union_member,X=with(matchdata,
                                                                                 cbind(famincome, educ, age, gender,
                                                                                       year,
                                                                                       state_pre
                                                                                 )),
                 exact=exact_vars, Weight.matrix=gen,
                 ties=T)
gc()
summary(matchout)

rm(gen); gc()

write.csv(c(as.numeric(matchout$est), as.numeric(matchout$se), as.numeric(matchout$nobs)), "matching_results_rr_cces_pastmember.csv")

mb  <- MatchBalance(past_union_member~famincome+ educ+ age+ gender+ year +
                      state_pre, data=matchdata, match.out=matchout, nboots=1000)

output <- list()

for(i in 1:6){

  temp <- data.frame(cbind(mb$BeforeMatching[[i]]$mean.Co, mb$BeforeMatching[[i]]$mean.Tr,
                           mb$AfterMatching[[i]]$mean.Co, mb$AfterMatching[[i]]$mean.Tr, mb$AfterMatching[[i]]$p.value, c("inc", "edu", "age", "sex",
                                                                                                                          "year", "state")[i]))
  names(temp) <- c("bm_control", "bm_treated", "am_control", "am_treated", "ttest_pval", "variable")
  output[[i]] <- temp
}

balance_stats <- do.call(rbind, output)
gc()

write.csv(balance_stats, "matching_balance_rr_cces_pastmember.csv")





#####################################################################################
##################################Effect on Dem Party ID by Year################
#####################################################################################

anes <- readRDS(file="anes_replication.RDS")

###Loop over years###
output_anes <- list()

for(i in seq(1964,2016, by=4)){
  
  cat(i, "\n")
  
  temp_bv <- tidy(lm(dem_id ~ union_member, anes[anes$year==i & anes$eth==1,]))[2,]
  temp_bv$years <- as.character(i)
  temp_bv$model <- "Bivariate"
  
  temp_full <- tidy(lm(dem_id ~ union_member + sex + factor(reg) + factor(agegroup) + inc + edu, anes[anes$year==i & anes$eth==1,]))[2,]
  temp_full$years <- as.character(i)
  temp_full$model <- "Full"
  
  output_anes[[i]] <- rbind(temp_bv, temp_full)
  
}


data <- readRDS(file="cces_crosssectional_replication.RDS")

output_cces <- list()

for(i in c(2006,2010,2012)){
  cat(i, "\n")
  
  temp_bv <- tidy(lm(dem_id ~ union_member, data[data$race==1 & data$year==i,]))[2,]
  temp_bv$years <- as.character(i)
  temp_bv$model <- "Bivariate"
  
  temp_full <- tidy(lm(dem_id ~ union_member + factor(gender) + famincome + age + educ + factor(state_pre), data[data$race==1 & data$year==i,]))[2,]
  temp_full$years <- as.character(i)
  temp_full$model <- "Full"
  
  output_cces[[i]] <- rbind(temp_bv, temp_full)
  
}

output_anes <- do.call(rbind, output_anes)
output_anes$data <- "ANES"
output_cces <- do.call(rbind, output_cces)
output_cces$data <- "CCES"

plotdata <- rbind(output_anes, output_cces)

plotdata$cilow <- plotdata$estimate - (1.96*plotdata$std.error)
plotdata$cihigh <- plotdata$estimate + (1.96*plotdata$std.error)

pd <- position_dodge(.4)

#Figure A1
pdf("union_white_dem_id_byyear.pdf", h=4)
ggplot(plotdata, aes(x=years, y=estimate, color=model, shape=data)) +
  geom_point(size=2, position=pd) +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=0, position=pd) +
  scale_color_manual(values=c("grey","black"), name="Model") +
  scale_shape_manual(values=c(16, 15), name="Survey") +
  #ylim(0,1) +
  ylab("Union Effect on Party ID (0-1 Scale)") +
  xlab("Year") +
  geom_hline(yintercept=0, linetype=2) +
  theme_classic()
dev.off()





####################################################################################################################
########################################################VSG#########################################################
################################################Voter Study Group Data#########################################################
####################################################################################################################


vsg <- readRDS(file="vsg_replication.RDS")

#######Regressions########

#separating gaining/losing union
lm_lag <- lm(racial_resentment_2016 ~ union_gained + union_lost + racial_resentment_baseline, vsg[vsg$race_2016==1,])
lm_change <- lm(racial_resentment_change ~ union_gained + union_lost, vsg[vsg$race_2016==1,])
lm_lag_controls <- lm(racial_resentment_2016 ~ union_gained + union_lost + racial_resentment_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls <- lm(racial_resentment_change ~ union_gained + union_lost + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_lag_controls_pid <- lm(racial_resentment_2016 ~ union_gained + union_lost + racial_resentment_baseline + dem_id_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls_pid <- lm(racial_resentment_change ~ union_gained + union_lost + dem_id_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])

stargazer(list(lm_lag, lm_change, lm_lag_controls, lm_change_controls),
          type = "latex")

stargazer(list(lm_change_controls_pid,lm_lag_controls_pid),
          type = "latex")


#black thermometer
lm_lag <- lm(ft_black_2016 ~ union_gained + union_lost + blacks_t_baseline, vsg[vsg$race_2016==1,])
lm_lag_controls <- lm(ft_black_2016 ~ union_gained + union_lost + blacks_t_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_lag_controls_pid <- lm(ft_black_2016 ~ union_gained + union_lost + blacks_t_baseline + dem_id_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])

stargazer(lm_lag, lm_lag_controls, lm_lag_controls_pid, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_change_blacktherm_gainedlost_vsg.html")




#affirmative action
lm_lag <- lm(affirmact_gen_2016 ~ union_gained + union_lost + affirmact_gen_baseline, vsg[vsg$race_2016==1,])
lm_change <- lm(affirmact_change ~ union_gained + union_lost, vsg[vsg$race_2016==1,])
lm_lag_controls <- lm(affirmact_gen_2016 ~ union_gained + union_lost + affirmact_gen_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls <- lm(affirmact_change ~ union_gained + union_lost + faminc_baseline + factor(gender_baseline) + age_baseline, vsg[vsg$race_2016==1,])
lm_lag_controls_pid <- lm(affirmact_gen_2016 ~ union_gained + union_lost + affirmact_gen_baseline + dem_id_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls_pid <- lm(affirmact_change ~ union_gained + union_lost + dem_id_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])

stargazer(lm_lag, lm_change, lm_lag_controls, lm_change_controls, lm_lag_controls_pid, lm_change_controls_pid, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_change_affaction_gainedlost_vsg.html")


###Alternative Collapsed treatment
lm_lag <- lm(racial_resentment_2016 ~ union_change + racial_resentment_baseline, vsg[vsg$race_2016==1,])
lm_change <- lm(racial_resentment_change ~ union_change, vsg[vsg$race_2016==1,])
lm_lag_controls <- lm(racial_resentment_2016 ~ union_change + racial_resentment_baseline + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls <- lm(racial_resentment_change ~ union_change + faminc_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])

stargazer(lm_lag, lm_change, lm_lag_controls, lm_change_controls, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_change_racialresentment_vsg.html")


#Alternative Mechanism: Income Change
lm_lag <- lm(racial_resentment_2016 ~ faminc_change + racial_resentment_baseline, vsg[vsg$race_2016==1,])
lm_change <- lm(racial_resentment_change ~ faminc_change, vsg[vsg$race_2016==1,])
lm_lag_controls <- lm(racial_resentment_2016 ~ faminc_change + racial_resentment_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls <- lm(racial_resentment_change ~ faminc_change + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_lag_controls_pid <- lm(racial_resentment_2016 ~ faminc_change + racial_resentment_baseline + dem_id_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls_pid <- lm(racial_resentment_change ~ faminc_change + dem_id_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])

stargazer(lm_lag, lm_change, lm_lag_controls, lm_change_controls, lm_lag_controls_pid, lm_change_controls_pid, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="income_change_racialresentment_vsg.html")


###Placebo: Abortion liberalism###

vsg$abortview3_baseline[vsg$abortview3_baseline==8] <- NA
vsg$abortview3_2016[vsg$abortview3_2016==8] <- NA

for(i in c("abortview3_baseline", "abortview3_2016")){
  cat(i, "\n")
  vsg[,i] <- -1*((vsg[,i] - min(vsg[,i], na.rm=T)) / (max(vsg[,i], na.rm=T) - min(vsg[,i], na.rm=T)))
  
}

vsg$abortion_change <- vsg$abortview3_2016 - vsg$abortview3_baseline

lm_lag <- lm(abortview3_2016 ~ union_gained + union_lost + abortview3_baseline, vsg[vsg$race_2016==1,])
lm_change <- lm(abortion_change ~ union_gained + union_lost, vsg[vsg$race_2016==1,])
lm_lag_controls <- lm(abortview3_2016 ~ union_gained + union_lost + abortview3_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls <- lm(abortion_change ~ union_gained + union_lost + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_lag_controls_pid <- lm(abortview3_2016 ~ union_gained + union_lost + abortview3_baseline + dem_id_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls_pid <- lm(abortion_change ~ union_gained + union_lost + dem_id_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])

stargazer(lm_lag, lm_change, lm_lag_controls, lm_change_controls, lm_lag_controls_pid, lm_change_controls_pid, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_change_placebo_abortionliberalism_vsg.html")



###Placebo: Social liberalism
library(ltm)

vsg$abort_baseline_binary[vsg$abortview3_baseline>-1] <- 1
vsg$abort_baseline_binary[vsg$abortview3_baseline==-1] <- 0
vsg$abort_2016_binary[vsg$abortview3_2016>-1] <- 1
vsg$abort_2016_binary[vsg$abortview3_2016==-1] <- 0

vsg$envwarm_baseline[vsg$envwarm_baseline==1] <- 1
vsg$envwarm_baseline[vsg$envwarm_baseline>1 & vsg$envwarm_baseline<5] <- 2

vsg$envwarm_2016[vsg$envwarm_2016==1] <- 1
vsg$envwarm_2016[vsg$envwarm_2016>1 & vsg$envwarm_2016<5] <- 2

social_liberalism_vars_baseline <- c("deathpenalty_baseline", "immi_naturalize_baseline", "gaymar2_baseline", 
                                     "envwarm_baseline", "abort_baseline_binary")
social_liberalism_vars_2016 <- c("deathpen_2016", "immi_naturalize_2016","gaymar_2016", "envwarm_2016", "abort_2016_binary")

for(i in c(social_liberalism_vars_baseline, social_liberalism_vars_2016)){
  cat(i, "\n")
  
  vsg$temp <- vsg[,i]
  vsg$temp[vsg$temp>2] <- NA
  vsg[,i] <- vsg$temp
  
  vsg[,i] <- -1*vsg[,i]
  vsg[,i] <- ((vsg[,i] - min(vsg[,i], na.rm=T)) / (max(vsg[,i], na.rm=T) - min(vsg[,i], na.rm=T)))
  
}


#Scores (higher values = liberal)
irt_output_baseline <- ltm(vsg[,social_liberalism_vars_baseline] ~ z1)
irt_output_2016 <- ltm(vsg[,social_liberalism_vars_2016] ~ z1)

vsg <- join(vsg, factor.scores(irt_output_baseline)$score.dat)
names(vsg)[names(vsg)=="z1"] <- "irt_score_baseline"
vsg$Obs <- NULL
vsg$Exp <- NULL
vsg$se.z1 <- NULL

vsg <- join(vsg, factor.scores(irt_output_2016)$score.dat)
names(vsg)[names(vsg)=="z1"] <- "irt_score_2016"

for(i in c("irt_score_baseline","irt_score_2016")){
  vsg[,i] <- 1-((vsg[,i] - min(vsg[,i], na.rm=T)) / (max(vsg[,i], na.rm=T) - min(vsg[,i], na.rm=T)))
}

vsg$irt_change <- vsg$irt_score_2016 - vsg$irt_score_baseline


lm_lag <- lm(irt_score_2016 ~ union_gained + union_lost + irt_score_baseline, vsg[vsg$race_2016==1,])
lm_change <- lm(irt_change ~ union_gained + union_lost, vsg[vsg$race_2016==1,])
lm_lag_controls <- lm(irt_score_2016 ~ union_gained + union_lost + irt_score_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls <- lm(irt_change ~ union_gained + union_lost + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_lag_controls_pid <- lm(irt_score_2016 ~ union_gained + union_lost + irt_score_baseline + dem_id_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])
lm_change_controls_pid <- lm(irt_change ~ union_gained + union_lost + dem_id_baseline + factor(gender_baseline) + educ_baseline + age_baseline, vsg[vsg$race_2016==1,])

stargazer(lm_lag, lm_change, lm_lag_controls, lm_change_controls, lm_lag_controls_pid, lm_change_controls_pid, style="ajps", #star.char=c("***","**","*","+"), 
          star.cutoffs = c(0.05,0.01,0.001),
          out="union_change_placebo_socialliberalism_vsg.html")






####################################################################################################################
#############################Racially Targeted Policy########################################################
####################################################################################################################

data <- readRDS(file= "anes_racial_policy_replication.RDS")


#########Regression: Racial Resentment Among Whites#######

lm_bv <- lm(racial_resentment ~ union_member, data[data$eth==1,])
lm_controls <- lm(racial_resentment ~ union_member + sex + factor(reg) + age + inc + edu, data[data$eth==1,])
lm_controls_fe <- lm(racial_resentment ~ union_member + sex + factor(reg) + age + inc + edu + factor(year), data[data$eth==1,])

stargazer(lm_bv, lm_controls, lm_controls_fe,
          type="latex",
          omit = c("reg", "year"),
          add.lines = list(
            c("Region Fixed Effects", "No", "Yes", "Yes"),
            c("Year Fixed Effects", "No", "No", "Yes")
          ),
          out="union_white_racialresentment_cces_new.tex")



temp_bv <- tidy(lm(racial_resentment ~ union_member, data[data$eth==1,]))[2,]
temp_controls <- tidy(lm(racial_resentment ~ union_member + sex + factor(reg) + age + inc + edu, data[data$eth==1,]))[2,]
temp_controls_fe <- tidy(lm(racial_resentment ~ union_member + sex + factor(reg) + age + inc + edu + factor(year), data[data$eth==1,]))[2,]

plotdata <- rbind(temp_bv, temp_controls, temp_controls_fe)

plotdata$model <- c("Bivariate", "Controls", "Controls +\nYear FEs")

plotdata$cilow <- plotdata$estimate - (1.96*plotdata$std.error)
plotdata$cihigh <- plotdata$estimate + (1.96*plotdata$std.error)

pd <- position_dodge(.2)

pdf("union_white_racialresentment_anes.pdf", h=4, w=4)
ggplot(plotdata, aes(x=model, y=estimate)) +
  geom_point(size=2, color="black") +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=0, color="black") +
  #scale_color_manual(values=c("grey","black"), name="Model") +
  #ylim(0,1) +
  ylab("Union Effect on Racial Resentment") +
  xlab("Model") +
  geom_hline(yintercept=0, linetype=2) +
  theme_classic()
dev.off()




##black policy

lm_bv <- lm(black_policy ~ union_member, data[data$eth==1,])
lm_controls <- lm(black_policy ~ union_member + sex + factor(reg) + age + inc + edu, data[data$eth==1,])
lm_controls_fe <- lm(black_policy ~ union_member + sex + factor(reg) + age + inc + edu + factor(year), data[data$eth==1,])

stargazer(lm_bv, lm_controls, lm_controls_fe,
          type="latex",
          omit = c("reg", "year"),
          add.lines = list(
            c("Region Fixed Effects", "No", "Yes", "Yes"),
            c("Year Fixed Effects", "No", "No", "Yes")
          ),
          out="union_white_racialresentment_cces_new.tex")



temp_bv <- tidy(lm(black_policy ~ union_member, data[data$eth==1,]))[2,]
temp_controls <- tidy(lm(black_policy ~ union_member + sex + factor(reg) + age + inc + edu, data[data$eth==1,]))[2,]
temp_controls_fe <- tidy(lm(black_policy ~ union_member + sex + factor(reg) + age + inc + edu + factor(year), data[data$eth==1,]))[2,]

plotdata <- rbind(temp_bv, temp_controls, temp_controls_fe)

plotdata$model <- c("Bivariate", "Controls", "Controls +\nYear FEs")

plotdata$cilow <- plotdata$estimate - (1.96*plotdata$std.error)
plotdata$cihigh <- plotdata$estimate + (1.96*plotdata$std.error)

pd <- position_dodge(.2)

pdf("union_white_blackpolicy_anes.pdf", h=4, w=4)
ggplot(plotdata, aes(x=model, y=estimate)) +
  geom_point(size=2, color="black") +
  geom_errorbar(aes(ymin=cilow, ymax=cihigh), width=0, color="black") +
  #scale_color_manual(values=c("grey","black"), name="Model") +
  #ylim(0,1) +
  ylab("Union Effect on Racial Policy Attitudes") +
  xlab("Model") +
  geom_hline(yintercept=0, linetype=2) +
  theme_classic()
dev.off()






