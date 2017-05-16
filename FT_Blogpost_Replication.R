#This is replication code for the Fact Tank post:
# 5 facts about U.S. political donations
# by Adam Hughes
#


#Load libraries
library("tidyverse")
library("haven")

# set working directory
setwd("/Users/AHughes/workspace/donations_facttank/")

# there must be two folders in the working directory:
# data_PRC, which contains the American Trends Panel W21
# and data_ANES, which contains each individual
# time series study in .dta format, downloaded from: 
# http://www.electionstudies.org/studypages/download/datacenter_all_NoData.php

## Question Wording
# ANES: During an election year people are often asked to make 
# a contribution to support campaigns. Did you give money to an
# individual candidate running for public office? Did you give 
# money to a political party during this election year? Did you
# give any money to any other group that supported or opposed 
# candidates? 

# Pew: Here’s a list of activities some people do and others do 
# not. Please indicate if you have done each of the following 
# activities in the past year: Contributed money to a candidate 
# running for public office or to a group working to elect a 
# candidate. 


## The following function provides weighted totals by groups:
# This function was written by Andrew Mercer, Pew Research Center.
##
#' Get weighted percentages or totals
#'
#' Take a categorical variable and return the weighted or unweighted percentage or total of each category. Grouping variable optional. 
#' Can be used to compare weights versus one another.
#'
#' @param var A character string. The name of the variable of interest in your data frame
#' @param .data The name of your data frame. Do not pass as a character string.
#' @param wt A vector of strings. The name(s) of the weight variable(s) in your data frame. Unweighted by default.
#' @param by A character string. The name of the variable that you want to split the data frame by if you need to look at weighted frequencies by group. Optional.
#' @param percent TRUE/FALSE: Should the variable totals be scaled to percentages that sum to 100 or not?
#' @param include_unw TRUE/FALSE: When comparing multiple weights, should the unweighted frequencies be included or not?
#'
#' @return A tibble with two columns: \code{var} and \code{Freq}, the latter giving your weighted percentages or totals
#'
#' @import tidyverse
#' @export
get_totals = function(var, .data, wt=NULL, by=NULL, percent = TRUE, include_unw = FALSE) {
  
  if (include_unw) {
    .data$unw = 1
    wt = c(wt, "unw")
  }
  
  if (length(wt) > 1) {
    totals <- wt %>%
      map(~get_totals(var, .data, wt = ., by = by, percent = percent)) %>%
      map2(.y = wt, function(tab, w) {
        names(tab)[-1] =  sprintf("%s_%s", names(tab)[-1],  w); return(tab)}) %>%
      reduce(left_join, by = var)
    
    len_wt <- length(wt)
    len_by <- ifelse(!is.null(by), length(levels(.data[[by]])), 1)
    totals_seq <- c(1, unlist((1:len_by + 1) %>% map(~seq(.x, .x + (len_wt - 1) * len_by, len_by))))
    return(totals[totals_seq])
  }
  
  if (!is.null(by)) {
    tabs = .data %>% split(.[, by]) %>%
      map(~get_totals(var, ., wt=wt, percent=percent))
    return(map2(tabs, names(tabs), function(tab, name) {names(tab)[2] = name; return(tab)}) %>%
             reduce(full_join, by=var))
  }
  
  if (is.null(wt)) {
    .data$._wt = 1
    wt = "._wt"
  } else {
    .data$._wt = .data[, wt]
  }
  
  if (percent) {
    .data$._wt = 100 * .data$._wt/sum(.data$._wt)
  }
  
  .data %>%
    group_by_(var) %>%
    summarise("Freq"= sum(._wt))
}

######################################
### AMERICAN TRENDS PANEL ANALYSIS ###
######################################

# load the PRC American Trends Panel dataset from Wave 21
prc <- read_spss("Data_PRC/ATP W21.sav") 
# the following line preserves labels and can be used for EDA
# %>% mutate_if(is.labelled, as_factor)
get_totals("CIVIC_ENG_ACTMOD_H_W21", prc, wt = "WEIGHT_W21")

# examine donations by political engagement
prc$folgov_bin<-NA
prc$folgov_bin[which(prc$FOLGOV_W21==1)]<-1
prc$folgov_bin[which(prc$FOLGOV_W21==2)]<-0
prc$folgov_bin[which(prc$FOLGOV_W21==3)]<-0
prc$folgov_bin[which(prc$FOLGOV_W21==4)]<-0
as.data.frame(get_totals("CIVIC_ENG_ACTMOD_H_W21", prc, wt = "WEIGHT_W21", by = "folgov_bin"))
prc$oftvote_bin<-NA
prc$oftvote_bin[which(prc$OFTVOTE_W21==1)]<-1
prc$oftvote_bin[which(prc$OFTVOTE_W21==2)]<-1
prc$oftvote_bin[which(prc$OFTVOTE_W21==3)]<-0
prc$oftvote_bin[which(prc$OFTVOTE_W21==4)]<-0
as.data.frame(get_totals("CIVIC_ENG_ACTMOD_H_W21", prc, wt = "WEIGHT_W21", by = "oftvote_bin"))

# bin income and examine breaks by income
prc$inc_bin<-0
prc$inc_bin[which(prc$F_INCOME_FINAL<=3)]<-1
prc$inc_bin[which(prc$F_INCOME_FINAL>=4 & prc$F_INCOME_FINAL<=6)]<-2
prc$inc_bin[which(prc$F_INCOME_FINAL==7 | prc$F_INCOME_FINAL==8)]<-3
prc$inc_bin[which(prc$F_INCOME_FINAL==9)]<-4
as.data.frame(get_totals("CIVIC_ENG_ACTMOD_H_W21", prc, wt = "WEIGHT_W21", by = "inc_bin"))

# bin education and examine breaks by education
prc$hsorless<-0
prc$hsorless[which(prc$F_EDUCCAT2_FINAL<=2)]<-1
prc$hsorless[which(prc$F_EDUCCAT2_FINAL==3)]<-2
prc$hsorless[which(prc$F_EDUCCAT2_FINAL==4 | prc$F_EDUCCAT2_FINAL==5)]<-3
prc$hsorless[which(prc$F_EDUCCAT2_FINAL==6)]<-4

as.data.frame(get_totals("CIVIC_ENG_ACTMOD_H_W21", prc, wt = "WEIGHT_W21", by = "F_EDUCCAT2_FINAL"))
as.data.frame(get_totals("CIVIC_ENG_ACTMOD_H_W21", prc, wt = "WEIGHT_W21", by = "hsorless"))

# bin age and examine breaks by age
prc$age_bin<-NA
prc$age_bin[which(prc$F_AGE_FINAL>=18 & prc$F_AGE_FINAL<30)]<-1
prc$age_bin[which(prc$F_AGE_FINAL>=30 & prc$F_AGE_FINAL<50)]<-2
prc$age_bin[which(prc$F_AGE_FINAL>=50 & prc$F_AGE_FINAL<65)]<-3
prc$age_bin[which(prc$F_AGE_FINAL>=65 & prc$F_AGE_FINAL<95)]<-4
as.data.frame(get_totals("CIVIC_ENG_ACTMOD_H_W21", prc, wt = "WEIGHT_W21", by = "age_bin"))

# look at donation amounts
get_totals("CONTRHOWMUCH_W21", prc, wt = "WEIGHT_W21")
# look at income breaks only among those who donated
as.data.frame(get_totals("CONTRHOWMUCH_W21", prc %>% filter(!is.nan(CONTRHOWMUCH_W21)), wt = "WEIGHT_W21", by = "inc_bin"))

#################################
### ANES TIME SERIES ANALYSIS ###
#################################

# load the 1992 data
anes<-read_dta("Data_ANES/NES1992.dta")
# exclude those who didn't take the post election survey
anes<-subset(anes, V923007!=0)
anes$donate_cand<-NA
anes$donate_cand[which(anes$V900371==1)]<-1
anes$donate_cand[which(anes$V900371==5)]<-0
anes$donate_party<-NA
anes$donate_party[which(anes$V900373==1)]<-1
anes$donate_party[which(anes$V900373==5)]<-0
anes$donate_other<-NA
anes$donate_other[which(anes$V900375==1)]<-1
anes$donate_other[which(anes$V900375==5)]<-0

as.data.frame(get_totals("donate_cand", anes, wt = "V923009"))
as.data.frame(get_totals("donate_party", anes, wt = "V923009"))
as.data.frame(get_totals("donate_other", anes, wt = "V923009"))

# bin the partisans
anes$pid_bin<-NA
anes$pid_bin[which(anes$V900320==0 |anes$V900320==1 |anes$V900320==2)]<-1
anes$pid_bin[which(anes$V900320==3)]<-2
anes$pid_bin[which(anes$V900320==4 |anes$V900320==5 |anes$V900320==6)]<-3

# create a donate any variable for cand/party/other donations
anes$donate_any<-0
anes$donate_any[which(anes$donate_other==1 |
                        anes$donate_cand==1 |
                        anes$donate_party==1)]<-1

as.data.frame(get_totals("donate_any", anes, wt = "V923009"))
# 10.96 for any contribution
as.data.frame(get_totals("donate_any", anes, wt = "V923009", by="V900320"))
# 0 is strong dem (N=277), 3 is true ind (N=126), 6 is strong rep (N=122)
as.data.frame(get_totals("donate_any", anes, wt = "V923009", by="pid_bin"))


# load the 1996 data
anes<-read_dta("Data_ANES/NES96.dta")   
# drop those who did not participate in the post election interview
anes<-subset(anes, V961169!=0)
anes$donate_cand<-NA
anes$donate_cand[which(anes$V961169==1)]<-1
anes$donate_cand[which(anes$V961169==5)]<-0
anes$donate_party<-NA
anes$donate_party[which(anes$V961171==1)]<-1
anes$donate_party[which(anes$V961171==5)]<-0
anes$donate_other<-NA
anes$donate_other[which(anes$V961173==1)]<-1
anes$donate_other[which(anes$V961173==5)]<-0

as.data.frame(get_totals("donate_cand", anes, wt = "V960005B"))
as.data.frame(get_totals("donate_party", anes, wt = "V960005B"))
as.data.frame(get_totals("donate_other", anes, wt = "V960005B"))

# bin the partisans
anes$pid_bin<-NA
anes$pid_bin[which(anes$V960420==0 |anes$V960420==1 |anes$V960420==2)]<-1
anes$pid_bin[which(anes$V960420==3)]<-2
anes$pid_bin[which(anes$V960420==4 |anes$V960420==5 |anes$V960420==6)]<-3

# create a donate any variable for cand/party/other donations
anes$donate_any<-0
anes$donate_any[which(anes$donate_other==1 |
                        anes$donate_cand==1 |
                        anes$donate_party==1)]<-1

as.data.frame(get_totals("donate_any", anes, wt = "V960005B"))
# 10.96 for any contribution
as.data.frame(get_totals("donate_any", anes, wt = "V960005B", by = "V960420"))
# 0 is strong dem (N=298), 3 is true ind (N=125), 6 is strong rep (N=214)
as.data.frame(get_totals("donate_any", anes, wt = "V960005B", by = "pid_bin"))

# load the 2000 data
anes<-read_dta("Data_ANES/anes2000TS.dta") 
# drop those who did not participate in the post election interview
anes<-subset(anes, V001229!=0)
anes$donate_cand<-NA
anes$donate_cand[which(anes$V001229==1)]<-1
anes$donate_cand[which(anes$V001229==5)]<-0
anes$donate_party<-NA
anes$donate_party[which(anes$V001231==1)]<-1
anes$donate_party[which(anes$V001231==5)]<-0
anes$donate_other<-NA
anes$donate_other[which(anes$V001233==1)]<-1
anes$donate_other[which(anes$V001233==5)]<-0

as.data.frame(get_totals("donate_cand", anes, wt = "V000002a"))
as.data.frame(get_totals("donate_party", anes, wt = "V000002a"))
as.data.frame(get_totals("donate_other", anes, wt = "V000002a"))

# bin the partisans
anes$pid_bin<-NA
anes$pid_bin[which(anes$V000523==0 |anes$V000523==1 |anes$V000523==2)]<-1
anes$pid_bin[which(anes$V000523==3)]<-2
anes$pid_bin[which(anes$V000523==4 |anes$V000523==5 |anes$V000523==6)]<-3

# create a donate any variable for cand/party/other donations
anes$donate_any<-0
anes$donate_any[which(anes$donate_other==1 |
                        anes$donate_cand==1 |
                        anes$donate_party==1)]<-1

as.data.frame(get_totals("donate_any", anes, wt = "V000002a"))
# 10.92 for any contribution
as.data.frame(get_totals("donate_any", anes, wt = "V000002a", by = "V000523"))
# 0 is strong dem (N=297), 3 is true ind (N=168), 6 is strong rep (N=211)
as.data.frame(get_totals("donate_any", anes, wt = "V000002a", by = "pid_bin"))

# load the 2004 data
anes<-read_dta("Data_ANES/anes2004TS.dta")
anes$donate_cand<-NA
anes$donate_cand[which(anes$V045014==1)]<-1
anes$donate_cand[which(anes$V045014==5)]<-0
anes$donate_party<-NA
anes$donate_party[which(anes$V045015==1)]<-1
anes$donate_party[which(anes$V045015==5)]<-0
anes$donate_other<-NA
anes$donate_other[which(anes$V045016==1)]<-1
anes$donate_other[which(anes$V045016==5)]<-0

as.data.frame(get_totals("donate_cand", anes, wt = "V040102"))
as.data.frame(get_totals("donate_party", anes, wt = "V040102"))
as.data.frame(get_totals("donate_other", anes, wt = "V040102"))
 
# bin the partisans
anes$pid_bin<-NA
anes$pid_bin[which(anes$V043116==0 |anes$V043116==1 |anes$V043116==2)]<-1
anes$pid_bin[which(anes$V043116==3)]<-2
anes$pid_bin[which(anes$V043116==4 |anes$V043116==5 |anes$V043116==6)]<-3

# create a donate any variable for cand/party/other donations
anes$donate_any<-0
anes$donate_any[which(anes$donate_other==1 |
                        anes$donate_cand==1 |
                        anes$donate_party==1)]<-1

as.data.frame(get_totals("donate_any", anes, wt = "V040102"))
# 15.57 for any contribution
as.data.frame(get_totals("donate_any", anes, wt = "V040102", by = "V043116"))
# 0 is strong dem (N=203), 3 is true ind (N=118), 6 is strong rep (N=193)
as.data.frame(get_totals("donate_any", anes, wt = "V040102", by = "pid_bin"))


# load the 2008 data
anes<-read_dta("Data_ANES/anes_timeseries_2008.dta") 
# exclude those that didn't take wave 2
anes<-subset(anes, V085033!=-2)
anes$donate_cand<-NA
anes$donate_cand[which(anes$V085033==1)]<-1
anes$donate_cand[which(anes$V085033==5)]<-0
anes$donate_party<-NA
anes$donate_party[which(anes$V085034==1)]<-1
anes$donate_party[which(anes$V085034==5)]<-0
anes$donate_other<-NA
anes$donate_other[which(anes$V085035==1)]<-1
anes$donate_other[which(anes$V085035==5)]<-0

as.data.frame(get_totals("donate_cand", anes, wt = "V080102"))
as.data.frame(get_totals("donate_party", anes, wt = "V080102"))
as.data.frame(get_totals("donate_other", anes, wt = "V080102"))

# bin the partisans
anes$pid_bin<-NA
anes$pid_bin[which(anes$V083098x==0 |anes$V083098x==1 |anes$V083098x==2)]<-1
anes$pid_bin[which(anes$V083098x==3)]<-2
anes$pid_bin[which(anes$V083098x==4 |anes$V083098x==5 |anes$V083098x==6)]<-3

# create a donate any variable for cand/party/other donations
anes$donate_any<-0
anes$donate_any[which(anes$donate_other==1 |
                        anes$donate_cand==1 |
                        anes$donate_party==1)]<-1

as.data.frame(get_totals("donate_any", anes, wt = "V080102"))
# 14.90 for any contribution
as.data.frame(get_totals("donate_any", anes, wt = "V080102", by = "V083098x"))
# 0 is strong dem (N=529), 3 is true ind (N=237), 6 is strong rep (N=207)
as.data.frame(get_totals("donate_any", anes, wt = "V080102", by = "pid_bin"))


# load the 2012 data with labels via mutate
anes<-read_dta("Data_ANES/anes_timeseries_2012.dta") %>% mutate_if(is.labelled, as_factor)
# subset to FTF only
anes<-subset(anes, mode!="2. Internet mode")
# exclude those that didn't take wave 2
anes<-subset(anes, mobilpo_ctbcand!="-6. Not asked, unit nonresponse (no post-election interview)")
anes$donate_cand<-NA
anes$donate_cand[which(anes$mobilpo_ctbcand=="1. Yes")]<-1
anes$donate_cand[which(anes$mobilpo_ctbcand=="2. No")]<-0
anes$donate_party<-NA
anes$donate_party[which(anes$mobilpo_ctbpty=="1. Yes")]<-1
anes$donate_party[which(anes$mobilpo_ctbpty=="2. No")]<-0
anes$donate_other<-NA
anes$donate_other[which(anes$mobilpo_ctboth=="1. Yes")]<-1
anes$donate_other[which(anes$mobilpo_ctboth=="2. No")]<-0

as.data.frame(get_totals("donate_cand", anes, wt = "weight_ftf"))
as.data.frame(get_totals("donate_party", anes, wt = "weight_ftf"))
as.data.frame(get_totals("donate_other", anes, wt = "weight_ftf"))

# bin the partisans
anes$pid_bin<-NA
anes$pid_bin[which(anes$pid_x=="1. Strong Democrat" |anes$pid_x=="2. Not very strong Democract" |anes$pid_x=="3. Independent-Democrat")]<-1
anes$pid_bin[which(anes$pid_x=="4. Independent")]<-2
anes$pid_bin[which(anes$pid_x=="5. Independent-Republican" |anes$pid_x=="6. Not very strong Republican" |anes$pid_x=="7. Strong Republican")]<-3

# create a donate any variable for cand/party/other donations
anes$donate_any<-0
anes$donate_any[which(anes$donate_other==1 |
                        anes$donate_cand==1 |
                        anes$donate_party==1)]<-1

as.data.frame(get_totals("donate_any", anes, wt = "weight_ftf"))
# 12.16 for any contribution
as.data.frame(get_totals("donate_any", anes, wt = "weight_ftf", by = "pid_x"))
# 1 is strong dem (N=562), 4 is true ind (N=207), 7 is strong rep (N=163)
as.data.frame(get_totals("donate_any", anes, wt = "weight_ftf", by = "pid_bin"))

#load the 2016 data
anes<-read_dta("Data_ANES/anes_timeseries_2016.dta")
# NOTE: This is the MAY 1 Release
# subset to FTF only
anes<-subset(anes, V160501!=2)
# exclude those that didn't take wave 2
anes<-subset(anes, V162014!=-6)
anes$donate_cand<-NA
anes$donate_cand[which(anes$V162014==1)]<-1
anes$donate_cand[which(anes$V162014==2)]<-0
anes$donate_party<-NA
anes$donate_party[which(anes$V162016==1)]<-1
anes$donate_party[which(anes$V162016==2)]<-0
anes$donate_other<-NA
anes$donate_other[which(anes$V162017==1)]<-1
anes$donate_other[which(anes$V162017==2)]<-0

as.data.frame(get_totals("donate_cand", anes, wt = "V160102f"))
as.data.frame(get_totals("donate_party", anes, wt = "V160102f"))
as.data.frame(get_totals("donate_other", anes, wt = "V160102f"))

# bin the partisans
anes$pid_bin<-NA
anes$pid_bin[which(anes$V161158x==1 |anes$V161158x==2 |anes$V161158x==3)]<-1
anes$pid_bin[which(anes$V161158x==4)]<-2
anes$pid_bin[which(anes$V161158x==5 |anes$V161158x==6 |anes$V161158x==7)]<-3

# create a donate any variable for cand/party/other donations
anes$donate_any<-0
anes$donate_any[which(anes$donate_other==1 |
                        anes$donate_cand==1 |
                        anes$donate_party==1)]<-1

as.data.frame(get_totals("donate_any", anes, wt = "V160102f"))
# 14.99 for any contribution
as.data.frame(get_totals("donate_any", anes, wt = "V160102f", by = "V161158x"))
# 1 is strong dem (N=203), 4 is true ind (N=104), 7 is strong rep (N=158)
as.data.frame(get_totals("donate_any", anes, wt = "V160102f", by = "pid_bin"))


