###############################################################################################
# Intangible Value  
# by Andrea L. Eisfeldt, Edward T. Kim, and Dimitris Papanikolaou
#
# 1_getdata.R
#
# Compile CRSP and Compustat data, run perpetual inventory method, compute be_int
# Export intermediate panel dataset
#
# Last updated: May 2021
#
# References: Fama and French (1992, 1993)
#             http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_factors.html
#             Fama-French replication code adapted from WRDS (https://wrds-www.wharton.upenn.edu/login/?next=/pages/support/applications/python-replications/fama-french-factors-python/) 
#             and Wayne Chang (https://sites.google.com/site/waynelinchang/home)
###############################################################################################


###############################################################################################
# Initialization
 
rm(list=ls())

library(data.table)
library(tidyr)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(zoo)
library(lubridate)
library(foreign)
library(RPostgres)
library(sqldf)
library(getPass)
library(Hmisc)


###############################################################################################
### 0. Set-up
###############################################################################################

## WRDS connection
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  user='', #input user ID 
                  password=getPass(),
                  dbname='wrds',
                  sslmode='require')

## Custom fill function  
# In time series data, fill in NAs until n
# https://stackoverflow.com/questions/25940241/fill-na-in-a-time-series-only-to-a-limited-number
na_locf_until = function(x, n) {
  l <- cumsum(! is.na(x))
  c(NA, x[! is.na(x)])[replace(l, ave(l, l, FUN=seq_along) > (n+1), 0) + 1]
}


## Output directory
setwd("") #set directory 

###############################################################################################
### 1. Download raw data files
###############################################################################################

## Compustat data (fundamentals, annual)
res <- dbSendQuery(wrds, "select gvkey, cusip, datadate, fyr, fyear, sich, naicsh,
                        at, lt, txditc, txdb, itcb, seq, ceq, pstkrv, pstk, pstkl, gdwl,
                        xrd, xsga, xad, xlr, xint, intan, rdip, cogs, ib, indfmt, datafmt, popsrc, consol
                   from comp.funda
                   where indfmt='INDL' and datafmt='STD' and consol='C' and popsrc='D'")
data_comp_raw <- dbFetch(res, n=-1)
dbClearResult(res)

## CPI data for perpetual inventory method
res <- dbSendQuery(wrds, "select *
                    from crsp.tfz_mth_cpi")
data_cpi_prep <- dbFetch(res, n=-1) 
dbClearResult(res)

data_cpi <- data_cpi_prep %>% mutate(datadate = ceiling_date(as.Date(mcaldt,format = "%m/%d/%y"), "month") - days(1)) %>% select(datadate,tmcpiref)
  

## Compustat-CRSP link table 
res <- dbSendQuery(wrds, "select gvkey, lpermno, linkdt, linkenddt, linktype, linkprim
                    from crsp.ccmxpf_lnkhist")
data_ccmlink <- dbFetch(res, n=-1) 
dbClearResult(res)


## CRSP raw tables
res <- dbSendQuery(wrds, "select date, permno, permco, shrout, prc, hsiccd, 
                          ret, retx, vol
                   from crsp.msf")
data_crsp_msf <- dbFetch(res, n=-1) 
dbClearResult(res)

res <- dbSendQuery(wrds, "select date, permno, comnam, shrcd, exchcd
                   from crsp.mse")
data_crsp_mse <- dbFetch(res, n=-1)
dbClearResult(res)

res <- dbSendQuery(wrds, "select dlstdt, permno, dlret
                   from crspq.msedelist")
data_crsp_msedelist <- dbFetch(res, n = -1)
dbClearResult(res)

#xxxx remove later: merge 2021 monthly stock prices by permno (for most recent data)
#crsp_price_2021 <- read.csv("crsp_price_2021.csv")

###############################################################################################
### 2. Clean CRSP and COMPUSTAT data
###############################################################################################

###########################################
## Clean CRSP, create variables
###########################################

## CRSP prep 0
crsp_msf <- data_crsp_msf %>%
  filter(!is.na(prc)) %>%
  mutate(Date = as.yearmon(as.Date(date))) %>%
  select(-date)

crsp_mse <- data_crsp_mse %>%
  filter(!is.na(shrcd)) %>%
  mutate(Date = as.yearmon(as.Date(date))) %>%
  select(-date)

crsp_msedelist <- data_crsp_msedelist %>%
  filter(!is.na(dlret)) %>%
  mutate(Date = as.yearmon(as.Date(dlstdt))) %>%
  select(-dlstdt)

## CRSP prep 1
#merge raw tables
crsp_m_prep <- crsp_msf %>% 
  merge(crsp_mse, by=c("Date", "permno"), all.x=TRUE, allow.cartesian=TRUE) %>%
  merge(crsp_msedelist, by=c("Date", "permno"), all.x=TRUE, allow.cartesian=TRUE) %>%
  mutate(Date = as.Date(Date))

#identify and drop duplicate observations
crsp_dup <- crsp_m_prep %>%
  arrange(Date, permno) %>%
  group_by(Date, permno) %>%
  filter(n() > 1) %>%
  filter(exchcd < 4 & exchcd > 0) %>% #prioritize exchcd between 1 and 3
  group_by(Date, permno) %>%
  filter(n() > 1) %>%
  filter(row_number()==n()) #prioritize last observation

crsp_nodup <- crsp_m_prep %>%
  arrange(Date, permno) %>%
  group_by(Date, permno) %>%
  filter(n() ==1)

crsp_prep_final <- bind_rows(crsp_nodup,crsp_dup) %>% arrange(Date, permno) 

crsp_m <- lazy_dt(as.data.table(crsp_prep_final), immutable=FALSE) %>% 
  #convert to factors
  mutate_at(vars(permno, permco, shrcd, exchcd), funs(as.factor)) %>%
  #fill shrcd and exchcd with most recent available
  group_by(permno) %>%    
  mutate_at(vars(shrcd, exchcd), funs(na.locf(., na.rm=FALSE))) %>%
  ungroup() %>% 
  #filter shrcd and exchcd
  filter((shrcd == 10 | shrcd == 11) & (exchcd == 1 | exchcd == 2 | exchcd == 3)) %>% 
  #generate returns from ret, dlret if unavailable
  mutate(retadj=ifelse(!is.na(ret), ret, ifelse(!is.na(dlret), dlret, NA)),
         meq = (shrout * abs(prc))/1000) %>%
  #aggregate market equity up to the permco level
  group_by(Date, permco) %>%
  mutate(me = sum(meq)) %>% 
  slice(1) %>% 
  ungroup() %>%
  #keep pre-2019 only
  #filter(year(Date)<2019) %>%
  as.data.frame()

##initialize first and last date observations
crsp_m_bounds <- lazy_dt(as.data.table(crsp_m), immutable=FALSE) %>%
  group_by(permno) %>%
  summarise(first = first(Date), last = last(Date)) %>% 
  as.data.frame()

##prep and clean
crsp_m1 <- crsp_m %>%
  select(Date, permno, comnam, shrcd, exchcd, hsiccd, shrout, prc, vol, retx, retadj, me) %>% 
  #mutate(Date=as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="month"), permno) %>% 
  #drop firm-date observations outside bounds
  merge(crsp_m_bounds, by="permno") %>%
  filter(Date>=as.Date(first) & Date<=as.Date(last)) %>% 
  mutate(Date=ceiling_date(Date,"month") - days(1)) %>%
  arrange(permno, Date) %>%
  group_by(permno) %>%
  fill(`comnam`) %>%
  ungroup

crsp_clean <- lazy_dt(as.data.table(crsp_m1), immutable=FALSE) %>%
  mutate(permno = as.factor(permno)) %>% 
  group_by(permno) %>%
  mutate(#calculate portfolio weights
    port.weight = as.numeric(ifelse(!is.na(Lag(me,shift=1)), Lag(me,shift=1), me/(1+retx))), 
    port.weight = ifelse(is.na(retadj) & is.na(prc), NA, port.weight)) %>% 
  ungroup %>%  
  arrange(Date, permno) %>%   
  select(Date, permno, comnam:port.weight) %>% 
  as.data.frame()

#output standalone permno-sic code mapping for perpetual inventory method
crsp_sic <- crsp_clean %>% select(Date,permno,hsiccd)

#save(crsp_clean, file = "crsp_clean.RData")
#save(crsp_sic, file = "crsp_sic.RData")


###########################################
## Clean Compustat data
###########################################

## Merge Compustat to link table identifier
data_ccm <-  data_ccmlink %>%
  filter(linktype %in% c("LU", "LC", "LS")) %>%
  filter(linkprim %in% c("P", "C", "J")) %>%
  merge(data_comp_raw, by="gvkey") %>% 
  mutate(datadate = as.Date(datadate), 
         permno = as.numeric(lpermno),
         linkdt = as.Date(linkdt),
         linkenddt = as.Date(linkenddt),
         linktype = factor(linktype, levels=c("LC", "LU", "LS")),
         linkprim = factor(linkprim, levels=c("P", "C", "J"))) %>%
  filter(datadate >= linkdt & (datadate <= linkenddt | is.na(linkenddt))) %>%
  arrange(datadate, permno, linktype, linkprim) %>%
  distinct(datadate, permno, .keep_all = TRUE)

## Clean Compustat, create variables
comp_prep <- lazy_dt(data_ccm, immutable=TRUE)  %>%
  group_by(permno) %>% 
  mutate(gvkey = as.numeric(gvkey)) %>%
  ungroup %>% arrange(datadate, permno) %>% data.frame %>%
  distinct(datadate, permno, .keep_all = TRUE)

comp_prep_final <- comp_prep %>%
  group_by(permno) %>%
  merge(data_cpi, by='datadate', all.x=TRUE) %>%
  arrange(permno, datadate) %>%
  mutate(be = coalesce(seq, ceq + pstk, at - lt) + coalesce(txditc, txdb + itcb, 0) - 
           coalesce(pstkrv, pstkl, pstk, 0)) %>% 
  ungroup %>%
  arrange(datadate, permno) %>%
  select(datadate, permno, gvkey, fyear, fyr, at, ib, xrd, xint, sich, xsga, xad, intan, gdwl, rdip, cogs, xlr, tmcpiref, be) %>%
  mutate_if(is.numeric, list(~ifelse(is.infinite(.), NA, .))) %>% 
  mutate_if(is.numeric, list(~round(., 5))) %>% 
  group_by(permno) %>%
  complete(datadate = seq(min(datadate), max(datadate), by = "year")) %>%
  ungroup()

## Run perpetual inventory method program, which outputs ppint2.RData
source("2_gen_int.R")

## Create intangible capital variables 
comp_clean <- comp_prep_final %>%
  merge(ppint3, by.x=c("permno","datadate"),all.x=TRUE) %>%
  merge(crsp_sic, by.x=c("datadate","permno"), by.y=c("Date","permno"), all.x=TRUE) %>%
  mutate(sic = coalesce(sich,hsiccd)) %>%
  mutate(#set Compustat date to most recently available as of year t December, to be merged to t+1 June
         Date = ceiling_date(as.Date(as.yearmon(ymd(paste0(fyear+1,"-6-30"))),format = "%m/%d/%y"), "month") - days(1), 
         #intangible capital variables
         k_int_t100 = coalesce(int_d20_t100,0),
         k_int_t30 = coalesce(gt,0)+coalesce(int_d20_t30,0))

## Merge historical book equity data 
# http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_historical_be_data.html
# Download csv and import
data_hist_be <- read.csv("hist_be.csv")

data_hist_be[data_hist_be == -999 | data_hist_be == -99.99] <- NA
data_hist_be <- data_hist_be %>%
  mutate(permno = factor(PERMNO)) %>%
  data.table %>%
  select(-FirstYr, -LastYr) %>%
  gather(Date, hist_be, -permno, na.rm=TRUE) %>%
  mutate(Date = ceiling_date(as.Date(as.yearmon(ymd(paste0(substr(Date, 2, 5),"-6-30"))),format = "%m/%d/%y"), "month") - days(1))


###########################################
## Merge CRSP and Compustat
###########################################

crsp_comp <- comp_clean  %>%
  merge(crsp_clean, ., by=c("Date", "permno"), all.x=TRUE, allow.cartesian=TRUE) %>%  
  merge(data_hist_be, by=c("Date", "permno"), all.x=TRUE, allow.caresian=TRUE) %>%
  arrange(permno, Date, desc(datadate)) %>%
  distinct(permno, Date, .keep_all = TRUE) %>% #drop duplicates based on datadates 
  #fill in gaps with most recent available, up to 11 months
  group_by(permno) %>%
  mutate_at(vars(datadate:hist_be), list(~na_locf_until(., 11))) %>% #fill(datadate:hist_be) %>% #alternative method to fill without limit
  ungroup %>%
  mutate(datadate=as.Date(datadate)) %>%
  arrange(Date, permno)

#save(crsp_comp, file = "crsp_comp.RData")


###############################################################################################
### 3. Generate B/M variables
###############################################################################################

int_prep1 <- crsp_comp %>%
  arrange(permno,Date) %>%
  group_by(permno) %>%
  mutate(be = coalesce(be, hist_be), 
         be_int_t100 = coalesce(be,0) + coalesce(k_int_t100,0) - coalesce(gdwl,0),
         be_int_t30 = coalesce(be,0) + coalesce(k_int_t30,0) - coalesce(gdwl,0),
         tot_k_ep = coalesce(at,0) + coalesce(orgt_d15_t100,0), ### replication of EP factor (delta = .15, theta = 1)
         # Market equity (size)
         me_dec = as.numeric(ifelse(month(Date)==6 & Lag(me,shift=6)>0, Lag(me,shift=6), NA)), # previous Dec ME 
         me_jun = as.numeric(ifelse(month(Date)==6, me, NA)), 
         # EP (2013) OMK
         ok = as.numeric(ifelse(tot_k_ep>0, orgt_d15_t100/tot_k_ep, NA)), 
         # Book to market ratio
         bm = as.numeric(ifelse(month(Date)==6 & me_dec>0, be/me_dec, NA)), 
         bm_int_t100 = as.numeric(ifelse(month(Date)==6 & me_dec>0, be_int_t100/me_dec, NA)), 
         bm_int_t30 = as.numeric(ifelse(month(Date)==6 & me_dec>0, be_int_t30/me_dec, NA)), 
         ime_t100 = as.numeric(ifelse(month(Date)==6 & me_dec>0, k_int_t100/me_dec, NA)),
         ime_t30 = as.numeric(ifelse(month(Date)==6 & me_dec>0, k_int_t30/me_dec, NA)), 
         # Lagged main variables
         lag_me_jun = Lag(me_jun, shift=1), 
         lag_me_dec = Lag(me_dec, shift=1),
         lag_bm = Lag(bm, shift=1),
         lag_bm_int_t100 = Lag(bm_int_t100, shift=1),
         lag_bm_int_t30 = Lag(bm_int_t30, shift=1),
         lag_ime_t100 = Lag(ime_t100, shift=1),
         lag_ime_t30 = Lag(ime_t30, shift=1),
         lag_ok = Lag(ok, shift=1))
        
int_prep2 <- int_prep1 %>%
  mutate_at(vars(be:lag_ok), list(~ifelse(!is.infinite(.), ., NA))) %>% 
  select(Date, datadate, permno, gvkey, comnam, exchcd, sic, prc, vol, retadj, me, port.weight, 
         be, fyear:lag_ok) %>% 
  arrange(Date, permno) %>%
  group_by(permno) %>%
  mutate_at(vars(me_dec:lag_ok), list(~na_locf_until(., 11))) %>% #fill(me_dec:lag_ok) %>%
  ungroup %>%
  mutate(port.weight = ifelse(is.na(port.weight), 0, port.weight))


## Assign and merge industry classifications

#17-industry: https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_17_ind_port.html
ff_ind_17 <- read.csv("ff_ind_17.csv")
names(ff_ind_17) <- c("ind17","ind17_l","ind17_h")

#12-industry: https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/det_12_ind_port.html
ff_ind_12 <- read.csv("ff_ind_12.csv")
names(ff_ind_12) <- c("ind12","ind12_l","ind12_h")

int_ind12 = sqldf("select * from int_prep2
                     left join ff_ind_12
                     on int_prep2.sic between ff_ind_12.ind12_l and ff_ind_12.ind12_h")

int_prep_final = sqldf("select * from int_ind12
                     left join ff_ind_17
                     on int_ind12.sic between ff_ind_17.ind17_l and ff_ind_17.ind17_h")

int_prep_final$ind12 <- ifelse(!is.na(int_prep_final$ind12),int_prep_final$ind12,12) #12 is "Other"
int_prep_final$ind17 <- ifelse(!is.na(int_prep_final$ind17),int_prep_final$ind17,17) #17 is "Other'
#save(int_prep_final, file = "int_prep_final.RData")


###########################################
## Output panel dataset, cleaned
###########################################

int_xsec <- int_prep_final %>% 
  select(datadate, permno, gvkey, be, k_int_t100, k_int_t30, bm, bm_int_t100, bm_int_t30, ime_t100, ok) %>% 
  filter(year(datadate)>=1975 & year(datadate)<=2018) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  distinct(datadate, permno, gvkey, .keep_all=TRUE ) %>%
  mutate_if(is.numeric, round, digits = 4)

save(int_xsec, file = "int_xsec.RData")
write.csv(int_xsec, file = "int_xsec.csv", row.names=FALSE)

