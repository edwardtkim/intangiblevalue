###############################################################################################
# Intangible Value 
# by Andrea L. Eisfeldt, Edward Kim, and Dimitris Papanikolaou
#
# 3_gen_int_factors.R
#
# Create value (HML) factors using intangibles-adjusted book to market equity
#
# Last updated: Nov 2020
#
###############################################################################################


################################################################################################    
## HML^INT, baseline

  main_clean_t100 <- int_prep_final %>%
    select(Date, permno, exchcd, sich, lag_bm, lag_bm_int_t100, lag_me_jun, lag_me_dec, port.weight, retadj)
  
  bp_t100 <- main_clean_t100 %>% 
    filter(exchcd == 1 & lag_bm > 0 & lag_bm_int_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>%
    group_by(Date) %>%
    summarise(bm_p70 = quantile(lag_bm_int_t100, probs=.7, na.rm=TRUE),
              bm_p30 = quantile(lag_bm_int_t100, probs=.3, na.rm=TRUE),
              size_med = quantile(lag_me_jun, probs=.5, na.rm=TRUE))

  main_pf_t100 <- main_clean_t100 %>%
    merge(bp_t100, by="Date", all.x=TRUE) %>%
    mutate(size = ifelse(lag_me_jun < size_med, "small", "big"),
           bm = ifelse(lag_bm_int_t100 < bm_p30, "low", ifelse(lag_bm_int_t100 > bm_p70, "high", "na")),
           pf = paste(size,bm,sep="_"))
  
  ret_t100 <- main_pf_t100 %>% 
    group_by(Date, pf) %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_bm_int_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    summarise(ret_pf = weighted.mean(retadj, port.weight, na.rm=TRUE)) %>% 
    spread(pf, ret_pf) %>% 
    mutate(high = (small_high + big_high)/2,
           low = (small_low + big_low)/2,
           hml_int_t100 = high - low) %>%
    select(Date, hml_int_t100)

  
################################################################################################    
## HML^INT, alternative (theta=0.3)
  
  main_clean_t30 <- int_prep_final %>%
    select(Date, permno, exchcd, sich, lag_bm, lag_bm_int_t30, lag_me_jun, lag_me_dec, port.weight, retadj)
  
  bp_t30 <- main_clean_t30 %>% 
    filter(exchcd == 1 & lag_bm > 0 & lag_bm_int_t30 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date) %>%
    summarise(bm_p70 = quantile(lag_bm_int_t30, probs=.7, na.rm=TRUE),
              bm_p30 = quantile(lag_bm_int_t30, probs=.3, na.rm=TRUE),
              size_med = quantile(lag_me_jun, probs=.5, na.rm=TRUE))

  main_pf_t30 <- main_clean_t30 %>%
    merge(bp_t30, by="Date", all.x=TRUE) %>%
    mutate(size = ifelse(lag_me_jun < size_med, "small", "big"),
           bm = ifelse(lag_bm_int_t30 < bm_p30, "low", ifelse(lag_bm_int_t30 > bm_p70, "high", "na")),
           pf = paste(size,bm,sep="_")) 
  
  ret_t30 <- main_pf_t30 %>% 
    group_by(Date, pf) %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_bm_int_t30 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    summarise(ret_pf = weighted.mean(retadj, port.weight, na.rm=TRUE)) %>% 
    spread(pf, ret_pf) %>% 
    mutate(high = (small_high + big_high)/2,
           low = (small_low + big_low)/2,
           hml_int_t30 = high - low) %>%
    select(Date, hml_int_t30)
  
  
################################################################################################    
## OMK factor

# Initialize dataset, filters based on Eisfeldt and Papanikolaou (2013)
ok_prep_final <- int_prep_final %>% 
    filter(!is.na(sic)) %>%
    filter(as.numeric(sic) < 6000 | as.numeric(sic) > 6799) %>%
    filter(!is.na(lag_ok) & lag_ok > 0) %>%
    filter(fyr == 12)
  
  main_clean_omk <- ok_prep_final %>%
    select(Date, permno, exchcd, sich, ind17, lag_ok, lag_me_jun, lag_me_dec, port.weight, retadj)
  
  bp_omk <- main_clean_omk %>% 
    filter(exchcd == 1 & lag_ok > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date,ind17) %>%
    summarise(ok_p20 = quantile(lag_ok, probs=.2, na.rm=TRUE),
              ok_p40 = quantile(lag_ok, probs=.4, na.rm=TRUE),
              ok_p60 = quantile(lag_ok, probs=.6, na.rm=TRUE),
              ok_p80 = quantile(lag_ok, probs=.8, na.rm=TRUE))

  main_pf_omk <- main_clean_omk %>%
    merge(bp_omk, by=c("Date","ind17"), all.x=TRUE) %>%
    mutate(ok_pf= ifelse(lag_ok<ok_p20, "ok1", ifelse(lag_ok>=ok_p20 & lag_ok<ok_p40, "ok2", ifelse(lag_ok>=ok_p40 & lag_ok<ok_p60, "ok3", ifelse(lag_ok>=ok_p60 & lag_ok<ok_p80, "ok4", "ok5")))))
  
  ret_omk <- main_pf_omk %>%
    group_by(Date, ok_pf) %>%
    filter(port.weight > 0 & lag_ok > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    summarise(ret_pf = weighted.mean(retadj, port.weight, na.rm=TRUE)) %>% 
    spread(ok_pf, ret_pf) %>% 
    mutate(omk = ok5 - ok1) %>%
    select(Date, omk)
   
   
###############################################################
## Compile all factors  
  
int_factorsk <- ret_t100 %>%  
    merge(ret_t30, by=c("Date"), all.x=TRUE, allow.cartesian=TRUE) %>%
    merge(ret_omk, by=c("Date"), all.x=TRUE, allow.cartesian=TRUE) %>%
    filter(year(Date)>=1975) %>%
    mutate_if(is.numeric, ~. *100) %>% 
    mutate_if(is.numeric, round, digits = 3) 
  
## Output
save(int_factors, file = "int_factors.RData") 
write.csv(int_factors, file = "int_factors.csv", row.names=FALSE)

  
  