###############################################################################################
# Intangible Value 
# by Andrea L. Eisfeldt, Edward Kim, and Dimitris Papanikolaou
#
# 3_gen_int_factors.R 
#
# Create value (HML) factors using intangibles-adjusted book to market equity
#
# Last updated: May 2021 
#
###############################################################################################


################################################################################################    
## FF^INT, EKP method 

  main_clean_t100 <- int_prep_final %>%
    select(Date, permno, exchcd, sich, ind12, lag_bm, lag_bm_int_t100, lag_me_jun, lag_me_dec, port.weight, retadj)
  
  bp_t100 <- main_clean_t100 %>% 
    filter(exchcd == 1 & lag_bm > 0 & lag_bm_int_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>%
    group_by(Date, ind12) %>%
    summarise(var_p70 = quantile(lag_bm_int_t100, probs=.7, na.rm=TRUE),
              var_p30 = quantile(lag_bm_int_t100, probs=.3, na.rm=TRUE),
              size_med = quantile(lag_me_jun, probs=.5, na.rm=TRUE))

  w2_wgt_all <- main_clean_t100 %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_bm_int_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date) %>%
    summarise(wgtall = sum(port.weight, na.rm=T))
  
  w2_wgt <- main_clean_t100 %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_bm_int_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date, ind12) %>%
    summarise(wgt = sum(port.weight, na.rm=T)) %>%
    merge(w2_wgt_all, by="Date") %>%
    mutate(wgt_ind = wgt/wgtall) %>%
    select(-wgt, -wgtall) %>%
    pivot_wider(names_from = ind12, values_from=wgt_ind, names_prefix = "wgt") 
  
  main_rank_t100 <- main_clean_t100 %>%
    merge(bp_t100, by=c("Date","ind12"), all.x=TRUE) %>%
    mutate(ff_size = ifelse(lag_me_jun < size_med, "small", "big"),
           ff_var = ifelse(lag_bm_int_t100 < var_p30, "low", ifelse(lag_bm_int_t100 >= var_p70, "high", ifelse(lag_bm_int_t100 < var_p70 & lag_bm_int_t100 >= var_p30, "med", "na"))),
           ff_pf = paste(ff_size,ff_var,ind12,sep="_"))

  
  ret_t100 <- main_rank_t100 %>% 
    group_by(Date, ff_pf) %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_bm_int_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% #
    summarise(ret_pf = weighted.mean(retadj, port.weight, na.rm=TRUE)) %>% 
    spread(ff_pf, ret_pf)  %>% 
    rowwise() %>%
    mutate(high_1 = coalesce(sum(small_high_1,big_high_1, na.rm=F)/2,small_high_1,big_high_1,NA),
           low_1 = coalesce(sum(small_low_1,big_low_1, na.rm=F)/2,small_low_1,big_low_1,NA),
           high_2 = coalesce(sum(small_high_2,big_high_2, na.rm=F)/2,small_high_2,big_high_2,NA),
           low_2 = coalesce(sum(small_low_2,big_low_2, na.rm=F)/2,small_low_2,big_low_2,NA),
           high_3 = coalesce(sum(small_high_3,big_high_3, na.rm=F)/2,small_high_3,big_high_3,NA),
           low_3 = coalesce(sum(small_low_3,big_low_3, na.rm=F)/2,small_low_3,big_low_3,NA),
           high_4 = coalesce(sum(small_high_4,big_high_4, na.rm=F)/2,small_high_4,big_high_4,NA),
           low_4 = coalesce(sum(small_low_4,big_low_4, na.rm=F)/2,small_low_4,big_low_4,NA),
           high_5 = coalesce(sum(small_high_5,big_high_5, na.rm=F)/2,small_high_5,big_high_5,NA),
           low_5 = coalesce(sum(small_low_5,big_low_5, na.rm=F)/2,small_low_5,big_low_5,NA),
           high_6 = coalesce(sum(small_high_6,big_high_6, na.rm=F)/2,small_high_6,big_high_6,NA),
           low_6 = coalesce(sum(small_low_6,big_low_6, na.rm=F)/2,small_low_6,big_low_6,NA),
           high_7 = coalesce(sum(small_high_7,big_high_7, na.rm=F)/2,small_high_7,big_high_7,NA),
           low_7 = coalesce(sum(small_low_7,big_low_7, na.rm=F)/2,small_low_7,big_low_7,NA),
           high_8 = coalesce(sum(small_high_8,big_high_8, na.rm=F)/2,small_high_8,big_high_8,NA),
           low_8 = coalesce(sum(small_low_8,big_low_8, na.rm=F)/2,small_low_8,big_low_8,NA),
           high_9 = coalesce(sum(small_high_9,big_high_9, na.rm=F)/2,small_high_9,big_high_9,NA),
           low_9 = coalesce(sum(small_low_9,big_low_9, na.rm=F)/2,small_low_9,big_low_9,NA),
           high_10 = coalesce(sum(small_high_10,big_high_10, na.rm=F)/2,small_high_10,big_high_10,NA),
           low_10 = coalesce(sum(small_low_10,big_low_10, na.rm=F)/2,small_low_10,big_low_10,NA),
           high_11 = coalesce(sum(small_high_11,big_high_11, na.rm=F)/2,small_high_11,big_high_11,NA),
           low_11 = coalesce(sum(small_low_11,big_low_11, na.rm=F)/2,small_low_11,big_low_11,NA),
           high_12 = coalesce(sum(small_high_12,big_high_12, na.rm=F)/2,small_high_12,big_high_12,NA),
           low_12 = coalesce(sum(small_low_12,big_low_12, na.rm=F)/2,small_low_12,big_low_12,NA),
           hml_1 = high_1 - low_1,
           hml_2 = high_2 - low_2,
           hml_3 = high_3 - low_3,
           hml_4 = high_4 - low_4,
           hml_5 = high_5 - low_5,
           hml_6 = high_6 - low_6,
           hml_7 = high_7 - low_7,
           hml_8 = high_8 - low_8,
           hml_9 = high_9 - low_9,
           hml_10 = high_10 - low_10,
           hml_11 = high_11 - low_11,
           hml_12 = high_12 - low_12)  %>%
    merge(w2_wgt, by="Date") %>%
    rowwise() %>%
    mutate(hml_int_t100 = sum(hml_1*wgt1, hml_2*wgt2, hml_3*wgt3, hml_4*wgt4, 
                     hml_5*wgt5, hml_6*wgt6, hml_7*wgt7, hml_8*wgt8,
                     hml_9*wgt9, hml_10*wgt10, hml_11*wgt11, hml_12*wgt12, na.rm=T)) %>%
    select(Date, hml_int_t100)
    
    
    
################################################################################################    
## FF^IME, EKP method
  
  main_clean_t100_ime <- int_prep_final %>%
    select(Date, permno, exchcd, sich, ind12, lag_bm, lag_ime_t100, lag_me_jun, lag_me_dec, port.weight, retadj)
  
  bp_t100_ime <- main_clean_t100_ime %>% 
    filter(exchcd == 1 & lag_bm > 0 & lag_ime_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>%
    group_by(Date, ind12) %>%
    summarise(var_p70 = quantile(lag_ime_t100, probs=.7, na.rm=TRUE),
              var_p30 = quantile(lag_ime_t100, probs=.3, na.rm=TRUE),
              size_med = quantile(lag_me_jun, probs=.5, na.rm=TRUE))
  
  w2_wgt_all_ime <- main_clean_t100_ime %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_ime_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date) %>%
    summarise(wgtall = sum(port.weight, na.rm=T))
  
  w2_wgt_ime <- main_clean_t100_ime %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_ime_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date, ind12) %>%
    summarise(wgt = sum(port.weight, na.rm=T)) %>%
    merge(w2_wgt_all_ime, by="Date") %>%
    mutate(wgt_ind = wgt/wgtall) %>%
    select(-wgt, -wgtall) %>%
    pivot_wider(names_from = ind12, values_from=wgt_ind, names_prefix = "wgt") 
  
  main_rank_t100_ime <- main_clean_t100_ime %>%
    merge(bp_t100_ime, by=c("Date","ind12"), all.x=TRUE) %>%
    mutate(ff_size = ifelse(lag_me_jun < size_med, "small", "big"),
           ff_var = ifelse(lag_ime_t100 < var_p30, "low", ifelse(lag_ime_t100 >= var_p70, "high", ifelse(lag_ime_t100 < var_p70 & lag_ime_t100 >= var_p30, "med", "na"))),
           ff_pf = paste(ff_size,ff_var,ind12,sep="_"))
  
  ret_t100_ime <- main_rank_t100_ime %>% 
    group_by(Date, ff_pf) %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_ime_t100 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% #
    summarise(ret_pf = weighted.mean(retadj, port.weight, na.rm=TRUE)) %>% 
    spread(ff_pf, ret_pf) %>% 
    rowwise() %>%
    mutate(high_1 = coalesce(sum(small_high_1,big_high_1, na.rm=F)/2,small_high_1,big_high_1,NA),
           low_1 = coalesce(sum(small_low_1,big_low_1, na.rm=F)/2,small_low_1,big_low_1,NA),
           high_2 = coalesce(sum(small_high_2,big_high_2, na.rm=F)/2,small_high_2,big_high_2,NA),
           low_2 = coalesce(sum(small_low_2,big_low_2, na.rm=F)/2,small_low_2,big_low_2,NA),
           high_3 = coalesce(sum(small_high_3,big_high_3, na.rm=F)/2,small_high_3,big_high_3,NA),
           low_3 = coalesce(sum(small_low_3,big_low_3, na.rm=F)/2,small_low_3,big_low_3,NA),
           high_4 = coalesce(sum(small_high_4,big_high_4, na.rm=F)/2,small_high_4,big_high_4,NA),
           low_4 = coalesce(sum(small_low_4,big_low_4, na.rm=F)/2,small_low_4,big_low_4,NA),
           high_5 = coalesce(sum(small_high_5,big_high_5, na.rm=F)/2,small_high_5,big_high_5,NA),
           low_5 = coalesce(sum(small_low_5,big_low_5, na.rm=F)/2,small_low_5,big_low_5,NA),
           high_6 = coalesce(sum(small_high_6,big_high_6, na.rm=F)/2,small_high_6,big_high_6,NA),
           low_6 = coalesce(sum(small_low_6,big_low_6, na.rm=F)/2,small_low_6,big_low_6,NA),
           high_7 = coalesce(sum(small_high_7,big_high_7, na.rm=F)/2,small_high_7,big_high_7,NA),
           low_7 = coalesce(sum(small_low_7,big_low_7, na.rm=F)/2,small_low_7,big_low_7,NA),
           high_8 = coalesce(sum(small_high_8,big_high_8, na.rm=F)/2,small_high_8,big_high_8,NA),
           low_8 = coalesce(sum(small_low_8,big_low_8, na.rm=F)/2,small_low_8,big_low_8,NA),
           high_9 = coalesce(sum(small_high_9,big_high_9, na.rm=F)/2,small_high_9,big_high_9,NA),
           low_9 = coalesce(sum(small_low_9,big_low_9, na.rm=F)/2,small_low_9,big_low_9,NA),
           high_10 = coalesce(sum(small_high_10,big_high_10, na.rm=F)/2,small_high_10,big_high_10,NA),
           low_10 = coalesce(sum(small_low_10,big_low_10, na.rm=F)/2,small_low_10,big_low_10,NA),
           high_11 = coalesce(sum(small_high_11,big_high_11, na.rm=F)/2,small_high_11,big_high_11,NA),
           low_11 = coalesce(sum(small_low_11,big_low_11, na.rm=F)/2,small_low_11,big_low_11,NA),
           high_12 = coalesce(sum(small_high_12,big_high_12, na.rm=F)/2,small_high_12,big_high_12,NA),
           low_12 = coalesce(sum(small_low_12,big_low_12, na.rm=F)/2,small_low_12,big_low_12,NA),
           hml_1 = high_1 - low_1,
           hml_2 = high_2 - low_2,
           hml_3 = high_3 - low_3,
           hml_4 = high_4 - low_4,
           hml_5 = high_5 - low_5,
           hml_6 = high_6 - low_6,
           hml_7 = high_7 - low_7,
           hml_8 = high_8 - low_8,
           hml_9 = high_9 - low_9,
           hml_10 = high_10 - low_10,
           hml_11 = high_11 - low_11,
           hml_12 = high_12 - low_12) %>%
    merge(w2_wgt_ime, by="Date") %>%
    rowwise() %>%
    mutate(hml_ime_t100 = sum(hml_1*wgt1, hml_2*wgt2, hml_3*wgt3, hml_4*wgt4, 
                              hml_5*wgt5, hml_6*wgt6, hml_7*wgt7, hml_8*wgt8,
                              hml_9*wgt9, hml_10*wgt10, hml_11*wgt11, hml_12*wgt12, na.rm=T)) %>%
    select(Date, hml_ime_t100)
  
  
  
  
  
  
################################################################################################    
## FF^INT, PT method
  
  main_clean_t30 <- int_prep_final %>%
    select(Date, permno, exchcd, sich, lag_bm, lag_bm_int_t30, lag_me_jun, lag_me_dec, port.weight, retadj)
  
  bp_t30 <- main_clean_t30 %>% 
    filter(exchcd == 1 & lag_bm > 0 & lag_bm_int_t30 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date) %>%
    summarise(var_p70 = quantile(lag_bm_int_t30, probs=.7, na.rm=TRUE),
              var_p30 = quantile(lag_bm_int_t30, probs=.3, na.rm=TRUE),
              size_med = quantile(lag_me_jun, probs=.5, na.rm=TRUE))

  main_rank_t30 <- main_clean_t30 %>%
    merge(bp_t30, by="Date", all.x=TRUE) %>%
    mutate(ff_size = ifelse(lag_me_jun < size_med, "small", "big"),
           ff_var = ifelse(lag_bm_int_t30 < var_p30, "low", ifelse(lag_bm_int_t30 >= var_p70, "high", "na")),
           ff_pf = paste(ff_size,ff_var,sep="_")) 
  
  ret_t30 <- main_rank_t30 %>% 
    group_by(Date, ff_pf) %>%
    filter(port.weight > 0 & lag_bm > 0 & lag_bm_int_t30 > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    summarise(ret_pf = weighted.mean(retadj, port.weight, na.rm=TRUE)) %>% 
    spread(ff_pf, ret_pf) %>% 
    mutate(high = (small_high + big_high)/2,
           low = (small_low + big_low)/2,
           hml_int_t30 = high - low) %>%
    select(Date, hml_int_t30)
  
  
################################################################################################    
## OMK factor by Eisfeldt and Papanikolaou (2013)

# Initialize dataset, filters based on Eisfeldt and Papanikolaou (2013)
ok_prep_final <- int_prep_final %>% 
    filter(!is.na(sic)) %>%
    filter(as.numeric(sic) < 6000 | as.numeric(sic) > 6799) %>%
    filter(!is.na(lag_ok) & lag_ok > 0) %>%
    filter(fyr == 12)
  
  main_clean_omk <- ok_prep_final %>%
    select(Date, permno, exchcd, sich, ind17, lag_bm, lag_ok, lag_me_jun, lag_me_dec, port.weight, retadj)
  
  bp_omk <- main_clean_omk %>% 
    filter(exchcd == 1 & lag_ok > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    group_by(Date,ind17) %>%
    summarise(var_p20 = quantile(lag_ok, probs=.2, na.rm=TRUE),
              var_p40 = quantile(lag_ok, probs=.4, na.rm=TRUE),
              var_p60 = quantile(lag_ok, probs=.6, na.rm=TRUE),
              var_p80 = quantile(lag_ok, probs=.8, na.rm=TRUE))

  main_rank_omk <- main_clean_omk %>%
    merge(bp_omk, by=c("Date","ind17"), all.x=TRUE) %>%
    mutate(ff_pf= ifelse(lag_ok<var_p20, "ok1", ifelse(lag_ok>=var_p20 & lag_ok<var_p40, "ok2", ifelse(lag_ok>=var_p40 & lag_ok<var_p60, "ok3", ifelse(lag_ok>=var_p60 & lag_ok<var_p80, "ok4", "ok5")))))
  
  ret_omk <- main_rank_omk %>%
    group_by(Date, ff_pf) %>%
    filter(port.weight > 0 & lag_ok > 0 & lag_me_jun > 0 & lag_me_dec > 0) %>% 
    summarise(ret_pf = weighted.mean(retadj, port.weight, na.rm=TRUE)) %>% 
    spread(ff_pf, ret_pf) %>% 
    mutate(omk = ok5 - ok1) %>%
    select(Date, omk)
   
   

  
###############################################################
## Compile all factors  
  
int_factors <- ret_t100 %>%  
    merge(ret_t100_ime, by=c("Date"), all.x=TRUE, allow.cartesian=TRUE) %>%
    merge(ret_t30, by=c("Date"), all.x=TRUE, allow.cartesian=TRUE) %>%
    merge(ret_omk, by=c("Date"), all.x=TRUE, allow.cartesian=TRUE) %>%
    filter(year(Date)>=1975) %>%
    mutate_if(is.numeric, ~. *100) %>% #convert to percent
    mutate_if(is.numeric, round, digits = 4) 
  
## Output
save(int_factors, file = "int_factors.RData") 
write.csv(int_factors, file = "int_factors.csv", row.names=FALSE)





  