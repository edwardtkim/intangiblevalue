###############################################################################################
# Intangible Value 
# by Andrea L. Eisfeldt, Edward Kim, and Dimitris Papanikolaou
#
# 2_gen_int.R 
#
# Generate intangible capital data using perpetual inventory method (Eisfeldt and Papanikolaou, 2013)
#
# Last updated: Nov 2020
#
###############################################################################################


##############################
## Initialize data
##############################

cc1 <- comp_prep_final #intermediate dataset from 1_getdata.R 
cc2 <- cc1 %>% select(datadate, permno, at, xrd, xint, xsga, sich, tmcpiref, intan, rdip, cogs)

# Merge sic code from crsp_sic, assign industry-specific rd depreciation rates
cc3 <- cc2 %>%
  merge(crsp_sic, by.x=c("datadate","permno"), by.y=c("Date","permno"), all.x=TRUE) %>%
  mutate(sic = coalesce(sich,hsiccd)) 

cc3$d_rd <- ifelse(cc3$sic==3341, 0.363, 
            ifelse(cc3$sic==5112, 0.308,
            ifelse(cc3$sic==3254, 0.112,
            ifelse(cc3$sic==3344, 0.226,    
            ifelse(cc3$sic==3364, 0.339,
            ifelse(cc3$sic==3342, 0.192,
            ifelse(cc3$sic==5415, 0.489,
            ifelse(cc3$sic==3361, 0.733,
            ifelse(cc3$sic==3362, 0.733,
            ifelse(cc3$sic==3363, 0.733,
            ifelse(cc3$sic==3345, 0.329,
            ifelse(cc3$sic==5417, 0.295,0.15))))))))))))

cc4 <- cc3 %>% 
  group_by(permno) %>%
  ## Method 1: perpetual inventory method for full sga 
  mutate(sga_t100 = ifelse(is.na(xsga),coalesce(xsga,0),coalesce(xsga,0))) %>%
  mutate(sga_t100_r = sga_t100/(tmcpiref/100)) %>%
  ## Method 2: consider organization and knowledge capital separately
  mutate(rd_r = coalesce(xrd/(tmcpiref/100),0)) %>%
  mutate(sga1 = coalesce(xsga,0)-coalesce(xrd,0)-coalesce(rdip,0)) %>%
  mutate(sga2 = ifelse((coalesce(xrd,0)>coalesce(xsga,0))&(coalesce(xrd,0)<coalesce(cogs,0)),coalesce(xsga,0),sga1)) %>%
  mutate(sga3 = ifelse(is.na(xsga),coalesce(xsga,0),sga2)) %>%
  mutate(sga_r = sga3/(tmcpiref/100)) %>%
  add_tally()


cc5 <- cc4 %>%
  filter(n>1) %>%
  mutate(gt = 0) %>%
  group_by(permno) %>%
  mutate(orgt_d15_t100 = (1-0.15)*((1*sga3)/(0.1+0.15))+1*sga3,
         int_d20_t30 = (1-0.20)*((0.3*sga3)/(0.1+0.20))+0.3*sga3,
         int_d20_t100 = (1-0.20)*((1*sga_t100)/(0.1+0.20))+1*sga_t100) %>%
  ungroup()


############################## 
## Main program
##############################

ppint1 <- cc5 %>% select(datadate, permno, gt, 
                         orgt_d15_t100, int_d20_t30, int_d20_t100)
ppint2 <- ppint1[FALSE,]

u <- unique(cc5$permno)
for (j in 1:length(u)) {
  cc6 <- cc5 %>% filter(permno==u[j])
  
  
n <- max(cc6$n)-1
for(i in 1:n)
{
  cc6$gt[i+1] = (1-cc6$d_rd[i])*cc6$gt[i] + cc6$rd_r[i+1] 
  cc6$orgt_d15_t100[i+1] = (1-0.15)*cc6$orgt_d15_t100[i] + cc6$sga_r[i+1]
  cc6$int_d20_t30[i+1] = (1-0.20)*cc6$int_d20_t30[i] + 0.3*cc6$sga_r[i+1]
  cc6$int_d20_t100[i+1] = (1-0.20)*cc6$int_d20_t100[i] + cc6$sga_t100_r[i+1]
  }
cc7 <- cc6 %>% select(datadate, permno, gt, 
                      orgt_d15_t100, int_d20_t30, int_d20_t100)
ppint2 <- rbind(ppint2, cc7)
}

## Output
rm(cc1, cc2, cc3, cc4, cc5, cc6, cc7)
save(ppint2, file="ppint2.RData")
