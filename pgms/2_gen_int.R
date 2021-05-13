###############################################################################################
# Intangible Value 
# by Andrea L. Eisfeldt, Edward T. Kim, and Dimitris Papanikolaou
#
# 2_gen_int.R 
#
# Generate intangible capital data using perpetual inventory method (Eisfeldt and Papanikolaou, 2013)
#
# Last updated: May 2021
#
###############################################################################################
 
 
##############################
## Initialize data
##############################

cc1 <- comp_prep_final #intermediate dataset from 1_getdata.R
cc2 <- cc1 %>% select(datadate, permno, at, xrd, xint, xsga, sich, tmcpiref, intan, rdip, cogs)

# Merge sic code from crsp_sic, assign industry-specific rd depreciation rates obtained from Li and Hall (2020) -- working paper version
rd_depreciation <- read.csv("rd_depreciation.csv")
names(rd_depreciation) <- c("sic","d_rd")
cc3 <- cc2 %>%
  merge(crsp_sic, by.x=c("datadate","permno"), by.y=c("Date","permno"), all.x=TRUE) %>%
  mutate(sic = coalesce(sich,hsiccd))  %>%
  merge(rd_depreciation, by="sic", all.x=T) 
cc3$d_rd <- ifelse(!is.na(cc3$d_rd), cc3$d_rd, 0.15) # Set d_rd for industries not defined by Li and Hall (2020) as 15%

cc4 <- cc3 %>% 
  group_by(permno) %>%
  ## Method 1: EKP method
  mutate(sga_t100 = ifelse(is.na(xsga),coalesce(xsga,0),coalesce(xsga,0))) %>%
  mutate(sga_t100_r = sga_t100/(tmcpiref/100)) %>%
  ## Method 2: PT method
  mutate(rd_r = coalesce(xrd/(tmcpiref/100),0)) %>%
  mutate(sga1 = coalesce(xsga,0)-coalesce(xrd,0)-coalesce(rdip,0)) %>%
  mutate(sga2 = ifelse((coalesce(xrd,0)>coalesce(xsga,0))&(coalesce(xrd,0)<coalesce(cogs,0)),coalesce(xsga,0),sga1)) %>%
  mutate(sga3 = ifelse(is.na(xsga),coalesce(xsga,0),sga2)) %>%
  mutate(sga_r = sga3/(tmcpiref/100)) %>%
  filter(!is.na(at)) %>%
  add_tally()

# Initialize first observation
cc4_init <- cc4 %>%
  filter(n>1) %>%
  mutate(gt = 0) %>%
  arrange(permno,datadate) %>%
  group_by(permno) %>%
  filter(!is.na(at)) %>%
  mutate(
         orgt_d15_t100 = (1-0.15)*((1*sga3)/(0.1+0.15))+1*sga3,
         int_d20_t30 = (1-0.20)*((0.3*sga3)/(0.1+0.20))+0.3*sga3,
         int_d20_t100 = (1-0.20)*((1*sga_t100)/(0.1+0.20))+1*sga_t100) %>%
  filter(row_number()==1) %>%
  mutate(datadate = datadate %m-% years(1)) %>%
  ungroup() 

cc5 <- dplyr::bind_rows(cc4, cc4_init) %>%
  arrange(permno, datadate) %>%
  filter(n>1) %>%
  filter(!is.na(at))


############################## 
## Main perpetual inventory method program
##############################
ppint1 <- cc5 %>% select(datadate, permno, gt, 
                         orgt_d15_t100, int_d20_t30, int_d20_t100) 
ppint2 <- ppint1[FALSE,]

u <- unique(cc5$permno)
for (j in 1:length(u)) {
  cc6 <- cc5 %>% filter(permno==u[j])
  
  
n <- max(cc6$n) 
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

# Remove initialized first observation
ppint3 <- ppint2 %>% 
  group_by(permno) %>%
  slice(-1)

## Output
rm(cc1, cc2, cc3, cc4, cc5, cc6, cc7)
save(ppint3, file="ppint3.RData")
