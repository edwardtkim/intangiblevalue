###############################################################################################
# Intangible Value 
# by Andrea L. Eisfeldt, Edward Kim, and Dimitris Papanikolaou
#
# R Programs:
#    1_getdata.R
#    2_gen_int.R
#    3_gen_int_factors.R
#
# Datasets:
#    int_xsec.csv
#    int_factors.csv
###############################################################################################

## Variable definitions

# int_xsec.csv
datadate is the reporting date for a data record in Compustat 
permno and gvkey are company identifiers
be is book equity as defined in Fama and French (1992, 1993)
int_t100 and int_t30 are intangible capital estimated using different methodologies: int_t100 accumulates 100% of past SG&A, while int_t30 separately accumulates 30% of past SG&A minus R&D and 100% of past R&D 
be_int_t100 and be_int_t30 are book equity variables adjusted for intangible capital 
bm, bm_int_t100, and bm_int_t30 are book-to-market ratios computed using methodology in Fama and French (1992, 1993) and the different measures of book equity
ok is the organization capital to total assets ratio from Eisfeldt and Papanikolaou (2013)



# int_factors.csv
Date is calendar year-month
hml_int_t100 and hml_int_t30 are the monthly returns of the intangible value factor in percent
omk is the monthly returns of the OMK portfolio (Eisfeldt and Papanikolaou, 2013) in percent

 