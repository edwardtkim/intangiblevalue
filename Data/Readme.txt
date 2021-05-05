###############################################################################################
# Intangible Value 
# by Andrea L. Eisfeldt, Edward Kim, and Dimitris Papanikolaou
#
# R Programs:
#    1_getdata.R
#    2_gen_int.R
#    3_gen_int_factors.R
#
###############################################################################################

## Variable descriptions

# int_xsec.csv
datadate: reporting date for a data record in Compustat Xpressfeed (fiscal period end date)
permno, gvkey: company identifier
be: book equity as defined in Fama and French (1992, 1993)
be_int_t100: be + int_t100 - gdwl
be_int_t30: be + int_t30 - gdwl
bm: be/me as defined in Fama and French (1992, 1993)
bm_int_t100: be_int_t100/me
bm_int_t30: be_int_t30/me
ok: organization capital to total assets ratio (Eisfeldt and Papanikolaou, 2013)

# int_factors.csv
Date: calendar year-month
hml_int_t100: HML^INT t100 monthly returns (percent)
hml_int-t30: HML^INT t30 monthly returns (percent)
omk: OMK portfolio (percent)
