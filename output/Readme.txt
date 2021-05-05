###############################################################################################
# Intangible Value 
# by Andrea L. Eisfeldt, Edward T. Kim, and Dimitris Papanikolaou
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
k_int_t100: intangible capital computed using EKP method
k_int_t30: intangible capital computed using PT method
bm: be/me as defined in Fama and French (1992, 1993)
bm_int_t100: (be + int_t100 - gdwl)/me
bm_int_t30: (be + int_t30 - gdwl)/me
ime_t100: k_int_t100/me
ok: organization capital to total assets ratio (Eisfeldt and Papanikolaou, 2013)

# int_factors.csv
Date: calendar year-month
hml_int_t100: HML^INT EKP monthly returns (percent)
hml_int-t30: HML^INT PT monthly returns (percent)
omk: OMK portfolio returns (percent)
