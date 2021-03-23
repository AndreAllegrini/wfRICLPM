
unconstrained <- 'ri_CND =~ 1*CON_t1 + 1*CON_t2 + 1*CON_t3
ri_HYP =~ 1*HYP_t1 + 1*HYP_t2 + 1*HYP_t3
ri_EMO =~ 1*EMp_t1 + 1*EMp_t2 + 1*EMp_t3
ri_PER =~ 1*PEp_t1 + 1*PEp_t2 + 1*PEp_t3

ri_CND ~~ ri_CND
ri_HYP ~~ ri_HYP
ri_EMO ~~ ri_EMO
ri_PER ~~ ri_PER

ri_CND ~~ ri_HYP + ri_EMO + ri_PER
ri_HYP ~~ ri_EMO + ri_PER
ri_EMO ~~ ri_PER

CON_t1 ~ CON_t1_mu*1
CON_t2 ~ CON_t2_mu*1
CON_t3 ~ CON_t3_mu*1
HYP_t1 ~ HYP_t1_mu*1
HYP_t2 ~ HYP_t2_mu*1
HYP_t3 ~ HYP_t3_mu*1
EMp_t1 ~ EMp_t1_mu*1
EMp_t2 ~ EMp_t2_mu*1
EMp_t3 ~ EMp_t3_mu*1
PEp_t1 ~ PEp_t1_mu*1
PEp_t2 ~ PEp_t2_mu*1
PEp_t3 ~ PEp_t3_mu*1

lat_CND1 =~ 1*CON_t1
lat_CND2 =~ 1*CON_t2
lat_CND3 =~ 1*CON_t3
lat_HYP1 =~ 1*HYP_t1
lat_HYP2 =~ 1*HYP_t2
lat_HYP3 =~ 1*HYP_t3
lat_EMO1 =~ 1*EMp_t1
lat_EMO2 =~ 1*EMp_t2
lat_EMO3 =~ 1*EMp_t3
lat_PER1 =~ 1*PEp_t1
lat_PER2 =~ 1*PEp_t2
lat_PER3 =~ 1*PEp_t3

lat_CND1 ~~ lat_HYP1 + lat_EMO1 + lat_PER1
lat_CND2 ~~ lat_HYP2 + lat_EMO2 + lat_PER2
lat_CND3 ~~ lat_HYP3 + lat_EMO3 + lat_PER3
lat_HYP1 ~~ lat_EMO1 + lat_PER1
lat_HYP2 ~~ lat_EMO2 + lat_PER2
lat_HYP3 ~~ lat_EMO3 + lat_PER3
lat_EMO1 ~~ lat_PER1
lat_EMO2 ~~ lat_PER2
lat_EMO3 ~~ lat_PER3

lat_CND2 ~ lat_CND1 + lat_HYP1 + lat_EMO1 + lat_PER1
lat_CND3 ~ lat_CND2 + lat_HYP2 + lat_EMO2 + lat_PER2
lat_HYP2 ~ lat_CND1 + lat_HYP1 + lat_EMO1 + lat_PER1
lat_HYP3 ~ lat_CND2 + lat_HYP2 + lat_EMO2 + lat_PER2
lat_EMO2 ~ lat_CND1 + lat_HYP1 + lat_EMO1 + lat_PER1
lat_EMO3 ~ lat_CND2 + lat_HYP2 + lat_EMO2 + lat_PER2
lat_PER2 ~ lat_CND1 + lat_HYP1 + lat_EMO1 + lat_PER1
lat_PER3 ~ lat_CND2 + lat_HYP2 + lat_EMO2 + lat_PER2

lat_CND1 ~~ lat_CND1
lat_CND2 ~~ lat_CND2
lat_CND3 ~~ lat_CND3
lat_HYP1 ~~ lat_HYP1
lat_HYP2 ~~ lat_HYP2
lat_HYP3 ~~ lat_HYP3
lat_EMO1 ~~ lat_EMO1
lat_EMO2 ~~ lat_EMO2
lat_EMO3 ~~ lat_EMO3
lat_PER1 ~~ lat_PER1
lat_PER2 ~~ lat_PER2
lat_PER3 ~~ lat_PER3
'