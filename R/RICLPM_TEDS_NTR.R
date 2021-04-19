
unconstrained <- 'ri_EXT =~ 1*EXT_t1 + 1*EXT_t2 + 1*EXT_t3
ri_ATT =~ 1*ATT_t1 + 1*ATT_t2 + 1*ATT_t3
ri_INT =~ 1*INT_t1 + 1*INT_t2 + 1*INT_t3
ri_SOC =~ 1*SOC_t1 + 1*SOC_t2 + 1*SOC_t3

ri_EXT ~~ ri_EXT
ri_ATT ~~ ri_ATT
ri_INT ~~ ri_INT
ri_SOC ~~ ri_SOC

ri_EXT ~~ ri_ATT + ri_INT + ri_SOC
ri_ATT ~~ ri_INT + ri_SOC
ri_INT ~~ ri_SOC

EXT_t1 ~ EXT_t1_mu*1
EXT_t2 ~ EXT_t2_mu*1
EXT_t3 ~ EXT_t3_mu*1
ATT_t1 ~ ATT_t1_mu*1
ATT_t2 ~ ATT_t2_mu*1
ATT_t3 ~ ATT_t3_mu*1
INT_t1 ~ INT_t1_mu*1
INT_t2 ~ INT_t2_mu*1
INT_t3 ~ INT_t3_mu*1
SOC_t1 ~ SOC_t1_mu*1
SOC_t2 ~ SOC_t2_mu*1
SOC_t3 ~ SOC_t3_mu*1

lat_EXT1 =~ 1*EXT_t1
lat_EXT2 =~ 1*EXT_t2
lat_EXT3 =~ 1*EXT_t3
lat_ATT1 =~ 1*ATT_t1
lat_ATT2 =~ 1*ATT_t2
lat_ATT3 =~ 1*ATT_t3
lat_INT1 =~ 1*INT_t1
lat_INT2 =~ 1*INT_t2
lat_INT3 =~ 1*INT_t3
lat_SOC1 =~ 1*SOC_t1
lat_SOC2 =~ 1*SOC_t2
lat_SOC3 =~ 1*SOC_t3

lat_EXT1 ~~ lat_ATT1 + lat_INT1 + lat_SOC1
lat_EXT2 ~~ lat_ATT2 + lat_INT2 + lat_SOC2
lat_EXT3 ~~ lat_ATT3 + lat_INT3 + lat_SOC3
lat_ATT1 ~~ lat_INT1 + lat_SOC1
lat_ATT2 ~~ lat_INT2 + lat_SOC2
lat_ATT3 ~~ lat_INT3 + lat_SOC3
lat_INT1 ~~ lat_SOC1
lat_INT2 ~~ lat_SOC2
lat_INT3 ~~ lat_SOC3

lat_EXT2 ~ lat_EXT1 + lat_ATT1 + lat_INT1 + lat_SOC1
lat_EXT3 ~ lat_EXT2 + lat_ATT2 + lat_INT2 + lat_SOC2
lat_ATT2 ~ lat_EXT1 + lat_ATT1 + lat_INT1 + lat_SOC1
lat_ATT3 ~ lat_EXT2 + lat_ATT2 + lat_INT2 + lat_SOC2
lat_INT2 ~ lat_EXT1 + lat_ATT1 + lat_INT1 + lat_SOC1
lat_INT3 ~ lat_EXT2 + lat_ATT2 + lat_INT2 + lat_SOC2
lat_SOC2 ~ lat_EXT1 + lat_ATT1 + lat_INT1 + lat_SOC1
lat_SOC3 ~ lat_EXT2 + lat_ATT2 + lat_INT2 + lat_SOC2

lat_EXT1 ~~ lat_EXT1
lat_EXT2 ~~ lat_EXT2
lat_EXT3 ~~ lat_EXT3
lat_ATT1 ~~ lat_ATT1
lat_ATT2 ~~ lat_ATT2
lat_ATT3 ~~ lat_ATT3
lat_INT1 ~~ lat_INT1
lat_INT2 ~~ lat_INT2
lat_INT3 ~~ lat_INT3
lat_SOC1 ~~ lat_SOC1
lat_SOC2 ~~ lat_SOC2
lat_SOC3 ~~ lat_SOC3
'