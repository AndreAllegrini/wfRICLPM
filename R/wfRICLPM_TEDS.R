WF_RICLPM <- '
#random interecepts for sib 1

ri_EXTa =~ 1*EXT_t1a + 1*EXT_t2a + 1*EXT_t3a
ri_ATTa =~ 1*ATT_t1a + 1*ATT_t2a + 1*ATT_t3a
ri_INTa =~ 1*INT_t1a + 1*INT_t2a + 1*INT_t3a
ri_SOCa =~ 1*SOC_t1a + 1*SOC_t2a + 1*SOC_t3a

#random interecepts for sib 2

ri_EXTb =~ 1*EXT_t1b + 1*EXT_t2b + 1*EXT_t3b
ri_ATTb =~ 1*ATT_t1b + 1*ATT_t2b + 1*ATT_t3b
ri_INTb =~ 1*INT_t1b + 1*INT_t2b + 1*INT_t3b
ri_SOCb =~ 1*SOC_t1b + 1*SOC_t2b + 1*SOC_t3b

#RI variances equal across twins and zygosity 

ri_EXTa ~~ c(vI_EXT,vI_EXT)*ri_EXTa
ri_ATTa ~~ c(vI_ATT,vI_ATT)*ri_ATTa
ri_INTa ~~ c(vI_INT,vI_INT)*ri_INTa
ri_SOCa ~~ c(vI_SOC,vI_SOC)*ri_SOCa

ri_EXTb ~~ c(vI_EXT,vI_EXT)*ri_EXTb
ri_ATTb ~~ c(vI_ATT,vI_ATT)*ri_ATTb
ri_INTb ~~ c(vI_INT,vI_INT)*ri_INTb
ri_SOCb ~~ c(vI_SOC,vI_SOC)*ri_SOCb

#RI covariances within sibling

ri_EXTa ~~ c(wI_EXTATT,wI_EXTATT)*ri_ATTa + c(wI_EXTINT,wI_EXTINT)*ri_INTa + c(wI_EXTSOC,wI_EXTSOC)*ri_SOCa
ri_ATTa ~~ c(wI_ATTINT,wI_ATTINT)*ri_INTa + c(wI_ATTSOC,wI_ATTSOC)*ri_SOCa
ri_INTa ~~ c(wI_INTSOC,wI_INTSOC)*ri_SOCa

ri_EXTb ~~ c(wI_EXTATT,wI_EXTATT)*ri_ATTb + c(wI_EXTINT,wI_EXTINT)*ri_INTb + c(wI_EXTSOC,wI_EXTSOC)*ri_SOCb
ri_ATTb ~~ c(wI_ATTINT,wI_ATTINT)*ri_INTb + c(wI_ATTSOC,wI_ATTSOC)*ri_SOCb
ri_INTb ~~ c(wI_INTSOC,wI_INTSOC)*ri_SOCb

#RI covariances between siblings

ri_EXTa ~~ c(bI_EXTEXT_dz,bI_EXTEXT_mz)*ri_EXTb + c(bI_EXTATT_dz,bI_EXTATT_mz)*ri_ATTb + c(bI_EXTINT_dz,bI_EXTINT_mz)*ri_INTb + c(bI_EXTSOC_dz,bI_EXTSOC_mz)*ri_SOCb
ri_ATTa ~~ c(bI_EXTATT_dz,bI_EXTATT_mz)*ri_EXTb + c(bI_ATTATT_dz,bI_ATTATT_mz)*ri_ATTb + c(bI_ATTINT_dz,bI_ATTINT_mz)*ri_INTb + c(bI_ATTSOC_dz,bI_ATTSOC_mz)*ri_SOCb
ri_INTa ~~ c(bI_EXTINT_dz,bI_EXTINT_mz)*ri_EXTb + c(bI_ATTINT_dz,bI_ATTINT_mz)*ri_ATTb + c(bI_INTINT_dz,bI_INTINT_mz)*ri_INTb + c(bI_INTSOC_dz,bI_INTSOC_mz)*ri_SOCb
ri_SOCa ~~ c(bI_EXTSOC_dz,bI_EXTSOC_mz)*ri_EXTb + c(bI_ATTSOC_dz,bI_ATTSOC_mz)*ri_ATTb + c(bI_INTSOC_dz,bI_INTSOC_mz)*ri_INTb + c(bI_SOCSOC_dz,bI_SOCSOC_mz)*ri_SOCb

# fixed intercepts equal across twins and zygosity 

EXT_t1a ~ EXT_t1_mu*1
EXT_t2a ~ EXT_t2_mu*1
EXT_t3a ~ EXT_t3_mu*1
ATT_t1a ~ ATT_t1_mu*1
ATT_t2a ~ ATT_t2_mu*1
ATT_t3a ~ ATT_t3_mu*1
INT_t1a ~ INT_t1_mu*1
INT_t2a ~ INT_t2_mu*1
INT_t3a ~ INT_t3_mu*1
SOC_t1a ~ SOC_t1_mu*1
SOC_t2a ~ SOC_t2_mu*1
SOC_t3a ~ SOC_t3_mu*1

EXT_t1b ~ EXT_t1_mu*1
EXT_t2b ~ EXT_t2_mu*1
EXT_t3b ~ EXT_t3_mu*1
ATT_t1b ~ ATT_t1_mu*1
ATT_t2b ~ ATT_t2_mu*1
ATT_t3b ~ ATT_t3_mu*1
INT_t1b ~ INT_t1_mu*1
INT_t2b ~ INT_t2_mu*1
INT_t3b ~ INT_t3_mu*1
SOC_t1b ~ SOC_t1_mu*1
SOC_t2b ~ SOC_t2_mu*1
SOC_t3b ~ SOC_t3_mu*1

#latent variables from observed loadings fixed to 1 

lat_EXTa1 =~ 1*EXT_t1a
lat_EXTa2 =~ 1*EXT_t2a
lat_EXTa3 =~ 1*EXT_t3a
lat_ATTa1 =~ 1*ATT_t1a
lat_ATTa2 =~ 1*ATT_t2a
lat_ATTa3 =~ 1*ATT_t3a
lat_INTa1 =~ 1*INT_t1a
lat_INTa2 =~ 1*INT_t2a
lat_INTa3 =~ 1*INT_t3a
lat_SOCa1 =~ 1*SOC_t1a
lat_SOCa2 =~ 1*SOC_t2a
lat_SOCa3 =~ 1*SOC_t3a

lat_EXTb1 =~ 1*EXT_t1b
lat_EXTb2 =~ 1*EXT_t2b
lat_EXTb3 =~ 1*EXT_t3b
lat_ATTb1 =~ 1*ATT_t1b
lat_ATTb2 =~ 1*ATT_t2b
lat_ATTb3 =~ 1*ATT_t3b
lat_INTb1 =~ 1*INT_t1b
lat_INTb2 =~ 1*INT_t2b
lat_INTb3 =~ 1*INT_t3b
lat_SOCb1 =~ 1*SOC_t1b
lat_SOCb2 =~ 1*SOC_t2b
lat_SOCb3 =~ 1*SOC_t3b

#regressions within - equal across twins and zygosity  

lat_EXTa2 ~ c(rwEXT_EXT2,rwEXT_EXT2)*lat_EXTa1 + c(rwEXT_ATT2,rwEXT_ATT2)*lat_ATTa1 + c(rwEXT_INT2,rwEXT_INT2)*lat_INTa1 + c(rwEXT_SOC2,rwEXT_SOC2)*lat_SOCa1
lat_EXTa3 ~ c(rwEXT_EXT3,rwEXT_EXT3)*lat_EXTa2 + c(rwEXT_ATT3,rwEXT_ATT3)*lat_ATTa2 + c(rwEXT_INT3,rwEXT_INT3)*lat_INTa2 + c(rwEXT_SOC3,rwEXT_SOC3)*lat_SOCa2
lat_ATTa2 ~ c(rwATT_EXT2,rwATT_EXT2)*lat_EXTa1 + c(rwATT_ATT2,rwATT_ATT2)*lat_ATTa1 + c(rwATT_INT2,rwATT_INT2)*lat_INTa1 + c(rwATT_SOC2,rwATT_SOC2)*lat_SOCa1
lat_ATTa3 ~ c(rwATT_EXT3,rwATT_EXT3)*lat_EXTa2 + c(rwATT_ATT3,rwATT_ATT3)*lat_ATTa2 + c(rwATT_INT3,rwATT_INT3)*lat_INTa2 + c(rwATT_SOC3,rwATT_SOC3)*lat_SOCa2
lat_INTa2 ~ c(rwINT_EXT2,rwINT_EXT2)*lat_EXTa1 + c(rwINT_ATT2,rwINT_ATT2)*lat_ATTa1 + c(rwINT_INT2,rwINT_INT2)*lat_INTa1 + c(rwINT_SOC2,rwINT_SOC2)*lat_SOCa1
lat_INTa3 ~ c(rwINT_EXT3,rwINT_EXT3)*lat_EXTa2 + c(rwINT_ATT3,rwINT_ATT3)*lat_ATTa2 + c(rwINT_INT3,rwINT_INT3)*lat_INTa2 + c(rwINT_SOC3,rwINT_SOC3)*lat_SOCa2
lat_SOCa2 ~ c(rwSOC_EXT2,rwSOC_EXT2)*lat_EXTa1 + c(rwSOC_ATT2,rwSOC_ATT2)*lat_ATTa1 + c(rwSOC_INT2,rwSOC_INT2)*lat_INTa1 + c(rwSOC_SOC2,rwSOC_SOC2)*lat_SOCa1
lat_SOCa3 ~ c(rwSOC_EXT3,rwSOC_EXT3)*lat_EXTa2 + c(rwSOC_ATT3,rwSOC_ATT3)*lat_ATTa2 + c(rwSOC_INT3,rwSOC_INT3)*lat_INTa2 + c(rwSOC_SOC3,rwSOC_SOC3)*lat_SOCa2

lat_EXTb2 ~ c(rwEXT_EXT2,rwEXT_EXT2)*lat_EXTb1 + c(rwEXT_ATT2,rwEXT_ATT2)*lat_ATTb1 + c(rwEXT_INT2,rwEXT_INT2)*lat_INTb1 + c(rwEXT_SOC2,rwEXT_SOC2)*lat_SOCb1
lat_EXTb3 ~ c(rwEXT_EXT3,rwEXT_EXT3)*lat_EXTb2 + c(rwEXT_ATT3,rwEXT_ATT3)*lat_ATTb2 + c(rwEXT_INT3,rwEXT_INT3)*lat_INTb2 + c(rwEXT_SOC3,rwEXT_SOC3)*lat_SOCb2
lat_ATTb2 ~ c(rwATT_EXT2,rwATT_EXT2)*lat_EXTb1 + c(rwATT_ATT2,rwATT_ATT2)*lat_ATTb1 + c(rwATT_INT2,rwATT_INT2)*lat_INTb1 + c(rwATT_SOC2,rwATT_SOC2)*lat_SOCb1
lat_ATTb3 ~ c(rwATT_EXT3,rwATT_EXT3)*lat_EXTb2 + c(rwATT_ATT3,rwATT_ATT3)*lat_ATTb2 + c(rwATT_INT3,rwATT_INT3)*lat_INTb2 + c(rwATT_SOC3,rwATT_SOC3)*lat_SOCb2
lat_INTb2 ~ c(rwINT_EXT2,rwINT_EXT2)*lat_EXTb1 + c(rwINT_ATT2,rwINT_ATT2)*lat_ATTb1 + c(rwINT_INT2,rwINT_INT2)*lat_INTb1 + c(rwINT_SOC2,rwINT_SOC2)*lat_SOCb1
lat_INTb3 ~ c(rwINT_EXT3,rwINT_EXT3)*lat_EXTb2 + c(rwINT_ATT3,rwINT_ATT3)*lat_ATTb2 + c(rwINT_INT3,rwINT_INT3)*lat_INTb2 + c(rwINT_SOC3,rwINT_SOC3)*lat_SOCb2
lat_SOCb2 ~ c(rwSOC_EXT2,rwSOC_EXT2)*lat_EXTb1 + c(rwSOC_ATT2,rwSOC_ATT2)*lat_ATTb1 + c(rwSOC_INT2,rwSOC_INT2)*lat_INTb1 + c(rwSOC_SOC2,rwSOC_SOC2)*lat_SOCb1
lat_SOCb3 ~ c(rwSOC_EXT3,rwSOC_EXT3)*lat_EXTb2 + c(rwSOC_ATT3,rwSOC_ATT3)*lat_ATTb2 + c(rwSOC_INT3,rwSOC_INT3)*lat_INTb2 + c(rwSOC_SOC3,rwSOC_SOC3)*lat_SOCb2

#regressions between - equal across twins and zygosity 

lat_EXTa2 ~ c(rbEXT_EXT2,rbEXT_EXT2)*lat_EXTb1 + c(rbEXT_ATT2,rbEXT_ATT2)*lat_ATTb1 + c(rbEXT_INT2,rbEXT_INT2)*lat_INTb1 + c(rbEXT_SOC2,rbEXT_SOC2)*lat_SOCb1
lat_EXTa3 ~ c(rbEXT_EXT3,rbEXT_EXT3)*lat_EXTb2 + c(rbEXT_ATT3,rbEXT_ATT3)*lat_ATTb2 + c(rbEXT_INT3,rbEXT_INT3)*lat_INTb2 + c(rbEXT_SOC3,rbEXT_SOC3)*lat_SOCb2
lat_ATTa2 ~ c(rbATT_EXT2,rbATT_EXT2)*lat_EXTb1 + c(rbATT_ATT2,rbATT_ATT2)*lat_ATTb1 + c(rbATT_INT2,rbATT_INT2)*lat_INTb1 + c(rbATT_SOC2,rbATT_SOC2)*lat_SOCb1
lat_ATTa3 ~ c(rbATT_EXT3,rbATT_EXT3)*lat_EXTb2 + c(rbATT_ATT3,rbATT_ATT3)*lat_ATTb2 + c(rbATT_INT3,rbATT_INT3)*lat_INTb2 + c(rbATT_SOC3,rbATT_SOC3)*lat_SOCb2
lat_INTa2 ~ c(rbINT_EXT2,rbINT_EXT2)*lat_EXTb1 + c(rbINT_ATT2,rbINT_ATT2)*lat_ATTb1 + c(rbINT_INT2,rbINT_INT2)*lat_INTb1 + c(rbINT_SOC2,rbINT_SOC2)*lat_SOCb1
lat_INTa3 ~ c(rbINT_EXT3,rbINT_EXT3)*lat_EXTb2 + c(rbINT_ATT3,rbINT_ATT3)*lat_ATTb2 + c(rbINT_INT3,rbINT_INT3)*lat_INTb2 + c(rbINT_SOC3,rbINT_SOC3)*lat_SOCb2
lat_SOCa2 ~ c(rbSOC_EXT2,rbSOC_EXT2)*lat_EXTb1 + c(rbSOC_ATT2,rbSOC_ATT2)*lat_ATTb1 + c(rbSOC_INT2,rbSOC_INT2)*lat_INTb1 + c(rbSOC_SOC2,rbSOC_SOC2)*lat_SOCb1
lat_SOCa3 ~ c(rbSOC_EXT3,rbSOC_EXT3)*lat_EXTb2 + c(rbSOC_ATT3,rbSOC_ATT3)*lat_ATTb2 + c(rbSOC_INT3,rbSOC_INT3)*lat_INTb2 + c(rbSOC_SOC3,rbSOC_SOC3)*lat_SOCb2

lat_EXTb2 ~ c(rbEXT_EXT2,rbEXT_EXT2)*lat_EXTa1 + c(rbEXT_ATT2,rbEXT_ATT2)*lat_ATTa1 + c(rbEXT_INT2,rbEXT_INT2)*lat_INTa1 + c(rbEXT_SOC2,rbEXT_SOC2)*lat_SOCa1
lat_EXTb3 ~ c(rbEXT_EXT3,rbEXT_EXT3)*lat_EXTa2 + c(rbEXT_ATT3,rbEXT_ATT3)*lat_ATTa2 + c(rbEXT_INT3,rbEXT_INT3)*lat_INTa2 + c(rbEXT_SOC3,rbEXT_SOC3)*lat_SOCa2
lat_ATTb2 ~ c(rbATT_EXT2,rbATT_EXT2)*lat_EXTa1 + c(rbATT_ATT2,rbATT_ATT2)*lat_ATTa1 + c(rbATT_INT2,rbATT_INT2)*lat_INTa1 + c(rbATT_SOC2,rbATT_SOC2)*lat_SOCa1
lat_ATTb3 ~ c(rbATT_EXT3,rbATT_EXT3)*lat_EXTa2 + c(rbATT_ATT3,rbATT_ATT3)*lat_ATTa2 + c(rbATT_INT3,rbATT_INT3)*lat_INTa2 + c(rbATT_SOC3,rbATT_SOC3)*lat_SOCa2
lat_INTb2 ~ c(rbINT_EXT2,rbINT_EXT2)*lat_EXTa1 + c(rbINT_ATT2,rbINT_ATT2)*lat_ATTa1 + c(rbINT_INT2,rbINT_INT2)*lat_INTa1 + c(rbINT_SOC2,rbINT_SOC2)*lat_SOCa1
lat_INTb3 ~ c(rbINT_EXT3,rbINT_EXT3)*lat_EXTa2 + c(rbINT_ATT3,rbINT_ATT3)*lat_ATTa2 + c(rbINT_INT3,rbINT_INT3)*lat_INTa2 + c(rbINT_SOC3,rbINT_SOC3)*lat_SOCa2
lat_SOCb2 ~ c(rbSOC_EXT2,rbSOC_EXT2)*lat_EXTa1 + c(rbSOC_ATT2,rbSOC_ATT2)*lat_ATTa1 + c(rbSOC_INT2,rbSOC_INT2)*lat_INTa1 + c(rbSOC_SOC2,rbSOC_SOC2)*lat_SOCa1
lat_SOCb3 ~ c(rbSOC_EXT3,rbSOC_EXT3)*lat_EXTa2 + c(rbSOC_ATT3,rbSOC_ATT3)*lat_ATTa2 + c(rbSOC_INT3,rbSOC_INT3)*lat_INTa2 + c(rbSOC_SOC3,rbSOC_SOC3)*lat_SOCa2

#latent variances - equal across twins and zygosity 

lat_EXTa1 ~~ c(e_EXT1,e_EXT1)*lat_EXTa1
lat_EXTa2 ~~ c(e_EXT2,e_EXT2)*lat_EXTa2
lat_EXTa3 ~~ c(e_EXT3,e_EXT3)*lat_EXTa3
lat_ATTa1 ~~ c(e_ATT1,e_ATT1)*lat_ATTa1
lat_ATTa2 ~~ c(e_ATT2,e_ATT2)*lat_ATTa2
lat_ATTa3 ~~ c(e_ATT3,e_ATT3)*lat_ATTa3
lat_INTa1 ~~ c(e_INT1,e_INT1)*lat_INTa1
lat_INTa2 ~~ c(e_INT2,e_INT2)*lat_INTa2
lat_INTa3 ~~ c(e_INT3,e_INT3)*lat_INTa3
lat_SOCa1 ~~ c(e_SOC1,e_SOC1)*lat_SOCa1
lat_SOCa2 ~~ c(e_SOC2,e_SOC2)*lat_SOCa2
lat_SOCa3 ~~ c(e_SOC3,e_SOC3)*lat_SOCa3 

lat_EXTb1 ~~ c(e_EXT1,e_EXT1)*lat_EXTb1
lat_EXTb2 ~~ c(e_EXT2,e_EXT2)*lat_EXTb2
lat_EXTb3 ~~ c(e_EXT3,e_EXT3)*lat_EXTb3
lat_ATTb1 ~~ c(e_ATT1,e_ATT1)*lat_ATTb1
lat_ATTb2 ~~ c(e_ATT2,e_ATT2)*lat_ATTb2
lat_ATTb3 ~~ c(e_ATT3,e_ATT3)*lat_ATTb3
lat_INTb1 ~~ c(e_INT1,e_INT1)*lat_INTb1
lat_INTb2 ~~ c(e_INT2,e_INT2)*lat_INTb2
lat_INTb3 ~~ c(e_INT3,e_INT3)*lat_INTb3
lat_SOCb1 ~~ c(e_SOC1,e_SOC1)*lat_SOCb1
lat_SOCb2 ~~ c(e_SOC2,e_SOC2)*lat_SOCb2
lat_SOCb3 ~~ c(e_SOC3,e_SOC3)*lat_SOCb3 

#lantent covariances within - equal across twins and zygosity 

lat_EXTa1 ~~ c(cw_EXTATT1,cw_EXTATT1)*lat_ATTa1 + c(cw_EXTINT1,cw_EXTINT1)*lat_INTa1 + c(cw_EXTSOC1,cw_EXTSOC1)*lat_SOCa1
lat_EXTa2 ~~ c(cw_EXTATT2,cw_EXTATT2)*lat_ATTa2 + c(cw_EXTINT2,cw_EXTINT2)*lat_INTa2 + c(cw_EXTSOC2,cw_EXTSOC2)*lat_SOCa2
lat_EXTa3 ~~ c(cw_EXTATT3,cw_EXTATT3)*lat_ATTa3 + c(cw_EXTINT3,cw_EXTINT3)*lat_INTa3 + c(cw_EXTSOC3,cw_EXTSOC3)*lat_SOCa3
lat_ATTa1 ~~ c(cw_ATTINT1,cw_ATTINT1)*lat_INTa1 + c(cw_ATTSOC1,cw_ATTSOC1)*lat_SOCa1
lat_ATTa2 ~~ c(cw_ATTINT2,cw_ATTINT2)*lat_INTa2 + c(cw_ATTSOC2,cw_ATTSOC2)*lat_SOCa2
lat_ATTa3 ~~ c(cw_ATTINT3,cw_ATTINT3)*lat_INTa3 + c(cw_ATTSOC3,cw_ATTSOC3)*lat_SOCa3
lat_INTa1 ~~ c(cw_INTSOC1,cw_INTSOC1)*lat_SOCa1
lat_INTa2 ~~ c(cw_INTSOC2,cw_INTSOC2)*lat_SOCa2
lat_INTa3 ~~ c(cw_INTSOC3,cw_INTSOC3)*lat_SOCa3

lat_EXTb1 ~~ c(cw_EXTATT1,cw_EXTATT1)*lat_ATTb1 + c(cw_EXTINT1,cw_EXTINT1)*lat_INTb1 + c(cw_EXTSOC1,cw_EXTSOC1)*lat_SOCb1
lat_EXTb2 ~~ c(cw_EXTATT2,cw_EXTATT2)*lat_ATTb2 + c(cw_EXTINT2,cw_EXTINT2)*lat_INTb2 + c(cw_EXTSOC2,cw_EXTSOC2)*lat_SOCb2
lat_EXTb3 ~~ c(cw_EXTATT3,cw_EXTATT3)*lat_ATTb3 + c(cw_EXTINT3,cw_EXTINT3)*lat_INTb3 + c(cw_EXTSOC3,cw_EXTSOC3)*lat_SOCb3
lat_ATTb1 ~~ c(cw_ATTINT1,cw_ATTINT1)*lat_INTb1 + c(cw_ATTSOC1,cw_ATTSOC1)*lat_SOCb1
lat_ATTb2 ~~ c(cw_ATTINT2,cw_ATTINT2)*lat_INTb2 + c(cw_ATTSOC2,cw_ATTSOC2)*lat_SOCb2
lat_ATTb3 ~~ c(cw_ATTINT3,cw_ATTINT3)*lat_INTb3 + c(cw_ATTSOC3,cw_ATTSOC3)*lat_SOCb3
lat_INTb1 ~~ c(cw_INTSOC1,cw_INTSOC1)*lat_SOCb1
lat_INTb2 ~~ c(cw_INTSOC2,cw_INTSOC2)*lat_SOCb2
lat_INTb3 ~~ c(cw_INTSOC3,cw_INTSOC3)*lat_SOCb3

#latent covariances between - different across zygosity

lat_EXTa1 ~~ c(cb_EXTEXT1_dz,cb_EXTEXT1_mz)*lat_EXTb1 + c(cb_EXTATT1_dz,cb_EXTATT1_mz)*lat_ATTb1 + c(cb_EXTINT1_dz,cb_EXTINT1_mz)*lat_INTb1 + c(cb_EXTSOC1_dz,cb_EXTSOC1_mz)*lat_SOCb1
lat_ATTa1 ~~ c(cb_EXTATT1_dz,cb_EXTATT1_mz)*lat_EXTb1 + c(cb_ATTATT1_dz,cb_ATTATT1_mz)*lat_ATTb1 + c(cb_ATTINT1_dz,cb_ATTINT1_mz)*lat_INTb1 + c(cb_ATTSOC1_dz,cb_ATTSOC1_mz)*lat_SOCb1
lat_INTa1 ~~ c(cb_EXTINT1_dz,cb_EXTINT1_mz)*lat_EXTb1 + c(cb_ATTINT1_dz,cb_ATTINT1_mz)*lat_ATTb1 + c(cb_INTINT1_dz,cb_INTINT1_mz)*lat_INTb1 + c(cb_INTSOC1_dz,cb_INTSOC1_mz)*lat_SOCb1
lat_SOCa1 ~~ c(cb_EXTSOC1_dz,cb_EXTSOC1_mz)*lat_EXTb1 + c(cb_ATTSOC1_dz,cb_ATTSOC1_mz)*lat_ATTb1 + c(cb_INTSOC1_dz,cb_INTSOC1_mz)*lat_INTb1 + c(cb_SOCSOC1_dz,cb_SOCSOC1_mz)*lat_SOCb1

lat_EXTa2 ~~ c(cb_EXTEXT2_dz,cb_EXTEXT2_mz)*lat_EXTb2 + c(cb_EXTATT2_dz,cb_EXTATT2_mz)*lat_ATTb2 + c(cb_EXTINT2_dz,cb_EXTINT2_mz)*lat_INTb2 + c(cb_EXTSOC2_dz,cb_EXTSOC2_mz)*lat_SOCb2
lat_ATTa2 ~~ c(cb_EXTATT2_dz,cb_EXTATT2_mz)*lat_EXTb2 + c(cb_ATTATT2_dz,cb_ATTATT2_mz)*lat_ATTb2 + c(cb_ATTINT2_dz,cb_ATTINT2_mz)*lat_INTb2 + c(cb_ATTSOC2_dz,cb_ATTSOC2_mz)*lat_SOCb2
lat_INTa2 ~~ c(cb_EXTINT2_dz,cb_EXTINT2_mz)*lat_EXTb2 + c(cb_ATTINT2_dz,cb_ATTINT2_mz)*lat_ATTb2 + c(cb_INTINT2_dz,cb_INTINT2_mz)*lat_INTb2 + c(cb_INTSOC2_dz,cb_INTSOC2_mz)*lat_SOCb2
lat_SOCa2 ~~ c(cb_EXTSOC2_dz,cb_EXTSOC2_mz)*lat_EXTb2 + c(cb_ATTSOC2_dz,cb_ATTSOC2_mz)*lat_ATTb2 + c(cb_INTSOC2_dz,cb_INTSOC2_mz)*lat_INTb2 + c(cb_SOCSOC2_dz,cb_SOCSOC2_mz)*lat_SOCb2

lat_EXTa3 ~~ c(cb_EXTEXT3_dz,cb_EXTEXT3_mz)*lat_EXTb3 + c(cb_EXTATT3_dz,cb_EXTATT3_mz)*lat_ATTb3 + c(cb_EXTINT3_dz,cb_EXTINT3_mz)*lat_INTb3 + c(cb_EXTSOC3_dz,cb_EXTSOC3_mz)*lat_SOCb3
lat_ATTa3 ~~ c(cb_EXTATT3_dz,cb_EXTATT3_mz)*lat_EXTb3 + c(cb_ATTATT3_dz,cb_ATTATT3_mz)*lat_ATTb3 + c(cb_ATTINT3_dz,cb_ATTINT3_mz)*lat_INTb3 + c(cb_ATTSOC3_dz,cb_ATTSOC3_mz)*lat_SOCb3
lat_INTa3 ~~ c(cb_EXTINT3_dz,cb_EXTINT3_mz)*lat_EXTb3 + c(cb_ATTINT3_dz,cb_ATTINT3_mz)*lat_ATTb3 + c(cb_INTINT3_dz,cb_INTINT3_mz)*lat_INTb3 + c(cb_INTSOC3_dz,cb_INTSOC3_mz)*lat_SOCb3
lat_SOCa3 ~~ c(cb_EXTSOC3_dz,cb_EXTSOC3_mz)*lat_EXTb3 + c(cb_ATTSOC3_dz,cb_ATTSOC3_mz)*lat_ATTb3 + c(cb_INTSOC3_dz,cb_INTSOC3_mz)*lat_INTb3 + c(cb_SOCSOC3_dz,cb_SOCSOC3_mz)*lat_SOCb3

#Random intercepts ADCE

a2EXT := 2*(bI_EXTEXT_mz-bI_EXTEXT_dz)
c2EXT :=(2*bI_EXTEXT_dz)-bI_EXTEXT_mz
e2EXT := 1-bI_EXTEXT_mz

a2ATT := (4*bI_ATTATT_dz)-bI_ATTATT_mz
d2ATT := (2*bI_ATTATT_mz) - (4*bI_ATTATT_dz)
e2ATT := 1-bI_ATTATT_mz
 
a2INT := 2*(bI_INTINT_mz-bI_INTINT_dz)
c2INT :=(2*bI_INTINT_dz)-bI_INTINT_mz
e2INT := 1-bI_INTINT_mz

a2SOC := (4*bI_SOCSOC_dz)-bI_SOCSOC_mz
d2SOC := (2*bI_SOCSOC_mz) - (4*bI_SOCSOC_dz)
e2SOC := 1-bI_SOCSOC_mz

#residuals ADCE

a1EXTr := 2*(cb_EXTEXT1_mz-cb_EXTEXT1_dz) 
c1EXTr :=(2*cb_EXTEXT1_dz)-cb_EXTEXT1_mz
e1EXTr := 1-cb_EXTEXT1_mz

a1ATTr := (4*cb_ATTATT1_dz)-cb_ATTATT1_mz
d1ATTr := (2*cb_ATTATT1_mz) - (4*cb_ATTATT1_dz)
e1ATTr := 1-cb_ATTATT1_mz

a1INTr := 2*(cb_INTINT1_mz-cb_INTINT1_dz)
c1INTr :=(2*cb_INTINT1_dz)-cb_INTINT1_mz
e1INTr := 1-cb_INTINT1_mz

a1SOCr := (4*cb_SOCSOC1_dz)-cb_SOCSOC1_mz
d1SOCr :=(2*cb_SOCSOC1_mz) - (4*cb_SOCSOC1_dz)
e1SOCr := 1-cb_SOCSOC1_mz


a2EXTr := 2*(cb_EXTEXT2_mz-cb_EXTEXT2_dz) 
c2EXTr :=(2*cb_EXTEXT2_dz)-cb_EXTEXT2_mz
e2EXTr := 1-cb_EXTEXT2_mz

a2ATTr := 2*(cb_ATTATT2_mz-cb_ATTATT2_dz) 
c2ATTr := (2*cb_ATTATT2_dz)-cb_ATTATT2_mz
e2ATTr := 1-cb_ATTATT2_mz


a2INTr := 2*(cb_INTINT2_mz-cb_INTINT2_dz)
c2INTr :=(2*cb_INTINT2_dz)-cb_INTINT2_mz
e2INTr := 1-cb_INTINT2_mz

a2SOCr := 2*(cb_SOCSOC2_mz-cb_SOCSOC2_dz)
c2SOCr :=(2*cb_SOCSOC2_dz)-cb_SOCSOC2_mz
e2SOCr := 1-cb_SOCSOC2_mz

a3EXTr := 2*(cb_EXTEXT3_mz-cb_EXTEXT3_dz) 
c3EXTr :=(2*cb_EXTEXT3_dz)-cb_EXTEXT3_mz
e3EXTr := 1-cb_EXTEXT3_mz

a3ATTr := 2*(cb_ATTATT3_mz-cb_ATTATT3_dz)
c3ATTr := (2*cb_ATTATT3_dz)-cb_ATTATT3_mz
e3ATTr := 1-cb_ATTATT3_mz

a3INTr := 2*(cb_INTINT3_mz-cb_INTINT3_dz)
c3INTr :=(2*cb_INTINT3_dz)-cb_INTINT3_mz
e3INTr := 1-cb_INTINT3_mz

a3SOCr := (4*cb_SOCSOC3_dz)-cb_SOCSOC3_mz
d3SOCr :=(2*cb_SOCSOC3_mz) - (4*cb_SOCSOC3_dz)
e3SOCr := 1-cb_SOCSOC3_mz
'