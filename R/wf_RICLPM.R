wf_RICLPM <- '
#random interecepts for sib 1

ri_CNDa =~ 1*CON_t1a + 1*CON_t2a + 1*CON_t3a
ri_HYPa =~ 1*HYP_t1a + 1*HYP_t2a + 1*HYP_t3a
ri_EMOa =~ 1*EMp_t1a + 1*EMp_t2a + 1*EMp_t3a
ri_PERa =~ 1*PEp_t1a + 1*PEp_t2a + 1*PEp_t3a

#random interecepts for sib 2

ri_CNDb =~ 1*CON_t1b + 1*CON_t2b + 1*CON_t3b
ri_HYPb =~ 1*HYP_t1b + 1*HYP_t2b + 1*HYP_t3b
ri_EMOb =~ 1*EMp_t1b + 1*EMp_t2b + 1*EMp_t3b
ri_PERb =~ 1*PEp_t1b + 1*PEp_t2b + 1*PEp_t3b

#RI variances equal across twins and zygosity 

ri_CNDa ~~ c(vI_CND,vI_CND)*ri_CNDa
ri_HYPa ~~ c(vI_HYP,vI_HYP)*ri_HYPa
ri_EMOa ~~ c(vI_EMO,vI_EMO)*ri_EMOa
ri_PERa ~~ c(vI_PER,vI_PER)*ri_PERa

ri_CNDb ~~ c(vI_CND,vI_CND)*ri_CNDb
ri_HYPb ~~ c(vI_HYP,vI_HYP)*ri_HYPb
ri_EMOb ~~ c(vI_EMO,vI_EMO)*ri_EMOb
ri_PERb ~~ c(vI_PER,vI_PER)*ri_PERb

#RI covariances within sibling

ri_CNDa ~~ c(wI_CNDHYP,wI_CNDHYP)*ri_HYPa + c(wI_CNDEMO,wI_CNDEMO)*ri_EMOa + c(wI_CNDPER,wI_CNDPER)*ri_PERa
ri_HYPa ~~ c(wI_HYPEMO,wI_HYPEMO)*ri_EMOa + c(wI_HYPPER,wI_HYPPER)*ri_PERa
ri_EMOa ~~ c(wI_EMOPER,wI_EMOPER)*ri_PERa

ri_CNDb ~~ c(wI_CNDHYP,wI_CNDHYP)*ri_HYPb + c(wI_CNDEMO,wI_CNDEMO)*ri_EMOb + c(wI_CNDPER,wI_CNDPER)*ri_PERb
ri_HYPb ~~ c(wI_HYPEMO,wI_HYPEMO)*ri_EMOb + c(wI_HYPPER,wI_HYPPER)*ri_PERb
ri_EMOb ~~ c(wI_EMOPER,wI_EMOPER)*ri_PERb

#RI covariances between siblings

ri_CNDa ~~ c(bI_CNDCND_dz,bI_CNDCND_mz)*ri_CNDb + c(bI_CNDHYP_dz,bI_CNDHYP_mz)*ri_HYPb + c(bI_CNDEMO_dz,bI_CNDEMO_mz)*ri_EMOb + c(bI_CNDPER_dz,bI_CNDPER_mz)*ri_PERb
ri_HYPa ~~ c(bI_CNDHYP_dz,bI_CNDHYP_mz)*ri_CNDb + c(bI_HYPHYP_dz,bI_HYPHYP_mz)*ri_HYPb + c(bI_HYPEMO_dz,bI_HYPEMO_mz)*ri_EMOb + c(bI_HYPPER_dz,bI_HYPPER_mz)*ri_PERb
ri_EMOa ~~ c(bI_CNDEMO_dz,bI_CNDEMO_mz)*ri_CNDb + c(bI_HYPEMO_dz,bI_HYPEMO_mz)*ri_HYPb + c(bI_EMOEMO_dz,bI_EMOEMO_mz)*ri_EMOb + c(bI_EMOPER_dz,bI_EMOPER_mz)*ri_PERb
ri_PERa ~~ c(bI_CNDPER_dz,bI_CNDPER_mz)*ri_CNDb + c(bI_HYPPER_dz,bI_HYPPER_mz)*ri_HYPb + c(bI_EMOPER_dz,bI_EMOPER_mz)*ri_EMOb + c(bI_PERPER_dz,bI_PERPER_mz)*ri_PERb

# fixed intercepts equal across twins and zygosity 

CON_t1a ~ CON_t1_mu*1
CON_t2a ~ CON_t2_mu*1
CON_t3a ~ CON_t3_mu*1
HYP_t1a ~ HYP_t1_mu*1
HYP_t2a ~ HYP_t2_mu*1
HYP_t3a ~ HYP_t3_mu*1
EMp_t1a ~ EMp_t1_mu*1
EMp_t2a ~ EMp_t2_mu*1
EMp_t3a ~ EMp_t3_mu*1
PEp_t1a ~ PEp_t1_mu*1
PEp_t2a ~ PEp_t2_mu*1
PEp_t3a ~ PEp_t3_mu*1

CON_t1b ~ CON_t1_mu*1
CON_t2b ~ CON_t2_mu*1
CON_t3b ~ CON_t3_mu*1
HYP_t1b ~ HYP_t1_mu*1
HYP_t2b ~ HYP_t2_mu*1
HYP_t3b ~ HYP_t3_mu*1
EMp_t1b ~ EMp_t1_mu*1
EMp_t2b ~ EMp_t2_mu*1
EMp_t3b ~ EMp_t3_mu*1
PEp_t1b ~ PEp_t1_mu*1
PEp_t2b ~ PEp_t2_mu*1
PEp_t3b ~ PEp_t3_mu*1

#latent variables from observed loadings fixed to 1 

lat_CNDa1 =~ 1*CON_t1a
lat_CNDa2 =~ 1*CON_t2a
lat_CNDa3 =~ 1*CON_t3a
lat_HYPa1 =~ 1*HYP_t1a
lat_HYPa2 =~ 1*HYP_t2a
lat_HYPa3 =~ 1*HYP_t3a
lat_EMOa1 =~ 1*EMp_t1a
lat_EMOa2 =~ 1*EMp_t2a
lat_EMOa3 =~ 1*EMp_t3a
lat_PERa1 =~ 1*PEp_t1a
lat_PERa2 =~ 1*PEp_t2a
lat_PERa3 =~ 1*PEp_t3a

lat_CNDb1 =~ 1*CON_t1b
lat_CNDb2 =~ 1*CON_t2b
lat_CNDb3 =~ 1*CON_t3b
lat_HYPb1 =~ 1*HYP_t1b
lat_HYPb2 =~ 1*HYP_t2b
lat_HYPb3 =~ 1*HYP_t3b
lat_EMOb1 =~ 1*EMp_t1b
lat_EMOb2 =~ 1*EMp_t2b
lat_EMOb3 =~ 1*EMp_t3b
lat_PERb1 =~ 1*PEp_t1b
lat_PERb2 =~ 1*PEp_t2b
lat_PERb3 =~ 1*PEp_t3b

#regressions within - equal across twins and zygosity  

lat_CNDa2 ~ c(rwCND_CND2,rwCND_CND2)*lat_CNDa1 + c(rwCND_HYP2,rwCND_HYP2)*lat_HYPa1 + c(rwCND_EMO2,rwCND_EMO2)*lat_EMOa1 + c(rwCND_PER2,rwCND_PER2)*lat_PERa1
lat_CNDa3 ~ c(rwCND_CND3,rwCND_CND3)*lat_CNDa2 + c(rwCND_HYP3,rwCND_HYP3)*lat_HYPa2 + c(rwCND_EMO3,rwCND_EMO3)*lat_EMOa2 + c(rwCND_PER3,rwCND_PER3)*lat_PERa2
lat_HYPa2 ~ c(rwHYP_CND2,rwHYP_CND2)*lat_CNDa1 + c(rwHYP_HYP2,rwHYP_HYP2)*lat_HYPa1 + c(rwHYP_EMO2,rwHYP_EMO2)*lat_EMOa1 + c(rwHYP_PER2,rwHYP_PER2)*lat_PERa1
lat_HYPa3 ~ c(rwHYP_CND3,rwHYP_CND3)*lat_CNDa2 + c(rwHYP_HYP3,rwHYP_HYP3)*lat_HYPa2 + c(rwHYP_EMO3,rwHYP_EMO3)*lat_EMOa2 + c(rwHYP_PER3,rwHYP_PER3)*lat_PERa2
lat_EMOa2 ~ c(rwEMO_CND2,rwEMO_CND2)*lat_CNDa1 + c(rwEMO_HYP2,rwEMO_HYP2)*lat_HYPa1 + c(rwEMO_EMO2,rwEMO_EMO2)*lat_EMOa1 + c(rwEMO_PER2,rwEMO_PER2)*lat_PERa1
lat_EMOa3 ~ c(rwEMO_CND3,rwEMO_CND3)*lat_CNDa2 + c(rwEMO_HYP3,rwEMO_HYP3)*lat_HYPa2 + c(rwEMO_EMO3,rwEMO_EMO3)*lat_EMOa2 + c(rwEMO_PER3,rwEMO_PER3)*lat_PERa2
lat_PERa2 ~ c(rwPER_CND2,rwPER_CND2)*lat_CNDa1 + c(rwPER_HYP2,rwPER_HYP2)*lat_HYPa1 + c(rwPER_EMO2,rwPER_EMO2)*lat_EMOa1 + c(rwPER_PER2,rwPER_PER2)*lat_PERa1
lat_PERa3 ~ c(rwPER_CND3,rwPER_CND3)*lat_CNDa2 + c(rwPER_HYP3,rwPER_HYP3)*lat_HYPa2 + c(rwPER_EMO3,rwPER_EMO3)*lat_EMOa2 + c(rwPER_PER3,rwPER_PER3)*lat_PERa2

lat_CNDb2 ~ c(rwCND_CND2,rwCND_CND2)*lat_CNDb1 + c(rwCND_HYP2,rwCND_HYP2)*lat_HYPb1 + c(rwCND_EMO2,rwCND_EMO2)*lat_EMOb1 + c(rwCND_PER2,rwCND_PER2)*lat_PERb1
lat_CNDb3 ~ c(rwCND_CND3,rwCND_CND3)*lat_CNDb2 + c(rwCND_HYP3,rwCND_HYP3)*lat_HYPb2 + c(rwCND_EMO3,rwCND_EMO3)*lat_EMOb2 + c(rwCND_PER3,rwCND_PER3)*lat_PERb2
lat_HYPb2 ~ c(rwHYP_CND2,rwHYP_CND2)*lat_CNDb1 + c(rwHYP_HYP2,rwHYP_HYP2)*lat_HYPb1 + c(rwHYP_EMO2,rwHYP_EMO2)*lat_EMOb1 + c(rwHYP_PER2,rwHYP_PER2)*lat_PERb1
lat_HYPb3 ~ c(rwHYP_CND3,rwHYP_CND3)*lat_CNDb2 + c(rwHYP_HYP3,rwHYP_HYP3)*lat_HYPb2 + c(rwHYP_EMO3,rwHYP_EMO3)*lat_EMOb2 + c(rwHYP_PER3,rwHYP_PER3)*lat_PERb2
lat_EMOb2 ~ c(rwEMO_CND2,rwEMO_CND2)*lat_CNDb1 + c(rwEMO_HYP2,rwEMO_HYP2)*lat_HYPb1 + c(rwEMO_EMO2,rwEMO_EMO2)*lat_EMOb1 + c(rwEMO_PER2,rwEMO_PER2)*lat_PERb1
lat_EMOb3 ~ c(rwEMO_CND3,rwEMO_CND3)*lat_CNDb2 + c(rwEMO_HYP3,rwEMO_HYP3)*lat_HYPb2 + c(rwEMO_EMO3,rwEMO_EMO3)*lat_EMOb2 + c(rwEMO_PER3,rwEMO_PER3)*lat_PERb2
lat_PERb2 ~ c(rwPER_CND2,rwPER_CND2)*lat_CNDb1 + c(rwPER_HYP2,rwPER_HYP2)*lat_HYPb1 + c(rwPER_EMO2,rwPER_EMO2)*lat_EMOb1 + c(rwPER_PER2,rwPER_PER2)*lat_PERb1
lat_PERb3 ~ c(rwPER_CND3,rwPER_CND3)*lat_CNDb2 + c(rwPER_HYP3,rwPER_HYP3)*lat_HYPb2 + c(rwPER_EMO3,rwPER_EMO3)*lat_EMOb2 + c(rwPER_PER3,rwPER_PER3)*lat_PERb2

#regressions between - equal across twins and zygosity 

lat_CNDa2 ~ c(rbCND_CND2,rbCND_CND2)*lat_CNDb1 + c(rbCND_HYP2,rbCND_HYP2)*lat_HYPb1 + c(rbCND_EMO2,rbCND_EMO2)*lat_EMOb1 + c(rbCND_PER2,rbCND_PER2)*lat_PERb1
lat_CNDa3 ~ c(rbCND_CND3,rbCND_CND3)*lat_CNDb2 + c(rbCND_HYP3,rbCND_HYP3)*lat_HYPb2 + c(rbCND_EMO3,rbCND_EMO3)*lat_EMOb2 + c(rbCND_PER3,rbCND_PER3)*lat_PERb2
lat_HYPa2 ~ c(rbHYP_CND2,rbHYP_CND2)*lat_CNDb1 + c(rbHYP_HYP2,rbHYP_HYP2)*lat_HYPb1 + c(rbHYP_EMO2,rbHYP_EMO2)*lat_EMOb1 + c(rbHYP_PER2,rbHYP_PER2)*lat_PERb1
lat_HYPa3 ~ c(rbHYP_CND3,rbHYP_CND3)*lat_CNDb2 + c(rbHYP_HYP3,rbHYP_HYP3)*lat_HYPb2 + c(rbHYP_EMO3,rbHYP_EMO3)*lat_EMOb2 + c(rbHYP_PER3,rbHYP_PER3)*lat_PERb2
lat_EMOa2 ~ c(rbEMO_CND2,rbEMO_CND2)*lat_CNDb1 + c(rbEMO_HYP2,rbEMO_HYP2)*lat_HYPb1 + c(rbEMO_EMO2,rbEMO_EMO2)*lat_EMOb1 + c(rbEMO_PER2,rbEMO_PER2)*lat_PERb1
lat_EMOa3 ~ c(rbEMO_CND3,rbEMO_CND3)*lat_CNDb2 + c(rbEMO_HYP3,rbEMO_HYP3)*lat_HYPb2 + c(rbEMO_EMO3,rbEMO_EMO3)*lat_EMOb2 + c(rbEMO_PER3,rbEMO_PER3)*lat_PERb2
lat_PERa2 ~ c(rbPER_CND2,rbPER_CND2)*lat_CNDb1 + c(rbPER_HYP2,rbPER_HYP2)*lat_HYPb1 + c(rbPER_EMO2,rbPER_EMO2)*lat_EMOb1 + c(rbPER_PER2,rbPER_PER2)*lat_PERb1
lat_PERa3 ~ c(rbPER_CND3,rbPER_CND3)*lat_CNDb2 + c(rbPER_HYP3,rbPER_HYP3)*lat_HYPb2 + c(rbPER_EMO3,rbPER_EMO3)*lat_EMOb2 + c(rbPER_PER3,rbPER_PER3)*lat_PERb2

lat_CNDb2 ~ c(rbCND_CND2,rbCND_CND2)*lat_CNDa1 + c(rbCND_HYP2,rbCND_HYP2)*lat_HYPa1 + c(rbCND_EMO2,rbCND_EMO2)*lat_EMOa1 + c(rbCND_PER2,rbCND_PER2)*lat_PERa1
lat_CNDb3 ~ c(rbCND_CND3,rbCND_CND3)*lat_CNDa2 + c(rbCND_HYP3,rbCND_HYP3)*lat_HYPa2 + c(rbCND_EMO3,rbCND_EMO3)*lat_EMOa2 + c(rbCND_PER3,rbCND_PER3)*lat_PERa2
lat_HYPb2 ~ c(rbHYP_CND2,rbHYP_CND2)*lat_CNDa1 + c(rbHYP_HYP2,rbHYP_HYP2)*lat_HYPa1 + c(rbHYP_EMO2,rbHYP_EMO2)*lat_EMOa1 + c(rbHYP_PER2,rbHYP_PER2)*lat_PERa1
lat_HYPb3 ~ c(rbHYP_CND3,rbHYP_CND3)*lat_CNDa2 + c(rbHYP_HYP3,rbHYP_HYP3)*lat_HYPa2 + c(rbHYP_EMO3,rbHYP_EMO3)*lat_EMOa2 + c(rbHYP_PER3,rbHYP_PER3)*lat_PERa2
lat_EMOb2 ~ c(rbEMO_CND2,rbEMO_CND2)*lat_CNDa1 + c(rbEMO_HYP2,rbEMO_HYP2)*lat_HYPa1 + c(rbEMO_EMO2,rbEMO_EMO2)*lat_EMOa1 + c(rbEMO_PER2,rbEMO_PER2)*lat_PERa1
lat_EMOb3 ~ c(rbEMO_CND3,rbEMO_CND3)*lat_CNDa2 + c(rbEMO_HYP3,rbEMO_HYP3)*lat_HYPa2 + c(rbEMO_EMO3,rbEMO_EMO3)*lat_EMOa2 + c(rbEMO_PER3,rbEMO_PER3)*lat_PERa2
lat_PERb2 ~ c(rbPER_CND2,rbPER_CND2)*lat_CNDa1 + c(rbPER_HYP2,rbPER_HYP2)*lat_HYPa1 + c(rbPER_EMO2,rbPER_EMO2)*lat_EMOa1 + c(rbPER_PER2,rbPER_PER2)*lat_PERa1
lat_PERb3 ~ c(rbPER_CND3,rbPER_CND3)*lat_CNDa2 + c(rbPER_HYP3,rbPER_HYP3)*lat_HYPa2 + c(rbPER_EMO3,rbPER_EMO3)*lat_EMOa2 + c(rbPER_PER3,rbPER_PER3)*lat_PERa2

#latent variances - equal across twins and zygosity 

lat_CNDa1 ~~ c(e_CND1,e_CND1)*lat_CNDa1
lat_CNDa2 ~~ c(e_CND2,e_CND2)*lat_CNDa2
lat_CNDa3 ~~ c(e_CND3,e_CND3)*lat_CNDa3
lat_HYPa1 ~~ c(e_HYP1,e_HYP1)*lat_HYPa1
lat_HYPa2 ~~ c(e_HYP2,e_HYP2)*lat_HYPa2
lat_HYPa3 ~~ c(e_HYP3,e_HYP3)*lat_HYPa3
lat_EMOa1 ~~ c(e_EMO1,e_EMO1)*lat_EMOa1
lat_EMOa2 ~~ c(e_EMO2,e_EMO2)*lat_EMOa2
lat_EMOa3 ~~ c(e_EMO3,e_EMO3)*lat_EMOa3
lat_PERa1 ~~ c(e_PER1,e_PER1)*lat_PERa1
lat_PERa2 ~~ c(e_PER2,e_PER2)*lat_PERa2
lat_PERa3 ~~ c(e_PER3,e_PER3)*lat_PERa3 

lat_CNDb1 ~~ c(e_CND1,e_CND1)*lat_CNDb1
lat_CNDb2 ~~ c(e_CND2,e_CND2)*lat_CNDb2
lat_CNDb3 ~~ c(e_CND3,e_CND3)*lat_CNDb3
lat_HYPb1 ~~ c(e_HYP1,e_HYP1)*lat_HYPb1
lat_HYPb2 ~~ c(e_HYP2,e_HYP2)*lat_HYPb2
lat_HYPb3 ~~ c(e_HYP3,e_HYP3)*lat_HYPb3
lat_EMOb1 ~~ c(e_EMO1,e_EMO1)*lat_EMOb1
lat_EMOb2 ~~ c(e_EMO2,e_EMO2)*lat_EMOb2
lat_EMOb3 ~~ c(e_EMO3,e_EMO3)*lat_EMOb3
lat_PERb1 ~~ c(e_PER1,e_PER1)*lat_PERb1
lat_PERb2 ~~ c(e_PER2,e_PER2)*lat_PERb2
lat_PERb3 ~~ c(e_PER3,e_PER3)*lat_PERb3 

#lantent covariances within - equal across twins and zygosity 

lat_CNDa1 ~~ c(cw_CNDHYP1,cw_CNDHYP1)*lat_HYPa1 + c(cw_CNDEMO1,cw_CNDEMO1)*lat_EMOa1 + c(cw_CNDPER1,cw_CNDPER1)*lat_PERa1
lat_CNDa2 ~~ c(cw_CNDHYP2,cw_CNDHYP2)*lat_HYPa2 + c(cw_CNDEMO2,cw_CNDEMO2)*lat_EMOa2 + c(cw_CNDPER2,cw_CNDPER2)*lat_PERa2
lat_CNDa3 ~~ c(cw_CNDHYP3,cw_CNDHYP3)*lat_HYPa3 + c(cw_CNDEMO3,cw_CNDEMO3)*lat_EMOa3 + c(cw_CNDPER3,cw_CNDPER3)*lat_PERa3
lat_HYPa1 ~~ c(cw_HYPEMO1,cw_HYPEMO1)*lat_EMOa1 + c(cw_HYPPER1,cw_HYPPER1)*lat_PERa1
lat_HYPa2 ~~ c(cw_HYPEMO2,cw_HYPEMO2)*lat_EMOa2 + c(cw_HYPPER2,cw_HYPPER2)*lat_PERa2
lat_HYPa3 ~~ c(cw_HYPEMO3,cw_HYPEMO3)*lat_EMOa3 + c(cw_HYPPER3,cw_HYPPER3)*lat_PERa3
lat_EMOa1 ~~ c(cw_EMOPER1,cw_EMOPER1)*lat_PERa1
lat_EMOa2 ~~ c(cw_EMOPER2,cw_EMOPER2)*lat_PERa2
lat_EMOa3 ~~ c(cw_EMOPER3,cw_EMOPER3)*lat_PERa3

lat_CNDb1 ~~ c(cw_CNDHYP1,cw_CNDHYP1)*lat_HYPb1 + c(cw_CNDEMO1,cw_CNDEMO1)*lat_EMOb1 + c(cw_CNDPER1,cw_CNDPER1)*lat_PERb1
lat_CNDb2 ~~ c(cw_CNDHYP2,cw_CNDHYP2)*lat_HYPb2 + c(cw_CNDEMO2,cw_CNDEMO2)*lat_EMOb2 + c(cw_CNDPER2,cw_CNDPER2)*lat_PERb2
lat_CNDb3 ~~ c(cw_CNDHYP3,cw_CNDHYP3)*lat_HYPb3 + c(cw_CNDEMO3,cw_CNDEMO3)*lat_EMOb3 + c(cw_CNDPER3,cw_CNDPER3)*lat_PERb3
lat_HYPb1 ~~ c(cw_HYPEMO1,cw_HYPEMO1)*lat_EMOb1 + c(cw_HYPPER1,cw_HYPPER1)*lat_PERb1
lat_HYPb2 ~~ c(cw_HYPEMO2,cw_HYPEMO2)*lat_EMOb2 + c(cw_HYPPER2,cw_HYPPER2)*lat_PERb2
lat_HYPb3 ~~ c(cw_HYPEMO3,cw_HYPEMO3)*lat_EMOb3 + c(cw_HYPPER3,cw_HYPPER3)*lat_PERb3
lat_EMOb1 ~~ c(cw_EMOPER1,cw_EMOPER1)*lat_PERb1
lat_EMOb2 ~~ c(cw_EMOPER2,cw_EMOPER2)*lat_PERb2
lat_EMOb3 ~~ c(cw_EMOPER3,cw_EMOPER3)*lat_PERb3

#latent covariances between - different across zygosity

lat_CNDa1 ~~ c(cb_CNDCND1_dz,cb_CNDCND1_mz)*lat_CNDb1 + c(cb_CNDHYP1_dz,cb_CNDHYP1_mz)*lat_HYPb1 + c(cb_CNDEMO1_dz,cb_CNDEMO1_mz)*lat_EMOb1 + c(cb_CNDPER1_dz,cb_CNDPER1_mz)*lat_PERb1
lat_HYPa1 ~~ c(cb_CNDHYP1_dz,cb_CNDHYP1_mz)*lat_CNDb1 + c(cb_HYPHYP1_dz,cb_HYPHYP1_mz)*lat_HYPb1 + c(cb_HYPEMO1_dz,cb_HYPEMO1_mz)*lat_EMOb1 + c(cb_HYPPER1_dz,cb_HYPPER1_mz)*lat_PERb1
lat_EMOa1 ~~ c(cb_CNDEMO1_dz,cb_CNDEMO1_mz)*lat_CNDb1 + c(cb_HYPEMO1_dz,cb_HYPEMO1_mz)*lat_HYPb1 + c(cb_EMOEMO1_dz,cb_EMOEMO1_mz)*lat_EMOb1 + c(cb_EMOPER1_dz,cb_EMOPER1_mz)*lat_PERb1
lat_PERa1 ~~ c(cb_CNDPER1_dz,cb_CNDPER1_mz)*lat_CNDb1 + c(cb_HYPPER1_dz,cb_HYPPER1_mz)*lat_HYPb1 + c(cb_EMOPER1_dz,cb_EMOPER1_mz)*lat_EMOb1 + c(cb_PERPER1_dz,cb_PERPER1_mz)*lat_PERb1

lat_CNDa2 ~~ c(cb_CNDCND2_dz,cb_CNDCND2_mz)*lat_CNDb2 + c(cb_CNDHYP2_dz,cb_CNDHYP2_mz)*lat_HYPb2 + c(cb_CNDEMO2_dz,cb_CNDEMO2_mz)*lat_EMOb2 + c(cb_CNDPER2_dz,cb_CNDPER2_mz)*lat_PERb2
lat_HYPa2 ~~ c(cb_CNDHYP2_dz,cb_CNDHYP2_mz)*lat_CNDb2 + c(cb_HYPHYP2_dz,cb_HYPHYP2_mz)*lat_HYPb2 + c(cb_HYPEMO2_dz,cb_HYPEMO2_mz)*lat_EMOb2 + c(cb_HYPPER2_dz,cb_HYPPER2_mz)*lat_PERb2
lat_EMOa2 ~~ c(cb_CNDEMO2_dz,cb_CNDEMO2_mz)*lat_CNDb2 + c(cb_HYPEMO2_dz,cb_HYPEMO2_mz)*lat_HYPb2 + c(cb_EMOEMO2_dz,cb_EMOEMO2_mz)*lat_EMOb2 + c(cb_EMOPER2_dz,cb_EMOPER2_mz)*lat_PERb2
lat_PERa2 ~~ c(cb_CNDPER2_dz,cb_CNDPER2_mz)*lat_CNDb2 + c(cb_HYPPER2_dz,cb_HYPPER2_mz)*lat_HYPb2 + c(cb_EMOPER2_dz,cb_EMOPER2_mz)*lat_EMOb2 + c(cb_PERPER2_dz,cb_PERPER2_mz)*lat_PERb2

lat_CNDa3 ~~ c(cb_CNDCND3_dz,cb_CNDCND3_mz)*lat_CNDb3 + c(cb_CNDHYP3_dz,cb_CNDHYP3_mz)*lat_HYPb3 + c(cb_CNDEMO3_dz,cb_CNDEMO3_mz)*lat_EMOb3 + c(cb_CNDPER3_dz,cb_CNDPER3_mz)*lat_PERb3
lat_HYPa3 ~~ c(cb_CNDHYP3_dz,cb_CNDHYP3_mz)*lat_CNDb3 + c(cb_HYPHYP3_dz,cb_HYPHYP3_mz)*lat_HYPb3 + c(cb_HYPEMO3_dz,cb_HYPEMO3_mz)*lat_EMOb3 + c(cb_HYPPER3_dz,cb_HYPPER3_mz)*lat_PERb3
lat_EMOa3 ~~ c(cb_CNDEMO3_dz,cb_CNDEMO3_mz)*lat_CNDb3 + c(cb_HYPEMO3_dz,cb_HYPEMO3_mz)*lat_HYPb3 + c(cb_EMOEMO3_dz,cb_EMOEMO3_mz)*lat_EMOb3 + c(cb_EMOPER3_dz,cb_EMOPER3_mz)*lat_PERb3
lat_PERa3 ~~ c(cb_CNDPER3_dz,cb_CNDPER3_mz)*lat_CNDb3 + c(cb_HYPPER3_dz,cb_HYPPER3_mz)*lat_HYPb3 + c(cb_EMOPER3_dz,cb_EMOPER3_mz)*lat_EMOb3 + c(cb_PERPER3_dz,cb_PERPER3_mz)*lat_PERb3

#Random intercepts ADCE

a2CND := 2*(bI_CNDCND_mz-bI_CNDCND_dz)
c2CND :=(2*bI_CNDCND_dz)-bI_CNDCND_mz
e2CND := 1-bI_CNDCND_mz

a2HYP := (4*bI_HYPHYP_dz)-bI_HYPHYP_mz
d2HYP := (2*bI_HYPHYP_mz) - (4*bI_HYPHYP_dz)
e2HYP := 1-bI_HYPHYP_mz
 
a2EMO := 2*(bI_EMOEMO_mz-bI_EMOEMO_dz)
c2EMO :=(2*bI_EMOEMO_dz)-bI_EMOEMO_mz
e2EMO := 1-bI_EMOEMO_mz

a2PER := (4*bI_PERPER_dz)-bI_PERPER_mz
d2PER := (2*bI_PERPER_mz) - (4*bI_PERPER_dz)
e2PER := 1-bI_PERPER_mz


#age specific ADCE

a1cnd := 2*(cb_CNDCND1_mz-cb_CNDCND1_dz) 
c1cnd :=(2*cb_CNDCND1_dz)-cb_CNDCND1_mz
e1cnd := 1-cb_CNDCND1_mz

a1hyp := (4*cb_HYPHYP1_dz)-cb_HYPHYP1_mz
d1hyp := (2*cb_HYPHYP1_mz) - (4*cb_HYPHYP1_dz)
e1hyp := 1-cb_HYPHYP1_mz

a1emo := 2*(cb_EMOEMO1_mz-cb_EMOEMO1_dz)
c1emo :=(2*cb_EMOEMO1_dz)-cb_EMOEMO1_mz
e1emo := 1-cb_EMOEMO1_mz

a1per := 2*(cb_PERPER1_mz-cb_PERPER1_dz)
c1per :=(2*cb_PERPER1_dz)-cb_PERPER1_mz
e1per := 1-cb_PERPER1_mz

a2cnd := 2*(cb_CNDCND2_mz-cb_CNDCND2_dz) 
c2cnd :=(2*cb_CNDCND2_dz)-cb_CNDCND2_mz
e2cnd := 1-cb_CNDCND2_mz

a2hyp := (4*cb_HYPHYP2_dz)-cb_HYPHYP2_mz
c2hyp :=(2*cb_HYPHYP2_dz)-cb_HYPHYP2_mz 
e2hyp := 1-cb_HYPHYP2_mz

a2emo := 2*(cb_EMOEMO2_mz-cb_EMOEMO2_dz)
c2emo :=(2*cb_EMOEMO2_dz)-cb_EMOEMO2_mz
e2emo := 1-cb_EMOEMO2_mz

a2per := 2*(cb_PERPER2_mz-cb_PERPER2_dz)
c2per :=(2*cb_PERPER2_dz)-cb_PERPER2_mz
e2per := 1-cb_PERPER2_mz

a3cnd := 2*(cb_CNDCND3_mz-cb_CNDCND3_dz) 
c3cnd :=(2*cb_CNDCND3_dz)-cb_CNDCND3_mz
e3cnd := 1-cb_CNDCND3_mz

a3hyp := (4*cb_HYPHYP3_dz)-cb_HYPHYP3_mz
c3hyp :=(2*cb_HYPHYP3_dz)-cb_HYPHYP3_mz 
e3hyp := 1-cb_HYPHYP3_mz

a3emo := 2*(cb_EMOEMO3_mz-cb_EMOEMO3_dz)
c3emo :=(2*cb_EMOEMO3_dz)-cb_EMOEMO3_mz
e3emo := 1-cb_EMOEMO3_mz

a3per := 2*(cb_PERPER3_mz-cb_PERPER3_dz)
c3per :=(2*cb_PERPER3_dz)-cb_PERPER3_mz
e3per := 1-cb_PERPER3_mz
'

