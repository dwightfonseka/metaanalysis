meta1=meta2_final
names(meta1)
#View(meta1_20200917)

########## TRANSFORMATIONS

######################## convert study 2 Beta of GEE to OR (Honghu Guan et al)

meta1$OR[1]=exp(meta1$OR[1])
meta1$OR[2]=exp(meta1$OR[2])
meta1$OR[3]=exp(meta1$OR[3])

meta1$lowCI[1]=exp(meta1$lowCI[1])
meta1$lowCI[2]=exp(meta1$lowCI[2])
meta1$lowCI[3]=exp(meta1$lowCI[3])

meta1$uppCI[1]=exp(meta1$uppCI[1])
meta1$uppCI[2]=exp(meta1$uppCI[2])
meta1$uppCI[3]=exp(meta1$uppCI[3])
#View(meta1)


######################## convert study 12 >> min/day to hr/day 
# (McCran et al)

meta1$OR[8]=exp(log(meta1$OR[8]) * 60)
meta1$uppCI[8]=exp(log(meta1$uppCI[8]) * 60)
meta1$lowCI[8]=exp(log(meta1$lowCI[8]) * 60)

#mcSE = (meta1$uppCI[10] - meta1$lowCI[10])/3.92

#meta1$uppCI[10]=exp(log(meta1$OR[10]) + (1.96 * mcSE))    # check conversion
#meta1$lowCI[10]=exp(log(meta1$OR[10]) - (1.96 * mcSE))

#meta1 = meta1[c(1:9,11:nrow(meta1)),]



#################### converting SE

meta1$SE = (log(meta1$uppCI) - log(meta1$lowCI))/3.92

#######################

library(dplyr)
meta2a = meta1 %>% filter(category=="cat1")
meta2b = meta1 %>% filter(category=="cat2")

meta2c = meta1 %>% filter(category=="cat1") %>% filter(Class=="duration")
meta2d= meta1 %>% filter(category=="cat2") %>% filter(Class=="duration")



### new ones

meta2e = meta1 %>% filter(Class=="duration") %>%
  filter(category=="cat1") %>% filter(studyType=="Cross-sectional")
meta2f = meta1 %>% filter(Class=="duration") %>%
  filter(category=="cat1") %>% filter(studyType=="Prospective")
meta2g = meta1 %>% filter(Class=="duration") %>%
    filter(studyType=="Cross-sectional")
meta2h = meta1 %>% filter(Class=="duration") %>%
   filter(studyType=="Prospective")
  

meta2i = meta1 %>% 
  filter(category=="cat1") %>% filter(studyType=="Cross-sectional")
meta2j = meta1 %>% 
  filter(category=="cat1") %>% filter(studyType=="Prospective")
meta2k = meta1 %>%
  filter(studyType=="Cross-sectional")
meta2l = meta1 %>%
  filter(studyType=="Prospective")



####################

meta3a = meta2a   # cat1   YesNo + duration
meta3b =  meta2c   # cat1   duration only
meta3c = rbind(meta2a, meta2b)   # cat1 + cat2   YesNo + duration
meta3d = rbind(meta2c, meta2d)  # cat1 + cat2    duration only

meta3u = rbind(meta2i,meta2j)
meta3p = rbind(meta2k,meta2l)

################### selection

# model 1a > all cross sectional cat 1  > meta2i
# model 1b > all propsective cat 1      > meta2j
# model 1c > screen cross sectional cat 1 > meta2e
# model 1d > screen prospectitive cat 1 > meta2f
# model 2a > all cat 1 & cat 2          > meta 3c
# model 2b > screen cat 1 & cat2        > meta 3d

meta2 = meta3d        #i,j,e,f, 3c, 3d



### create forest plot and summary function

par1 = function(meta2){
  
  library(meta)
  
  or1=as.numeric(meta2$OR)  
  ses3 =as.numeric(meta2$SE)  
  gp=meta2$studyType
  aut=meta2$Author
  samp=meta2$sampleSize
  
  par11=c(
    log(or1)
  )
  
  
  parDF=data.frame(
    rr=par11,
    se=ses3,
    group=gp,
    ss=samp,
    aut=aut
  )
 
}  
  
forest1 = function(meta2){  
   
  library(meta)
  res2 = metagen(parDF$rr, 
                 parDF$se, 
                 data=parDF,
                 byvar=parDF$group,
                 studlab=parDF$aut,
                # n.e=parDF$ss,
                 overall=TRUE,
                 comb.random=TRUE, 
                 comb.fixed=TRUE,
                 method.tau = "REML",  #DL, PM, REML, ML, HS, SJ, HE, EB
                 hakn = FALSE,
                 keepdata= TRUE,
                 prediction = FALSE,
                 sm = "OR")
  
  #jpeg("C://Users//d_n_f//Desktop//rplot.jpg", width = 750, height = 750)
  
  
  forest.meta(res2,
         leftcols = c('studlab','rr','se'),
         leftlabs = c("Author","log(OR)","SE"),
         digits.pval=2
           )
  

  #dev.off()
  
  s1=summary.meta(res2,
               overall.hetstat = TRUE, 
               overall = TRUE)

  print(s1)
  
  return(res2)
  
}


### bias and funnel plot function

bias1=function(parDF, res2){ 
  
 m1= metabias(
    parDF$rr, 
    parDF$se,
    method.bias = "rank",
    plotit = TRUE,
    correct = TRUE,
    k.min = 3
  )

 print(m1)
  
  
  ############## pblication bias #############
  
  funnel(res2,
        ylim = c(0.8,0)
  )
  
  #trimfill(res2)
  #metabias(res2)
  #devtools::install_github("MathiasHarrer/dmetar")
  
  #library(dmetar)
  #eggers.test(res2)
  
}
 

sens1 = function(res2){
 
  ################## leave one out
  
  met1= metainf(res2, pooled="random",)
  forest(met1)
  

  ###### subgroup analysis ##############
  
  #subgp = update.meta(res2, byvar=parDF$gp)
  #print(subgp)
  
}


################### RUNNING THE FUNCTIONS ################3

parDF = par1(meta2)
res2=forest1(parDF)
bias1(parDF, res2)
sens1(res2)




#################


#method.tau = "DL"	DerSimonian-Laird estimator (DerSimonian and Laird, 1986)
#method.tau = "PM"	Paule-Mandel estimator (Paule and Mandel, 1982)
#method.tau = "REML"	Restricted maximum-likelihood estimator (Viechtbauer, 2005)
#method.tau = "ML"	Maximum-likelihood estimator (Viechtbauer, 2005)
#method.tau = "HS"	Hunter-Schmidt estimator (Hunter and Schmidt, 2015)
#method.tau = "SJ"	Sidik-Jonkman estimator (Sidik and Jonkman, 2005)
#method.tau = "HE"	Hedges estimator (Hedges and Olkin, 1985)
#method.tau = "EB"	Empirical Bayes estimator (Morris, 1983)



#'"RR"' for the _log risk ratio_.

#'"OR"' for the _log odds ratio_.

#'"RD"' for the _risk difference_.

#'"AS"' for the _arcsine square root transformed risk
#difference_ (Rücker et al., 2009).

#'"PETO"' for the _log odds ratio_ estimated with Peto's
#         method (Yusuf et al., 1985).

# "MD" for mean difference


################ methodology

# ?metagen
# metabin
# metacont

