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

meta1a = meta1 %>% filter(category=="cat1") %>%
  filter(Class=="duration")

meta1a1 = meta1 %>% filter(category=="cat1") %>%
  filter(Class=="duration") %>% filter(studyType=="Cross-sectional")

meta1a2 = meta1 %>% filter(category=="cat1") %>%
  filter(Class=="duration") %>% filter(studyType=="Prospective")



meta2a = meta1 %>% filter(Class=="duration")

meta2a1 = meta1 %>% 
  filter(Class=="duration") %>% filter(studyType=="Cross-sectional")

meta2a2 = meta1 %>%
  filter(Class=="duration") %>% filter(studyType=="Prospective")


### create forest plot and summary function

options(OutDec="\xB7")

model = function(meta2){
  
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

  library(meta)
  res2 = metagen(parDF$rr, 
                 parDF$se, 
                 data=parDF,
               #  byvar=parDF$group,
                 studlab=parDF$aut,
                 n.e=parDF$ss,
                 overall=TRUE,
                 comb.random=TRUE, 
                 comb.fixed=TRUE,
                 method.tau = "REML",  #DL, PM, REML, ML, HS, SJ, HE, EB
                 hakn = FALSE,
                 keepdata= TRUE,
                 prediction = FALSE,
                 sm = "OR")
  
  
  forest.meta(res2,
         leftcols = c('studlab','rr','se'),
         leftlabs = c("Author","log(OR)","SE"),
         digits.pval=2
           )
  

  s1=summary.meta(res2,
               overall.hetstat = TRUE, 
               overall = TRUE)

  print(s1)
  
  return(res2)
  
}


#### model1

model(meta1a)
model(meta1a1)
model(meta1a2)

#### model2

model(meta2a)
model(meta2a1)
model(meta2a2)
