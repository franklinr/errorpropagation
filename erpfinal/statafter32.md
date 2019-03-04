---
output:
  html_document:
    fig_height: 7
    fig_width: 10
  word_document: default
---
ERP model
=========



```
## R version 3.5.2 (2018-12-20)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS Mojave 10.14.3
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] kableExtra_1.0.1  signal_0.7-6      forcats_0.3.0    
##  [4] dplyr_0.8.0.1     purrr_0.2.5       readr_1.3.1      
##  [7] tidyr_0.8.2       tibble_2.0.1      tidyverse_1.2.1  
## [10] emmeans_1.3.2     multcomp_1.4-8    TH.data_1.0-9    
## [13] MASS_7.3-51.1     survival_2.43-3   mvtnorm_1.0-8    
## [16] stringr_1.4.0     lme4_1.1-18-1     Matrix_1.2-15    
## [19] ggplot2_3.1.0     data.table_1.12.0 reshape2_1.4.3   
## [22] markdown_0.9      knitr_1.21       
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0        lubridate_1.7.4   lattice_0.20-38  
##  [4] zoo_1.8-4         assertthat_0.2.0  digest_0.6.18    
##  [7] R6_2.3.0          cellranger_1.1.0  plyr_1.8.4       
## [10] backports_1.1.3   evaluate_0.12     coda_0.19-2      
## [13] httr_1.4.0        pillar_1.3.1      rlang_0.3.1      
## [16] lazyeval_0.2.1    readxl_1.3.0      minqa_1.2.4      
## [19] rstudioapi_0.9.0  nloptr_1.2.1      rmarkdown_1.11   
## [22] splines_3.5.2     webshot_0.5.1     munsell_0.5.0    
## [25] broom_0.5.1       compiler_3.5.2    modelr_0.1.2     
## [28] xfun_0.4          pkgconfig_2.0.2   htmltools_0.3.6  
## [31] tidyselect_0.2.5  codetools_0.2-16  viridisLite_0.3.0
## [34] crayon_1.3.4      withr_2.1.2       grid_3.5.2       
## [37] nlme_3.1-137      jsonlite_1.6      xtable_1.8-3     
## [40] gtable_0.2.0      magrittr_1.5      scales_1.0.0     
## [43] estimability_1.3  cli_1.0.1         stringi_1.2.4    
## [46] xml2_1.2.0        generics_0.0.2    sandwich_2.5-0   
## [49] tools_3.5.2       glue_1.3.0        hms_0.4.2        
## [52] colorspace_1.4-0  rvest_0.3.2       haven_2.0.0
```






```r
noslopes=TRUE   # set to use just random intercept model
noslopes=FALSE  # set to false to get maximal model

if (!file.exists("results/resultdata.rds")){
  write("reading dataframe.csv", stderr())
  # load data for all models
#  resultWhole.df <- read.csv("smalldevel.csv") 
  resultWhole.df <- read.csv("results/dataframe.csv") # automatically uses working dir
  write("finished reading dataframe.csv", stderr())
  result.df <- resultWhole.df[,0:97]
#  result.df <- removeNAColumns(resultWhole.df)
  names(result.df)[6]<-"Condition"
  
   result.df$file=as.character(result.df$file)
  result.df$sub = str_extract(result.df$file,"[-]s[0-9]+[-]")
  result.df$sub = str_replace_all(result.df$sub,"[-]","")
  result.df$sub=factor(result.df$sub)
  
  result.df$epoch = as.integer(str_extract(result.df$file,"[0-9]+$"))
  result.df$coul = str_extract(result.df$file,"[0-9.]+vcoul[12]")
  result.df$vrom = str_extract(result.df$file,"[0-9.]+vrommer")
  result.df$vromNot = str_extract(result.df$file,"[0-9.]+vromNot")
  result.df$devellearn = str_extract(result.df$file,"[0-9.]+violation")
 
#  result.df$file = NULL
  epochCol = which(names(result.df)=="epoch")
  subCol = which(names(result.df)=="sub")
  coulCol = which(names(result.df)=="coul")
  vromCol = which(names(result.df)=="vrom")
  vromNotCol = which(names(result.df)=="vromNot")
  devellearnCol = which(names(result.df)=="devellearn")
  
  result2.df = result.df[,c(subCol,coulCol,vromCol,vromNotCol,devellearnCol,epochCol,0:(subCol-1))]
  names(result2.df)[which(names(result2.df)=="sent")]<-"Example"
  
  result2.df$tick = result2.df$tick+1
  result2.df$layer = factor(result2.df$layer, levels=c("word","compress", "hidden","cword"))
  result2.df$depth = as.numeric(result2.df$layer)
  result.df=result2.df
#  saveRDS(result.df,file="smalldevel.rds")
  saveRDS(result.df,file="results/resultdata.rds")
}else{
  print("using Rds file")
#  devresult.df=readRDS(file="smalldevel.rds")
  result.df=readRDS(file="results/resultdata.rds")
}

#print folder list to make sure correct folders 
folder = str_replace(result.df$file[1],"/derivatives.*","")
if (folder == "derivatives100000"){
  folder = basename(getwd())
}
sublist = paste(levels(result.df$sub),collapse=",")
print(paste("Folder",folder," nsub=",sublist))
```

```
## [1] "Folder sim70-s0-Sim116-alt10-50/deriv0.1vcoul1-100000  nsub= s0,s1,s2,s3,s4,s5,s6,s7,s8,s9"
```

```r
# load counts
counts.df = read.csv("results/counts.csv")
counts.df$perc = round(100*counts.df$prop)
#print(counts.df[order(counts.df$pair),])
unigram.df = read.csv("results/unigram.csv")
#print(unigram.df[unigram.df$pair%in%c("wine","water","tea","cake"),])

result2.df=result.df 
first=which(names(result2.df)=="the")-1
end=which(names(result2.df)=="per")
# compute metrics Hidden layer is smaller than Lexical layer, so this captures all units
# hidden has only 50 units, so rest are marked with NA
result2.df$abssum  = rowSums(abs(result2.df[,first:end]),na.rm = TRUE)  # sum abs error
result2.df$absmean  = rowMeans(abs(result2.df[,first:end]),na.rm = TRUE) # mean abs error

#object.size(result2.df[,first:end])
#Sys.getenv("R_MAX_VSIZE")


couldf = subset(result2.df,epoch == max(result2.df$epoch) & !layer %in% c("cword") & !is.na(coul))
couldf$file = NULL
couldf$vrom = NULL
couldf$vromNot = NULL
#xtabs(~ Condition + coul, couldf)
print(unique(couldf$coul))
```

```
## [1] "0.1vcoul1" "0.1vcoul2"
```

```r
rommerdf = subset(result2.df,epoch == max(result2.df$epoch) & !layer %in% c("cword") & !is.na(vrom))
rommerdf$file = NULL
rommerdf$coul = NULL
rommerdf$vromNot = NULL

rommerNotdf = subset(result2.df,epoch == max(result2.df$epoch) & !layer %in% c("cword") & !is.na(vromNot))
rommerNotdf$file = NULL
rommerNotdf$coul = NULL
rommerNotdf$vrom = NULL

develop.df =  subset(result2.df,!layer %in% c("cword") & is.na(coul) & is.na(vrom) & is.na(vromNot))
develop.df$file = NULL
develop.df$coul = NULL
develop.df$vrom = NULL
develop.df$vromNot = NULL
develop.df$devellearn = NULL

adultdata.df = subset(develop.df,epoch == max(develop.df$epoch) )
adultdata.df$file = NULL
adultdata.df$vrom = NULL
adultdata.df$coul = NULL
adultdata.df$vromNot = NULL
adultdata.df$devellearn = NULL


#load("adultdata.RData")
layerNotERP = c("output","target", "error","myoutputder","myinputder")
dependMeasure = "abssum" # we can change the dependent measure for all models
mainMeasure = "inputderiv"
p600layer = "hidden"
#load(adultdata.df)
first=which(names(adultdata.df)=="the")
end=which(names(adultdata.df)=="per")
# when doing final version, change noslopes to FALSE
figwidth=6
figheight=3
```


-------------------

## Cloze


```r
write("cloze", stderr())
cloze.df = subset(adultdata.df,  str_detect(adultdata.df$Condition,"CLOZE") & wordcat %in% c('NOUNI'))
cloze.df$Condition = factor(cloze.df$Condition,labels=c("High Cloze","Medium Cloze","Low Cloze"))

print(head(cloze.df[cloze.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##         word wordcat    Condition                                 Example
## 120043 water   NOUNI   High Cloze  a teacher was drink -ing the water . .
## 120160 water   NOUNI Medium Cloze  a teacher was taste -ing the water . .
## 120277 water   NOUNI    Low Cloze   a teacher was take -ing the water . .
## 122175 water   NOUNI   High Cloze     the driver will drink the water . .
## 122279 water   NOUNI Medium Cloze     the driver will taste the water . .
## 122383 water   NOUNI    Low Cloze      the driver will take the water . .
##        tick
## 120043    7
## 120160    7
## 120277    7
## 122175    6
## 122279    6
## 122383    6
```

```r
# check that layers are the right size
cloze.df$laylen = 1+(end-first)-rowSums(apply(cloze.df[,first:end],2,is.na))
print(aggregate(laylen ~ layer + measure, cloze.df,mean))
```

```
##       layer    measure laylen
## 1      word inputderiv     87
## 2  compress inputderiv     29
## 3    hidden inputderiv     49
## 4      word   outderiv     87
## 5  compress   outderiv     29
## 6    hidden   outderiv     49
## 7      word     output     87
## 8  compress     output     29
## 9    hidden     output     49
## 10     word     target     87
```

```r
cloze.df$laylen=NULL

showWordOutError("coffee,tea,wine,water,cake",c("Condition","measure"),cloze.df)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)



```r
computeClozeERPCorr(cloze.df,dependMeasure,"word")
```

```
## [1] "Correlation output prediction and abssum error of word is -0.999999999999791"
```

```r
computeClozeERPCorr(cloze.df,dependMeasure,"hidden")
```

```
## [1] "Correlation output prediction and abssum error of hidden is -0.410709240990722"
```

```r
corrHiddenOutputInputDeriv(cloze.df,"Condition",layername="word")
```

```
## Condition
##   High Cloze Medium Cloze    Low Cloze 
##            8            8            8
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
myggsave("img/actderivcloze.png",width=6,height=6)
```


```r
# this is for creating ERP figure
nooutput = subset(cloze.df, ! measure %in% layerNotERP )
end = which(names(nooutput)=="depth")-1
first = which(names(nooutput)=="none")
norawcloze.df = nooutput[,-(first:end)] # remove word specific activations/error


dvlen = length(names(norawcloze.df))- which(names(norawcloze.df)=="depth")
longnoraw= melt(norawcloze.df,id.var=1:(length(norawcloze.df)-dvlen)) # make long format
print(folder) # we use folder rename html file
```

```
## [1] "sim70-s0-Sim116-alt10-50/deriv0.1vcoul1-100000"
```

```r
# add target layer for figure
#mapping = aes(x=depth, y=value, colour = Condition,linetype=Condition)
p1 = drawERP(longnoraw,"value",  "Condition" ,"measure","variable" ,timesize=-1)
#p1= drawERP(longnoraw,mapping, timesize=-1,span = 0.8)
p1 = p1 + facet_wrap( ~ measure+variable,scales = "free",nrow=1)
p1
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
print(folder)
```

```
## [1] "sim70-s0-Sim116-alt10-50/deriv0.1vcoul1-100000"
```


```r
# this is the counts for verb arguments in the input
interactionText="NO TEXT"


# get example for table
tablesub = subset(norawcloze.df,layer == "word" & measure == mainMeasure)
exampleTable = tablesub[1:3,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##           Condition                                 Example
## 120045   High Cloze  a teacher was drink -ing the water . .
## 120162 Medium Cloze  a teacher was taste -ing the water . .
## 120279    Low Cloze   a teacher was take -ing the water . .
```

```r
wordccompinput  = subset(norawcloze.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj
  print("run models")
  # our analysis uses this set

  wordccompinput$cloze = 0
  wordccompinput$cloze[wordccompinput$Condition=="Low Cloze" ]=1
  wordccompinput$cloze[wordccompinput$Condition=="High Cloze" ]=-1
  wordccompinput$cword = ifelse(wordccompinput$layer=="word",0.5,-0.5)

  # this is the main mixed model with centered variables
  randomeff = " + (1 + cloze*cword |sub)"
  if (noslopes){ randomeff = " + (1 | sub)" }
  formu = as.formula(paste(dependMeasure,"~ cloze*cword",randomeff ))
  omnimodel = lmer(formu, wordccompinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  
  modellist = modelComparison(omnimodel) # create anova table by terms subtraction

}
```

```
## [1] "run models"
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cloze * cword + (1 + cloze * cword | sub)
##    Data: wordccompinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: -102.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9503 -0.4862 -0.1553  0.4866  7.8559 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr             
##  sub      (Intercept) 0.03472  0.1863                    
##           cloze       0.04822  0.2196    0.37            
##           cword       0.09356  0.3059   -0.78 -0.86      
##           cloze:cword 0.13975  0.3738   -0.81 -0.82  0.96
##  Residual             0.05136  0.2266                    
## Number of obs: 1800, groups:  sub, 10
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  1.10801    0.05916  18.729
## cloze        0.37826    0.06974   5.423
## cword        0.79577    0.09731   8.177
## cloze:cword  0.25016    0.11894   2.103
## 
## Correlation of Fixed Effects:
##             (Intr) cloze  cword 
## cloze        0.364              
## cword       -0.768 -0.847       
## cloze:cword -0.799 -0.815  0.947
## [1] "remove . ~ . -  cloze:cword "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cloze + cword + (1 + cloze * cword | sub)
## model: abssum ~ cloze * cword + (1 + cloze * cword | sub)
##        Df     AIC      BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
## model2 14 -89.760 -12.8220 58.880  -117.76                           
## model  15 -91.757  -9.3243 60.879  -121.76 3.9978      1    0.04556 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cloze:cword  *** "
## [1] "remove . ~ . -  cword "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cloze + (1 + cloze * cword | sub)
## model: abssum ~ cloze + cword + (1 + cloze * cword | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 13 -58.263  13.179 42.131  -84.263                             
## model  14 -89.760 -12.822 58.880 -117.760 33.497      1  7.139e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cword  *** "
## [1] "remove . ~ . -  cloze "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ (1 + cloze * cword | sub)
## model: abssum ~ cloze + (1 + cloze * cword | sub)
##        Df     AIC    BIC logLik deviance Chisq Chi Df Pr(>Chisq)   
## model2 12 -52.154 13.793 38.077  -76.154                           
## model  13 -58.263 13.179 42.131  -84.263 8.109      1   0.004405 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cloze  *** "
```

```r
# create figure for paper
#mapping = aes_string(x="depth", y=dependMeasure, colour = "Condition",linetype="Condition")
drawERP(wordccompinput,dependMeasure,"Condition", span = 0.8, timeline.y=2.1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
if (!noslopes){ # when doing final version, change noslopes to FALSE
  myggsave("img/cloze.png",width=figwidth,height=figheight) # control font size with height/width
}

n4P6cor = computeN4P6correlation(wordccompinput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.410709246238285"
```

```r
n4P6cor = computeN4P6correlation2(wordccompinput,"High Cloze","Low Cloze")
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.228274663319815"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex
Cloze probabilities were manipulated in the language input on direct objects in active transitive items.
The same test word \textit{water} was used in each of the test sentences, but its cloze probability for each verb was manipulated.  The same test word \textit{water} was used in each of the test sentences, but its cloze probability for each verb was manipulated.  It occurred 60\% of the time with the verb \textit{drink} (High Cloze), 15\% with the verb \textit{taste} (Medium Cloze), and 4\% of the time with the verb \textit{take} (Low Cloze).

This is an example of the stimuli used in the model

\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
High Cloze & a teacher was drink -ing the water . .\\
\hline
Medium Cloze & a teacher was taste -ing the water . .\\
\hline
Low Cloze & a teacher was take -ing the water . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + cloze*cword |sub)

Mixed Model Results
a main effect of cloze ,$\beta$=0.38, SE=0.07, $\chi^2$(1)=8.11, p=0.0044 
a main effect of cword ,$\beta$=0.8, SE=0.097, $\chi^2$(1)=33.5, p$<$0.001 
@@an interaction of cloze:cword ,$\beta$=0.25, SE=0.12, $\chi^2$(1)=4, p=0.046 

```


-------------------

## Federmeier et al. 2007


```r
write("feder", stderr())
feder.df = subset(adultdata.df,  str_detect(adultdata.df$Condition,"(EXPECT)") & wordcat %in% c('NOUNI'))
cond = str_split_fixed(feder.df$Condition,";",2)
feder.df$Constraint = cond[,1]
feder.df$Expectation = cond[,2]
feder.df$Constraint = factor(feder.df$Constraint,labels=c("Strong","Weak"))
feder.df$Expectation = factor(feder.df$Expectation,labels=c("Expected","Unexpected"))
nc = length(feder.df)
feder.df = feder.df[,c(nc-1,nc,1:(nc-2))] # move new columns to front

print(head(feder.df[feder.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##         word wordcat             Condition
## 119614   tea   NOUNI   STRONGCONS;EXPECTED
## 119718  wine   NOUNI     WEAKCONS;EXPECTED
## 119822 water   NOUNI STRONGCONS;UNEXPECTED
## 119926 water   NOUNI   WEAKCONS;UNEXPECTED
## 121083   tea   NOUNI   STRONGCONS;EXPECTED
## 121213  wine   NOUNI     WEAKCONS;EXPECTED
##                                             Example tick
## 119614                  a wife will sip the tea . .    6
## 119718               a wife will sniff the wine . .    6
## 119822                a wife will sip the water . .    6
## 119926              a wife will sniff the water . .    6
## 121083     the brother -s were sip -ing the tea . .    8
## 121213  the brother -s were sniff -ing the wine . .    8
```

```r
showWordOutError("coffee,tea,wine,water,cake",c("Expectation","Constraint","measure"),feder.df)
```

![plot of chunk n400-line](figure/n400-line-1.png)


```r
computeClozeERPCorr(feder.df,dependMeasure,"word")
```

```
## [1] "Correlation output prediction and abssum error of word is -0.999999999999748"
```

```r
corrHiddenOutputInputDeriv(feder.df,"Expectation",dim2="Constraint",layername="word")
```

```
## Condition
##               ACTCONT               CATCONT               CATVIOL 
##                     0                     0                     0 
##                  CONG               GARDAMB                GARDSC 
##                     0                     0                     0 
##             HIGHCLOZE                 INCOH              LOWCLOZE 
##                     0                     0                     0 
##              PASSCONT             PLUR;CONT             PLUR;VIOL 
##                     0                     0                     0 
##               SEMATTR             SING;CONT             SING;VIOL 
##                     0                     0                     0 
##               STRONG    STRONGCONS;EXPECTED STRONGCONS;UNEXPECTED 
##                     0                     8                     8 
##               SUBCONT               SUBVIOL             TENSECONT 
##                     0                     0                     0 
##             TENSEVIOL             TEST-PRED           TEST-UNPRED 
##                     0                     0                     0 
##                  WEAK                 WEAK      WEAKCONS;EXPECTED 
##                     0                     0                     8 
##   WEAKCONS;UNEXPECTED             ́ZEROCLOZE 
##                     8                     0
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)



```r
# this is for creating ERP figure
nooutput = subset(feder.df, ! measure %in% layerNotERP )
end = which(names(nooutput)=="depth")-1
first = which(names(nooutput)=="none")
norawfeder.df = nooutput[,-(first:end)] # remove word specific activations/error

catmembers = str_split("coffee,tea,wine,water,beer",",")[[1]]
nooutput$abssumcat  = rowSums(abs(nooutput[,catmembers]),na.rm = TRUE)  
nooutput$abstarget <- abs(nooutput[,"tea"])
norawclozecat.df = nooutput[,-(first:end)] 
wordccompinputcat  = subset(norawclozecat.df,measure == mainMeasure & layer %in% c("word") )
wordccompinputcat$abssumcatnotar = wordccompinputcat$abssumcat - wordccompinputcat$abstarget
wordccompinputcat$abssumcatnotarprop = wordccompinputcat$abssumcatnotar/wordccompinputcat$abssum
wordccompinputcat$abstargetprop = wordccompinputcat$abstarget/wordccompinputcat$abssum
print(aggregate(cbind(abstargetprop,abssumcatnotarprop) ~ Condition,wordccompinputcat,mean))
```

```
##               Condition abstargetprop abssumcatnotarprop
## 1   STRONGCONS;EXPECTED    0.50000009          0.4826536
## 2 STRONGCONS;UNEXPECTED    0.31914547          0.6724826
## 3     WEAKCONS;EXPECTED    0.11358329          0.8670062
## 4   WEAKCONS;UNEXPECTED    0.08342568          0.9007265
```

```r
ggplot(wordccompinputcat,aes(x=abstargetprop,y=abssumcatnotarprop,colour=Condition))+geom_point()
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
print("Correlation for category/target error in weak expected items")
```

```
## [1] "Correlation for category/target error in weak expected items"
```

```r
weakexp = subset(wordccompinputcat,Condition == "WEAKCONS;EXPECTED")
print(cor(weakexp$abssumcatnotarprop,weakexp$abstargetprop))
```

```
## [1] -0.8582342
```

```r
dvlen = length(names(norawfeder.df))- which(names(norawfeder.df)=="depth")
longnoraw= melt(norawfeder.df,id.var=1:(length(norawfeder.df)-dvlen)) # make long format
print(folder) # we use folder rename html file
```

```
## [1] "sim70-s0-Sim116-alt10-50/deriv0.1vcoul1-100000"
```

```r
# add target layer for figure
#mapping = aes(x=depth, y=value, colour = Constraint,linetype=Expectation)
p1= drawERP(longnoraw,"value","Constraint","Expectation","measure","variable", timesize=-1)
p1 = p1 + facet_wrap( ~ measure+variable,scales = "free",nrow=1)
p1
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png)


```r
# get example for table
tablesub = subset(norawfeder.df,layer == "word" & measure == mainMeasure)
exampleTable = tablesub[1:4,c("Constraint","Expectation","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##        Constraint Expectation                          Example
## 119616     Strong    Expected      a wife will sip the tea . .
## 119720       Weak    Expected   a wife will sniff the wine . .
## 119824     Strong  Unexpected    a wife will sip the water . .
## 119928       Weak  Unexpected  a wife will sniff the water . .
```

```r
interactionText=""

wordccompinput  = subset(norawfeder.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj
  print("run models")
  # our analysis uses this set
  wordccompinput$cstrong = ifelse(wordccompinput$Constraint=="Strong",-0.5,0.5)
  wordccompinput$cexpected = ifelse(wordccompinput$Expectation=="Expected",-0.5,0.5)
  wordccompinput$cword = ifelse(wordccompinput$layer=="word",0.5,-0.5)
# - Constraint:Expectation:layer - Constraint:Expectation - Constraint:layer  - Expectation:layer
  # this is the main mixed model with centered variables
    randomeff = " + (1 + Constraint*Expectation*layer - Constraint:Expectation:layer - Constraint:layer | sub)"
  formu = as.formula(paste(dependMeasure,"~ cexpected + cstrong + cword + Constraint+Expectation+layer + sub" ))
  if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ cexpected*cstrong*cword",randomeff ))
  omnimodel = lmer(formu, wordccompinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))

  modellist = modelComparison(omnimodel) # create anova table by terms subtraction

   randomeff2 = " + (1 + Constraint*Expectation | sub)"
    wordccompinputword = subset(wordccompinput, layer == "word")
  formu = as.formula(paste(dependMeasure,"~ cexpected*cstrong",randomeff2 ))
  omnimodelword = lmer(formu, wordccompinputword,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodelword))
  modellistword = modelComparison(omnimodelword) # create anova table by terms subtraction

  wordccompinputhidden= subset(wordccompinput, layer == "hidden")
  omnimodelhid = lmer(formu, wordccompinputhidden,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodelhid))
  modellisthid = modelComparison(omnimodelhid) # create anova table by terms subtraction
}
```

```
## [1] "run models"
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## abssum ~ cexpected * cstrong * cword + (1 + Constraint * Expectation *  
##     layer - Constraint:Expectation:layer - Constraint:layer |      sub)
##    Data: wordccompinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: -1542.4
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.225 -0.538 -0.054  0.423  9.643 
## 
## Random effects:
##  Groups   Name                                 Variance Std.Dev. Corr 
##  sub      (Intercept)                          0.07068  0.2658        
##           ConstraintWeak                       0.06421  0.2534   -0.65
##           ExpectationUnexpected                0.10199  0.3194   -0.99
##           layerhidden                          0.01021  0.1011   -0.47
##           ConstraintWeak:ExpectationUnexpected 0.06527  0.2555    0.43
##           ExpectationUnexpected:layerhidden    0.09830  0.3135    0.60
##  Residual                                      0.02802  0.1674        
##                         
##                         
##                         
##   0.68                  
##   0.21  0.45            
##  -0.88 -0.46 -0.07      
##  -0.30 -0.60  0.27  0.09
##                         
## Number of obs: 2400, groups:  sub, 10
## 
## Fixed effects:
##                          Estimate Std. Error t value
## (Intercept)              1.003275   0.043563  23.031
## cexpected                0.446742   0.070069   6.376
## cstrong                  0.005959   0.048971   0.122
## cword                    0.772455   0.066089  11.688
## cexpected:cstrong       -0.310816   0.081938  -3.793
## cexpected:cword          0.416987   0.100082   4.166
## cstrong:cword            0.271038   0.013667  19.832
## cexpected:cstrong:cword -0.461231   0.027333 -16.874
## 
## Correlation of Fixed Effects:
##             (Intr) cxpctd cstrng cword  cxpctd:cs cxpctd:cw cstrn:
## cexpected   -0.144                                                
## cstrong     -0.115  0.393                                         
## cword       -0.812 -0.305  0.175                                  
## cxpctd:cstr -0.030 -0.027 -0.608 -0.035                           
## cxpctd:cwrd -0.892  0.104  0.412  0.870 -0.089                    
## cstrng:cwrd  0.000  0.000  0.000  0.000  0.000     0.000          
## cxpctd:cst:  0.000  0.000  0.000  0.000  0.000     0.000     0.000
## [1] "remove . ~ . -  cexpected:cstrong:cword "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model2:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model2:     sub) + cexpected:cstrong + cexpected:cword + cstrong:cword
## model: abssum ~ cexpected * cstrong * cword + (1 + Constraint * Expectation * 
## model:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model:     sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 29 -1255.9 -1088.2 656.94  -1313.9                             
## model  30 -1522.8 -1349.3 791.40  -1582.8 268.93      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cexpected:cstrong:cword  *** "
## [1] "remove . ~ . -  cstrong:cword "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model2:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model2:     sub) + cexpected:cstrong + cexpected:cword
## model: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model:     sub) + cexpected:cstrong + cexpected:cword + cstrong:cword
##        Df      AIC      BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 28  -930.92  -768.99 493.46  -986.92                             
## model  29 -1255.88 -1088.17 656.94 -1313.88 326.97      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cstrong:cword  *** "
## [1] "remove . ~ . -  cexpected:cword "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model2:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model2:     sub) + cexpected:cstrong
## model: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model:     sub) + cexpected:cstrong + cexpected:cword
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2 27 -922.17 -766.02 488.08  -976.17                            
## model  28 -930.92 -768.99 493.46  -986.92 10.746      1   0.001045 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cexpected:cword  *** "
## [1] "remove . ~ . -  cexpected:cstrong "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model2:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model2:     sub)
## model: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model:     sub) + cexpected:cstrong
##        Df     AIC     BIC logLik deviance Chisq Chi Df Pr(>Chisq)  
## model2 26 -920.47 -770.10 486.23  -972.47                          
## model  27 -922.17 -766.02 488.08  -976.17 3.701      1    0.05438 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cexpected:cstrong "
## [1] "remove . ~ . -  cword "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cexpected + cstrong + (1 + Constraint * Expectation * 
## model2:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model2:     sub)
## model: abssum ~ cexpected + cstrong + cword + (1 + Constraint * Expectation * 
## model:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model:     sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 25 -901.26 -756.68 475.63  -951.26                             
## model  26 -920.47 -770.10 486.23  -972.47 21.208      1  4.121e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cword  *** "
## [1] "remove . ~ . -  cstrong "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ cexpected + (1 + Constraint * Expectation * layer - 
## model2:     Constraint:Expectation:layer - Constraint:layer | sub)
## model: abssum ~ cexpected + cstrong + (1 + Constraint * Expectation * 
## model:     layer - Constraint:Expectation:layer - Constraint:layer | 
## model:     sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2 24 -902.99 -764.19 475.49  -950.99                         
## model  25 -901.26 -756.68 475.63  -951.26 0.2718      1     0.6021
## [1] "########## Above comparison for  cstrong "
## [1] "remove . ~ . -  cexpected "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinput
## Models:
## model2: abssum ~ (1 + Constraint * Expectation * layer - Constraint:Expectation:layer - 
## model2:     Constraint:layer | sub)
## model: abssum ~ cexpected + (1 + Constraint * Expectation * layer - 
## model:     Constraint:Expectation:layer - Constraint:layer | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 23 -890.21 -757.19 468.10  -936.21                             
## model  24 -902.99 -764.19 475.49  -950.99 14.781      1  0.0001207 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cexpected  *** "
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cexpected * cstrong + (1 + Constraint * Expectation |  
##     sub)
##    Data: wordccompinputword
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: -2361.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2932 -0.4793  0.0081  0.4170  4.3461 
## 
## Random effects:
##  Groups   Name                                 Variance Std.Dev. Corr 
##  sub      (Intercept)                          0.047119 0.21707       
##           ConstraintWeak                       0.060987 0.24696  -0.36
##           ExpectationUnexpected                0.080210 0.28321  -0.98
##           ConstraintWeak:ExpectationUnexpected 0.076002 0.27569   0.25
##  Residual                                      0.007145 0.08453       
##             
##             
##             
##   0.36      
##  -0.98 -0.23
##             
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                   Estimate Std. Error t value
## (Intercept)        1.38950    0.02551  54.464
## cexpected          0.65524    0.09023   7.262
## cstrong            0.14148    0.03633   3.894
## cexpected:cstrong -0.54143    0.08772  -6.172
## 
## Correlation of Fixed Effects:
##             (Intr) cxpctd cstrng
## cexpected   -0.718              
## cstrong      0.319  0.052       
## cxpctd:cstr -0.386  0.252 -0.911
## [1] "remove . ~ . -  cexpected:cstrong "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinputword
## Models:
## model2: abssum ~ cexpected + cstrong + (1 + Constraint * Expectation | 
## model2:     sub)
## model: abssum ~ cexpected * cstrong + (1 + Constraint * Expectation | 
## model:     sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 14 -2337.8 -2266.6 1182.9  -2365.8                             
## model  15 -2352.4 -2276.1 1191.2  -2382.4 16.547      1  4.747e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cexpected:cstrong  *** "
## [1] "remove . ~ . -  cstrong "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinputword
## Models:
## model2: abssum ~ cexpected + (1 + Constraint * Expectation | sub)
## model: abssum ~ cexpected + cstrong + (1 + Constraint * Expectation | 
## model:     sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
## model2 13 -2336.7 -2270.6 1181.4  -2362.7                           
## model  14 -2337.8 -2266.6 1182.9  -2365.8 3.1323      1    0.07675 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cstrong "
## [1] "remove . ~ . -  cexpected "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinputword
## Models:
## model2: abssum ~ (1 + Constraint * Expectation | sub)
## model: abssum ~ cexpected + (1 + Constraint * Expectation | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 12 -2321.7 -2260.6 1172.8  -2345.7                             
## model  13 -2336.7 -2270.6 1181.4  -2362.7 17.032      1  3.675e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cexpected  *** "
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cexpected * cstrong + (1 + Constraint * Expectation |  
##     sub)
##    Data: wordccompinputhidden
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: -231.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2827 -0.5168 -0.0836  0.4319  7.8109 
## 
## Random effects:
##  Groups   Name                                 Variance Std.Dev. Corr 
##  sub      (Intercept)                          0.08043  0.2836        
##           ConstraintWeak                       0.10969  0.3312   -0.80
##           ExpectationUnexpected                0.09953  0.3155   -0.32
##           ConstraintWeak:ExpectationUnexpected 0.07653  0.2766    0.58
##  Residual                                      0.04249  0.2061        
##             
##             
##             
##   0.52      
##  -0.79 -0.61
##             
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                   Estimate Std. Error t value
## (Intercept)        0.61705    0.07300   8.453
## cexpected          0.23825    0.08177   2.914
## cstrong           -0.12956    0.07609  -1.703
## cexpected:cstrong -0.08020    0.09066  -0.885
## 
## Correlation of Fixed Effects:
##             (Intr) cxpctd cstrng
## cexpected    0.612              
## cstrong     -0.180  0.174       
## cxpctd:cstr  0.033 -0.202 -0.494
## [1] "remove . ~ . -  cexpected:cstrong "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinputhidden
## Models:
## model2: abssum ~ cexpected + cstrong + (1 + Constraint * Expectation | 
## model2:     sub)
## model: abssum ~ cexpected * cstrong + (1 + Constraint * Expectation | 
## model:     sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2 14 -217.03 -145.77 122.51  -245.03                         
## model  15 -215.86 -139.51 122.93  -245.86 0.8337      1     0.3612
## [1] "########## Above comparison for  cexpected:cstrong "
## [1] "remove . ~ . -  cstrong "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinputhidden
## Models:
## model2: abssum ~ cexpected + (1 + Constraint * Expectation | sub)
## model: abssum ~ cexpected + cstrong + (1 + Constraint * Expectation | 
## model:     sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
## model2 13 -214.21 -148.04 120.10  -240.21                           
## model  14 -217.03 -145.77 122.51  -245.03 4.8183      1    0.02816 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cstrong  *** "
## [1] "remove . ~ . -  cexpected "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: wordccompinputhidden
## Models:
## model2: abssum ~ (1 + Constraint * Expectation | sub)
## model: abssum ~ cexpected + (1 + Constraint * Expectation | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
## model2 12 -211.66 -150.58 117.83  -235.66                           
## model  13 -214.21 -148.04 120.10  -240.21 4.5445      1    0.03302 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cexpected  *** "
```

```r
# create figure for paper
wordccompinput$layer= factor(wordccompinput$layer)
#mapping = aes_string(x="depth", y=dependMeasure, colour = "Constraint",linetype="Expectation")
drawERP(wordccompinput,dependMeasure,"Expectation","Constraint" , timeline.y=2)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
if (!noslopes){ # when doing final version, change noslopes to FALSE
  myggsave("img/fed2007.png",width=figwidth,height=figheight) # control font size with height/width
}

n4P6cor = computeN4P6correlation(wordccompinput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.415496434384026"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex
Sentential constraint was varied by manipulating the verb.  The strong sentential constraint verb \textit{sip} occurred with \textit{tea} 61\% of the time and \textit{water} 10\% of the time.  The weak sentential constraint verb \textit{sniff} occurred with \textit{wine} and with \textit{water} 15\% of the time.  Word expectation can be examined by comparing the unexpected word \textit{water} and the expected words \textit{tea} and \textit{wine}.

This is an example of the stimuli used in the model

\begin{tabular}{l|l|l}
\hline
Constraint & Expectation & Example\\
\hline
Strong & Expected & a wife will sip the tea . .\\
\hline
Weak & Expected & a wife will sniff the wine . .\\
\hline
Strong & Unexpected & a wife will sip the water . .\\
\hline
Weak & Unexpected & a wife will sniff the water . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + Constraint*Expectation*layer - Constraint:Expectation:layer - Constraint:layer | sub)

Mixed Model Results
a main effect of cexpected ,$\beta$=0.45, SE=0.07, $\chi^2$(1)=14.78, p$<$0.001 
no main effect of cstrong ,$\beta$=0.006, SE=0.049, $\chi^2$(1)=0.27, p=0.602 
a main effect of cword ,$\beta$=0.77, SE=0.066, $\chi^2$(1)=21.21, p$<$0.001 
no interaction of cexpected:cstrong ,$\beta$=-0.31, SE=0.082, $\chi^2$(1)=3.7, p=0.054 
an interaction of cexpected:cword ,$\beta$=0.42, SE=0.1, $\chi^2$(1)=10.75, p=0.001 
an interaction of cstrong:cword ,$\beta$=0.27, SE=0.014, $\chi^2$(1)=326.97, p$<$0.001 
@@an interaction of cexpected:cstrong:cword ,$\beta$=-0.46, SE=0.027, $\chi^2$(1)=268.93, p$<$0.001 [1] "Word layer"


Mixed Model Results
a main effect of cexpected ,$\beta$=0.66, SE=0.09, $\chi^2$(1)=17.03, p$<$0.001 
no main effect of cstrong ,$\beta$=0.14, SE=0.036, $\chi^2$(1)=3.13, p=0.077 
@@an interaction of cexpected:cstrong ,$\beta$=-0.54, SE=0.088, $\chi^2$(1)=16.55, p$<$0.001 [1] "Hidden layer"


Mixed Model Results
a main effect of cexpected ,$\beta$=0.24, SE=0.082, $\chi^2$(1)=4.54, p=0.033 
a main effect of cstrong ,$\beta$=-0.13, SE=0.076, $\chi^2$(1)=4.82, p=0.028 
@@no interaction of cexpected:cstrong ,$\beta$=-0.08, SE=0.091, $\chi^2$(1)=0.83, p=0.361 

```

## Position  Van Petten and Kutas


```r
write("vanpetten", stderr())
pos.df = subset(adultdata.df,  str_detect(adultdata.df$Condition,"(CONG|INCOH)")) # & tick < 10)

print(head(pos.df[pos.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##         word wordcat Condition                                     Example
## 112035   man   NOUNA      CONG  man -s throw -ed the toy to the driver . .
## 112048    -s      PL      CONG  man -s throw -ed the toy to the driver . .
## 112061 throw   VERBD      CONG  man -s throw -ed the toy to the driver . .
## 112074   -ed      ED      CONG  man -s throw -ed the toy to the driver . .
## 112087   the     DET      CONG  man -s throw -ed the toy to the driver . .
## 112100   toy   NOUNI      CONG  man -s throw -ed the toy to the driver . .
##        tick
## 112035    1
## 112048    2
## 112061    3
## 112074    4
## 112087    5
## 112100    6
```

```r
postargword.df = subset(pos.df, layer %in% c("word") & measure %in% c("output",mainMeasure) & contfunc=="C" & wordcat != "VERBD" & Condition == "CONG") 
postargword.df$verb = str_match(postargword.df$Example,"(give|throw|send|lend)")[,2]
meanbyword = aggregate(cbind(man,kite,cake) ~ tick + measure + verb + Condition,postargword.df, mean)
meanbywordlong= melt(meanbyword,id.var=1:4)

p1 = ggplot(meanbywordlong, aes(x=verb, y=value, fill = variable)) 
p1 = p1 + geom_bar(stat="identity",position="dodge") 
#p1 = p1 + facet_wrap(~ measure + tick,scales = "free",ncol=4)
p1 = p1 + facet_grid( tick ~ measure + Condition,scales = "free_y")
p1 = p1 + theme(legend.position="bottom")
p1
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)



```r
# this is for creating ERP figure
nooutput = subset(pos.df, ! measure %in% layerNotERP)
end = which(names(nooutput)=="depth")-1
first = which(names(nooutput)=="none")
norawpos.df = nooutput[,-(first:end)] # remove word specific activations/error
norawpos.df$Condition = factor(norawpos.df$Condition,labels=c("Congruent","Syntactic"))

dvlen = length(names(norawpos.df))- which(names(norawpos.df)=="depth")
longnoraw= melt(norawpos.df,id.var=1:(length(norawpos.df)-dvlen)) # make long format

# add target layer for figure
longnoraw2 = subset(longnoraw, measure == mainMeasure & contfunc == "C") #& wordcat != "VERBD"
#longnoraw2$contfunc = paste(longnoraw2$tick,longnoraw2$contfunc)
#mapping = aes(x=depth, y=value, colour = Condition,linetype=Condition)
p1= drawERP(longnoraw2,"value","Condition","variable","tick", timesize=-1)
p1 = p1 + facet_grid(tick ~ variable ,scales = "free")
#p1 = p1 + facet_wrap( ~ tick+variable)
p1
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)


```r
interactionText=""
# get example for table
tablesub = subset(norawpos.df,layer == "word" & measure == mainMeasure & tick == 2)
exampleTable = tablesub[1:4,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##        Condition                                            Example
## 112050 Congruent         man -s throw -ed the toy to the driver . .
## 112193 Syntactic         steak -s throw -ed the man to the kite . .
## 112752 Congruent  the father was lend -ing a sister the kite -s . .
## 112908 Syntactic   the ball was lend -ing a coffee the woman -s . .
```

```r
posinput  = subset(norawpos.df,measure == mainMeasure & layer %in% c("word",p600layer) & word != "." & contfunc=="C" & wordcat %in% c("NOUNA","NOUNI"))
#posinput = subset(posinput, !str_detect(Example,"-par") )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj
  print("run models")
  # our analysis uses this set

  posinput$dcoher = ifelse(posinput$Condition=="Congruent",1,0)
  posinput$ccoher = ifelse(posinput$Condition=="Congruent",0.5,-0.5)
  posinput$cword = ifelse(posinput$layer=="word",0.5,-0.5)
  posinput$ctick = posinput$tick - mean(posinput$tick)
  posinput$ccontent = ifelse(posinput$contfunc=="C",0.5,-0.5)
#& wordcat != "VERBD"
  posinputnoun = subset(posinput, layer == "word")
  randomeff = paste(" + (1 + dcoher*ctick |sub)")
  if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ ctick*dcoher",randomeff ))
  omnimodel = lmer(formu, posinputnoun,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  
  modellist = modelComparison(omnimodel) # create anova table by terms subtraction

  # create figure for paper
}
```

```
## [1] "run models"
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ ctick * dcoher + (1 + dcoher * ctick | sub)
##    Data: posinputnoun
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: -5296.3
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -23.5576  -0.3104   0.0780   0.4200   2.4187 
## 
## Random effects:
##  Groups   Name         Variance  Std.Dev.  Corr             
##  sub      (Intercept)  3.583e-05 0.0059860                  
##           dcoher       7.349e-06 0.0027109 -1.00            
##           ctick        1.821e-06 0.0013493  1.00 -1.00      
##           dcoher:ctick 1.933e-07 0.0004397 -1.00  1.00 -1.00
##  Residual              2.996e-03 0.0547353                  
## Number of obs: 1800, groups:  sub, 10
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)   1.9631757  0.0026291 746.719
## ctick         0.0027206  0.0007025   3.873
## dcoher       -0.0493966  0.0027189 -18.168
## ctick:dcoher -0.0095747  0.0008014 -11.948
## 
## Correlation of Fixed Effects:
##             (Intr) ctick  dcoher
## ctick        0.437              
## dcoher      -0.693 -0.192       
## ctick:dcohr -0.125 -0.659  0.055
## [1] "remove . ~ . -  ctick:dcoher "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: posinputnoun
## Models:
## model2: abssum ~ ctick + dcoher + (1 + dcoher * ctick | sub)
## model: abssum ~ ctick * dcoher + (1 + dcoher * ctick | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 14 -5281.8 -5204.9 2654.9  -5309.8                             
## model  15 -5313.0 -5230.6 2671.5  -5343.0 33.163      1  8.476e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  ctick:dcoher  *** "
## [1] "remove . ~ . -  dcoher "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: posinputnoun
## Models:
## model2: abssum ~ ctick + (1 + dcoher * ctick | sub)
## model: abssum ~ ctick + dcoher + (1 + dcoher * ctick | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2 13 -5275.0 -5203.5 2650.5  -5301.0                            
## model  14 -5281.8 -5204.9 2654.9  -5309.8 8.8597      1   0.002915 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  dcoher  *** "
## [1] "remove . ~ . -  ctick "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: posinputnoun
## Models:
## model2: abssum ~ (1 + dcoher * ctick | sub)
## model: abssum ~ ctick + (1 + dcoher * ctick | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
## model2 12 -5273.4 -5207.5 2648.7  -5297.4                           
## model  13 -5275.0 -5203.5 2650.5  -5301.0 3.5591      1    0.05922 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  ctick "
```

```r
# & wordcat != "VERBD"
  longnoraw2 = subset(posinput,  layer == "word" )
  form = as.formula(paste(dependMeasure," ~ tick + Condition",sep=""))
  mdf = aggregate(form, longnoraw2, mean)
  mapping = aes_string(x="tick", y=dependMeasure, colour = "Condition",linetype="Condition")
  p1 = ggplot(mdf,mapping=mapping)
  #p1 = p1 + geom_line() + geom_point()
  p1 = p1 + geom_point()
  p1 = p1 + stat_smooth(method="lm",se=F)
  p1 = p1 + scale_colour_grey()
#  p1 = p1 + scale_colour_brewer( palette="Set1")
  p1 = p1 + scale_x_continuous(breaks=1:12)
  p1 = p1 + theme_bw()
  p1 = p1 +theme(legend.background = element_rect(size=.4, color="grey80")) 
  p1=p1 + theme(legend.position="bottom")+xlab("Sentence Position")+ylab("Sum Abs. Error")
  p1 = p1 + theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))
  cong = str_split(exampleTable$Example[1]," ")[[1]]
  inco = str_split(exampleTable$Example[2]," ")[[1]]
  
coindf = data.frame(tick=rep(1:9,2),word=c(cong[2:10],inco[2:10]),Condition=rep(c("Congruent","Incoherent"),each=9),dependMeasure=rep(c(1.9,1.97),each=9))

#   p1=p1+geom_text(aes(x=tick, y= dependMeasure,label=word,colour = Condition),coindf,size=3)
p1
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/vanpetten.png",width=figwidth,height=figheight) # control font size with height/width
  }
n4P6cor = computeN4P6correlation(posinput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.313495766048524"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex
Need stats to explain this

This is an example of the stimuli used in the model

\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
Congruent & man -s throw -ed the toy to the driver . .\\
\hline
Syntactic & steak -s throw -ed the man to the kite . .\\
\hline
Congruent & the father was lend -ing a sister the kite -s . .\\
\hline
Syntactic & the ball was lend -ing a coffee the woman -s . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + dcoher*ctick |sub)

Mixed Model Results
no main effect of ctick ,$\beta$=0.0027, SE=7e-04, $\chi^2$(1)=3.56, p=0.059 
a main effect of dcoher ,$\beta$=-0.049, SE=0.0027, $\chi^2$(1)=8.86, p=0.0029 
@@ @@an interaction of ctick:dcoher ,$\beta$=-0.0096, SE=8e-04, $\chi^2$(1)=33.16, p$<$0.001 

```


## AGREEMENT Hagoort et al 1993


```r
# This is Hagoort et al 1993
write("agree", stderr())
agree.df = subset(adultdata.df,  str_detect(Condition,"(PLUR|SING)") )
verbpos = which(agree.df$wordcat == "VERBT")
#verb.df = agree.df[verbpos,]
wordcatsamelen = which(agree.df$wordcat!=agree.df$wordcat[1])[1]-1  # same number of rows for each category
postverb.df = agree.df[verbpos + wordcatsamelen,] # position after verb
postverb.df=postverb.df[!is.na(postverb.df$Condition),]

cond = str_split_fixed(postverb.df$Condition,";",2)
postverb.df$Number = cond[,1]
postverb.df$Agreement = cond[,2]
postverb.df$Number = factor(postverb.df$Number,labels=c("Plural","Singular"))
postverb.df$Agreement = factor(postverb.df$Agreement,labels=c("Control","Violation"))
nc = length(postverb.df)
postverb.df = postverb.df[,c(nc-1,nc,1:(nc-2))]
xtabs(~ Agreement + Number ,postverb.df)
```

```
##            Number
## Agreement   Plural Singular
##   Control     3000     3000
##   Violation   3000     3000
```

```r
print(head(postverb.df[postverb.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##        word wordcat Condition                              Example tick
## 112373    a     DET PLUR;CONT          the boy -s take a stick . .    5
## 112477  -ss      SS PLUR;VIOL      the boy -s take -ss a stick . .    5
## 112581  -ss      SS SING;CONT         the boy take -ss a stick . .    4
## 112685    a     DET SING;VIOL             the boy take a stick . .    4
## 115441 wine   NOUNI PLUR;CONT      the father -s taste wine -s . .    5
## 115545  -ss      SS PLUR;VIOL  the father -s taste -ss wine -s . .    5
```

```r
showWordOutError("the,X.ss,X.ing,X.ed,it",c("Agreement","measure","Condition"),postverb.df)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)


```r
computeClozeERPCorr(postverb.df,dependMeasure,"hidden")
```

```
## [1] "Correlation output prediction and abssum error of hidden is -0.635099300781296"
```

```r
corrHiddenOutputInputDeriv(postverb.df,"Agreement","Number")
```

```
## Condition
##               ACTCONT               CATCONT               CATVIOL 
##                     0                     0                     0 
##                  CONG               GARDAMB                GARDSC 
##                     0                     0                     0 
##             HIGHCLOZE                 INCOH              LOWCLOZE 
##                     0                     0                     0 
##              PASSCONT             PLUR;CONT             PLUR;VIOL 
##                     0                     8                     8 
##               SEMATTR             SING;CONT             SING;VIOL 
##                     0                     8                     8 
##               STRONG    STRONGCONS;EXPECTED STRONGCONS;UNEXPECTED 
##                     0                     0                     0 
##               SUBCONT               SUBVIOL             TENSECONT 
##                     0                     0                     0 
##             TENSEVIOL             TEST-PRED           TEST-UNPRED 
##                     0                     0                     0 
##                  WEAK                 WEAK      WEAKCONS;EXPECTED 
##                     0                     0                     0 
##   WEAKCONS;UNEXPECTED             ́ZEROCLOZE 
##                     0                     0
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)



```r
# make erp figure with different measures
#mapping = aes(x=depth, y=value, colour = Number,linetype=Agreement) 
#norawpostverbnotargout.df=removeWordsDrawERP(postverb.df,"value","Number","Agreement","variable",showFig=T)
norawpostverbnotargout.df=removeWordsDrawERP(postverb.df,"value","Agreement","Number","measure",iv5="variable",showFig=T)
```

```
##   Number Agreement sub  epoch word wordcat contfunc
## 1 Plural   Control  s0 100000    a     DET        F
## 2 Plural   Control  s0 100000    a     DET        F
## 3 Plural   Control  s0 100000    a     DET        F
## 4 Plural   Control  s0 100000    a     DET        F
## 5 Plural   Control  s0 100000    a     DET        F
## 6 Plural   Control  s0 100000    a     DET        F
##                        Example Condition tick    layer    measure depth
## 1  the boy -s take a stick . . PLUR;CONT    5     word   outderiv     1
## 2  the boy -s take a stick . . PLUR;CONT    5     word inputderiv     1
## 3  the boy -s take a stick . . PLUR;CONT    5 compress   outderiv     2
## 4  the boy -s take a stick . . PLUR;CONT    5 compress inputderiv     2
## 5  the boy -s take a stick . . PLUR;CONT    5   hidden   outderiv     3
## 6  the boy -s take a stick . . PLUR;CONT    5   hidden inputderiv     3
##   variable      value
## 1   abssum  7.4703980
## 2   abssum  1.7322764
## 3   abssum 56.9887266
## 4   abssum  1.4021043
## 5   abssum 18.1684365
## 6   abssum  0.9772369
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)



```r
interactionText="NO TEXT"
modellist2 = list()
difflex = 0

# get example for table
agreeinputword = subset(norawpostverbnotargout.df,measure == mainMeasure  & layer == "word")
exampleTable = agreeinputword[1:4,c("Number","Agreement","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##          Number Agreement                          Example
## 112375   Plural   Control      the boy -s take a stick . .
## 112479   Plural Violation  the boy -s take -ss a stick . .
## 112583 Singular   Control     the boy take -ss a stick . .
## 112687 Singular Violation         the boy take a stick . .
```

```r
agreeinput  = subset(norawpostverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj

  agreeinput$cplural = ifelse(agreeinput$Number=="Plural",0.5,-0.5)
  agreeinput$cviolation = ifelse(agreeinput$Agreement=="Violation",0.5,-0.5)
  agreeinput$cp6layer = ifelse(agreeinput$layer==p600layer,0.5,-0.5)

  # this is the main mixed model with centered variables
    randomeff = paste(" + (1 + cviolation*cp6layer |sub)")  #Agreement * layer 
      if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ cviolation*cp6layer",randomeff))
  omnimodel = lmer(formu, agreeinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  modellist2 = modelComparison(omnimodel) # create anova table
  # this model is for posthocs
  formu2 = as.formula(paste(dependMeasure,"~ Agreement*layer",randomeff))
  omnimodelFactor = lmer(formu2, agreeinput)
  #print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ Agreement | layer)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)

  difflex = abs(posthocs$`layer = word`$test$coefficients[[1]])
}
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cviolation * cp6layer + (1 + cviolation * cp6layer |  
##     sub)
##    Data: agreeinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 7411.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8373 -0.3443  0.0001  0.2544  5.9185 
## 
## Random effects:
##  Groups   Name                Variance Std.Dev. Corr          
##  sub      (Intercept)         0.2806   0.5297                 
##           cviolation          0.4496   0.6705   0.88          
##           cp6layer            0.9643   0.9820   1.00 0.90     
##           cviolation:cp6layer 2.1000   1.4491   0.92 0.99 0.94
##  Residual                     1.2446   1.1156                 
## Number of obs: 2400, groups:  sub, 10
## 
## Fixed effects:
##                     Estimate Std. Error t value
## (Intercept)           2.2591     0.1691  13.363
## cviolation            2.2993     0.2169  10.602
## cp6layer              1.0973     0.3139   3.496
## cviolation:cp6layer   3.4465     0.4672   7.377
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cp6lyr
## cviolation  0.853               
## cp6layer    0.980  0.869        
## cvltn:cp6ly 0.897  0.954  0.910 
## [1] "remove . ~ . -  cviolation:cp6layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: agreeinput
## Models:
## model2: abssum ~ cviolation + cp6layer + (1 + cviolation * cp6layer | 
## model2:     sub)
## model: abssum ~ cviolation * cp6layer + (1 + cviolation * cp6layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 14 7448.2 7529.1 -3710.1   7420.2                             
## model  15 7430.7 7517.4 -3700.3   7400.7 19.509      1  1.001e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation:cp6layer  *** "
## [1] "remove . ~ . -  cp6layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: agreeinput
## Models:
## model2: abssum ~ cviolation + (1 + cviolation * cp6layer | sub)
## model: abssum ~ cviolation + cp6layer + (1 + cviolation * cp6layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2 13 7452.9 7528.1 -3713.5   7426.9                            
## model  14 7448.2 7529.1 -3710.1   7420.2 6.7507      1   0.009371 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cp6layer  *** "
## [1] "remove . ~ . -  cviolation "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: agreeinput
## Models:
## model2: abssum ~ (1 + cviolation * cp6layer | sub)
## model: abssum ~ cviolation + (1 + cviolation * cp6layer | sub)
##        Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)   
## model2 12 7458.8 7528.2 -3717.4   7434.8                           
## model  13 7452.9 7528.1 -3713.5   7426.9 7.879      1   0.005001 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation  *** "
```

```
## Note: df set to 9
```

```
## $`layer = word`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)    
## Control - Violation == 0 -0.57607    0.07023  -8.203 1.81e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## $`layer = hidden`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)    
## Control - Violation == 0  -4.0225     0.4453  -9.034 8.28e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

```r
  # figure for paper
# mapping = aes_string(x="depth", y=dependMeasure, colour = "Agreement",linetype="Agreement")
  drawERP(agreeinput,dependMeasure,"Agreement",  span=1, timeline.y=5.2)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/agree.png",width=figwidth,height=figheight)
  }
n4P6cor = computeN4P6correlation(agreeinput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.635099307875991"
```

```r
n4P6cor = computeN4P6correlation2(agreeinput,"Control","Violation",cond="Agreement")
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.347896923432525"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex
The model received active transitive input with singular subjects 75\% of the time and plural subjects 25\% of the time. 

This is an example of the stimuli used in the model.


\begin{tabular}{l|l|l}
\hline
Number & Agreement & Example\\
\hline
Plural & Control & the boy -s take a stick . .\\
\hline
Plural & Violation & the boy -s take -ss a stick . .\\
\hline
Singular & Control & the boy take -ss a stick . .\\
\hline
Singular & Violation & the boy take a stick . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + cviolation*cp6layer |sub)

Mixed Model Results
a main effect of cviolation ,$\beta$=2.3, SE=0.22, $\chi^2$(1)=7.88, p=0.005 
a main effect of cp6layer ,$\beta$=1.1, SE=0.31, $\chi^2$(1)=6.75, p=0.0094 
@@an interaction of cviolation:cp6layer ,$\beta$=3.4, SE=0.47, $\chi^2$(1)=19.51, p$<$0.001 
 
Posthoc tests for  layer = word 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 0.5761, t(9)=8.2, p$<$0.001 
Posthoc tests for  layer = hidden 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 4.0225, t(9)=9.03, p$<$0.001 
ratio  Control - Violation   6.982  rev  0.143
```




## Tense  Allen et al. (2003)


```r
write("tense", stderr())
tense.df = subset(adultdata.df,  str_detect(Condition,"(TENSECONT|TENSEVIOL)") )
tense.df$Condition = factor(tense.df$Condition,labels=c("Control","Violation"))
verbpos = which(tense.df$wordcat == "VERBT")
wordcatsamelen = which(tense.df$wordcat!=tense.df$wordcat[1])[1]-1
verb.df = tense.df[verbpos,] #  verb
postverb.df = tense.df[verbpos + wordcatsamelen,] # position after verb
postverb.df=postverb.df[!is.na(postverb.df$Condition),]

print(head(postverb.df[postverb.df$measure=="target",c("word","wordcat","Condition","Example","tick")]))
```

```
##        word wordcat Condition
## 139166  the     DET   Control
## 139283  -ed      ED Violation
## 147252  the     DET   Control
## 147369  -ed      ED Violation
## 155546  the     DET   Control
## 155676  -ed      ED Violation
##                                              Example tick
## 139166           a father will drink the beer -s . .    5
## 139283       a father will drink -ed the beer -s . .    5
## 147252         the driver will taste the wine -s . .    5
## 147369     the driver will taste -ed the wine -s . .    5
## 155546      the driver -s will taste the beer -s . .    6
## 155676  the driver -s will taste -ed the beer -s . .    6
```

```r
showWordOutError("the,a,X.ss,X.ing,X.ed,X.par",c("Condition","measure"),postverb.df)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png)


```r
corrHiddenOutputInputDeriv(postverb.df,"Condition")
```

```
## Condition
##   Control Violation 
##         8         8
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)

```r
myggsave("img/actderivtense.png",width=6,height=6)
```


```r
# make erp figure with different measures
norawpostverbnotargout.df=removeWordsDrawERP(postverb.df,"value","Condition")
```

```
##   sub  epoch word wordcat contfunc                              Example
## 1  s0 100000  the     DET        F  a father will drink the beer -s . .
## 2  s0 100000  the     DET        F  a father will drink the beer -s . .
## 3  s0 100000  the     DET        F  a father will drink the beer -s . .
## 4  s0 100000  the     DET        F  a father will drink the beer -s . .
## 5  s0 100000  the     DET        F  a father will drink the beer -s . .
## 6  s0 100000  the     DET        F  a father will drink the beer -s . .
##   Condition tick    layer    measure depth variable     value
## 1   Control    5     word   outderiv     1   abssum 1.2117450
## 2   Control    5     word inputderiv     1   abssum 0.3494872
## 3   Control    5 compress   outderiv     2   abssum 5.7298633
## 4   Control    5 compress inputderiv     2   abssum 0.2838988
## 5   Control    5   hidden   outderiv     3   abssum 3.8026026
## 6   Control    5   hidden inputderiv     3   abssum 0.2746235
```


```r
interactionText="NO TEXT"
modellist2 = list()
# get example for table
tenseexampleword = subset(norawpostverbnotargout.df,measure == mainMeasure  & layer == "word")
exampleTable = tenseexampleword[1:2,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##        Condition                                  Example
## 139168   Control      a father will drink the beer -s . .
## 139285 Violation  a father will drink -ed the beer -s . .
```

```r
 tenseinput  = subset(norawpostverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj
 
  tenseinput$cviolation = ifelse(tenseinput$Condition=="Violation",0.5,-0.5) # one against two
  tenseinput$cp600layer = ifelse(tenseinput$layer==p600layer,0.5,-0.5)

  # this is the main mixed model with centered variables
  randomeff = paste(" + (1 +  cviolation*cp600layer |sub)")  # Condition*layer
  if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ cviolation*cp600layer",randomeff))
  omnimodel = lmer(formu, tenseinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  modellist2 = modelComparison(omnimodel) # create anova table

  # this model is for posthocs
  formu2 = as.formula(paste(dependMeasure,"~ Condition*layer",randomeff))
  omnimodelFactor = lmer(formu2, tenseinput)
  #print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ Condition | layer)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)
  difflex = difflex + abs(posthocs$`layer = word`$test$coefficients[[1]])

}
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## abssum ~ cviolation * cp600layer + (1 + cviolation * cp600layer |  
##     sub)
##    Data: tenseinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 3374
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.5670 -0.5457  0.0004  0.5447  9.3367 
## 
## Random effects:
##  Groups   Name                  Variance Std.Dev. Corr          
##  sub      (Intercept)           0.4298   0.6556                 
##           cviolation            0.9295   0.9641   0.73          
##           cp600layer            1.4997   1.2246   1.00 0.77     
##           cviolation:cp600layer 3.8627   1.9654   0.81 0.99 0.84
##  Residual                       0.9090   0.9534                 
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                       Estimate Std. Error t value
## (Intercept)             2.9088     0.2091  13.909
## cviolation              3.5446     0.3098  11.442
## cp600layer              2.5985     0.3911   6.643
## cviolation:cp600layer   5.5315     0.6312   8.764
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cp600l
## cviolation  0.709               
## cp600layer  0.979  0.750        
## cvltn:cp600 0.786  0.962  0.821 
## [1] "remove . ~ . -  cviolation:cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: tenseinput
## Models:
## model2: abssum ~ cviolation + cp600layer + (1 + cviolation * cp600layer | 
## model2:     sub)
## model: abssum ~ cviolation * cp600layer + (1 + cviolation * cp600layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 14 3416.5 3487.8 -1694.3   3388.5                             
## model  15 3396.0 3472.4 -1683.0   3366.0 22.534      1  2.064e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation:cp600layer  *** "
## [1] "remove . ~ . -  cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: tenseinput
## Models:
## model2: abssum ~ cviolation + (1 + cviolation * cp600layer | sub)
## model: abssum ~ cviolation + cp600layer + (1 + cviolation * cp600layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2 13 3414.6 3480.8 -1694.3   3388.6                         
## model  14 3416.5 3487.8 -1694.3   3388.5 0.1049      1      0.746
## [1] "########## Above comparison for  cp600layer "
## [1] "remove . ~ . -  cviolation "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: tenseinput
## Models:
## model2: abssum ~ (1 + cviolation * cp600layer | sub)
## model: abssum ~ cviolation + (1 + cviolation * cp600layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2 12 3422.6 3483.6 -1699.3   3398.6                            
## model  13 3414.6 3480.8 -1694.3   3388.6 9.9171      1   0.001638 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation  *** "
```

```
## Note: df set to 9
```

```
## $`layer = word`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)    
## Control - Violation == 0 -0.77888    0.08669  -8.984 8.66e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## $`layer = hidden`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)    
## Control - Violation == 0  -6.3104     0.6194  -10.19 3.06e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

```r
  # figure for paper
 # mapping = aes_string(x="depth", y=dependMeasure, colour = "Condition",linetype="Condition")
  drawERP(tenseinput,dependMeasure,"Condition",  timeline.y=8,span=.8)
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/tense.png",width=figwidth,height=figheight)
  }
  
n4P6cor = computeN4P6correlation(tenseinput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.626156749789801"
```

```r
n4P6cor = computeN4P6correlation2(tenseinput,"Control","Violation")
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.419532545097064"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex

This is an example of the stimuli used in the model.


\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
Control & a father will drink the beer -s . .\\
\hline
Violation & a father will drink -ed the beer -s . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 +  cviolation*cp600layer |sub)

Mixed Model Results
a main effect of cviolation ,$\beta$=3.5, SE=0.31, $\chi^2$(1)=9.92, p=0.0016 
no main effect of cp600layer ,$\beta$=2.6, SE=0.39, $\chi^2$(1)=0.1, p=0.746 
@@an interaction of cviolation:cp600layer ,$\beta$=5.5, SE=0.63, $\chi^2$(1)=22.53, p$<$0.001 
 
Posthoc tests for  layer = word 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 0.7789, t(9)=8.98, p$<$0.001 
Posthoc tests for  layer = hidden 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 6.3104, t(9)=10.19, p$<$0.001 
ratio  Control - Violation   8.102  rev  0.123
```


## WORDCAT (Wassenaar and Hagoort, 2005)


```r
write("wordcat", stderr())
cat.df = subset(adultdata.df,  str_detect(Condition,"(CATCONT|CATVIOL)") )
cat.df$Condition=factor(cat.df$Condition,labels=c("Control","Violation"))
verbpos = which(cat.df$wordcat == "VERBI")
wordcatsamelen = which(cat.df$wordcat!=cat.df$wordcat[1])[1]-1
verb.df = cat.df[verbpos,] #  verb
postverb.df = cat.df[verbpos + wordcatsamelen,] # position after verb
postverb.df=postverb.df[!is.na(postverb.df$Condition),]

print(head(postverb.df[postverb.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##        word wordcat Condition                                    Example
## 118340   -s      PL   Control   the grandma was take -ing the nap -s . .
## 118470  -ed      ED Violation  the grandma was take -ing the nap -ed . .
## 152387   -s      PL   Control           the girl take -ed the nap -s . .
## 152504  -ed      ED Violation          the girl take -ed the nap -ed . .
## 165465   -s      PL   Control     a teacher was take -ing the nap -s . .
## 165595  -ed      ED Violation    a teacher was take -ing the nap -ed . .
##        tick
## 118340    8
## 118470    8
## 152387    7
## 152504    7
## 165465    8
## 165595    8
```

```r
showWordOutError("X.ed,X.s,per",c("Condition","measure"),postverb.df)
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)



```r
corrHiddenOutputInputDeriv(postverb.df,"Condition")
```

```
## Condition
##   Control Violation 
##         8         8
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)


```r
#mapping = aes(x=depth, y=value, colour = Condition,linetype=Condition) 
norawpostverbnotargout.df=removeWordsDrawERP(postverb.df,"value", "Condition")
```

```
##   sub  epoch word wordcat contfunc
## 1  s0 100000   -s      PL        F
## 2  s0 100000   -s      PL        F
## 3  s0 100000   -s      PL        F
## 4  s0 100000   -s      PL        F
## 5  s0 100000   -s      PL        F
## 6  s0 100000   -s      PL        F
##                                     Example Condition tick    layer
## 1  the grandma was take -ing the nap -s . .   Control    8     word
## 2  the grandma was take -ing the nap -s . .   Control    8     word
## 3  the grandma was take -ing the nap -s . .   Control    8 compress
## 4  the grandma was take -ing the nap -s . .   Control    8 compress
## 5  the grandma was take -ing the nap -s . .   Control    8   hidden
## 6  the grandma was take -ing the nap -s . .   Control    8   hidden
##      measure depth variable      value
## 1   outderiv     1   abssum  6.2936400
## 2 inputderiv     1   abssum  1.6822183
## 3   outderiv     2   abssum 63.0345795
## 4 inputderiv     2   abssum  0.7655548
## 5   outderiv     3   abssum 24.1052016
## 6 inputderiv     3   abssum  1.2524126
```


```r
interactionText="NO TEXT"
modellist2 = list()

# get example for table
catwordexample = subset(norawpostverbnotargout.df,measure == mainMeasure  & layer == "word")
exampleTable = catwordexample[1:4,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##        Condition                                    Example
## 118342   Control   the grandma was take -ing the nap -s . .
## 118472 Violation  the grandma was take -ing the nap -ed . .
## 152389   Control           the girl take -ed the nap -s . .
## 152506 Violation          the girl take -ed the nap -ed . .
```

```r
catinput  = subset(norawpostverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj

  catinput$cviolation = ifelse(catinput$Condition=="Violation",0.5,-0.5) # one against two
  catinput$cp600layer = ifelse(catinput$layer==p600layer,0.5,-0.5)

  # this is the main mixed model with centered variables
  randomeff = paste(" + (1 + Condition * layer |sub)")
    if (noslopes){ randomeff = " + (1 | sub)" }
  formu = as.formula(paste(dependMeasure,"~ cviolation*cp600layer",randomeff))
  omnimodel = lmer(formu, catinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  modellist2 = modelComparison(omnimodel) # create anova table

  # this model is for posthocs
  formu2 = as.formula(paste(dependMeasure,"~ Condition*layer",randomeff))
  omnimodelFactor = lmer(formu2, catinput)
  #print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ Condition | layer)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)
  difflex = difflex + abs(posthocs$`layer = word`$test$coefficients[[1]])

}
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cviolation * cp600layer + (1 + Condition * layer | sub)
##    Data: catinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 3198.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.9333 -0.2611 -0.0010  0.1601  6.2477 
## 
## Random effects:
##  Groups   Name                           Variance Std.Dev. Corr       
##  sub      (Intercept)                    0.03168  0.1780              
##           ConditionViolation             0.39445  0.6280    0.05      
##           layerhidden                    0.48029  0.6930    0.81  0.32
##           ConditionViolation:layerhidden 3.06953  1.7520   -0.18  0.29
##  Residual                                0.76678  0.8757              
##       
##       
##       
##       
##   0.27
##       
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                       Estimate Std. Error t value
## (Intercept)             2.0689     0.2751   7.519
## cviolation              1.2334     0.3881   3.178
## cp600layer              0.9069     0.4006   2.264
## cviolation:cp600layer   2.3415     0.5632   4.158
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cp600l
## cviolation  0.814               
## cp600layer  0.916  0.791        
## cvltn:cp600 0.669  0.848  0.827 
## [1] "remove . ~ . -  cviolation:cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: catinput
## Models:
## model2: abssum ~ cviolation + cp600layer + (1 + Condition * layer | sub)
## model: abssum ~ cviolation * cp600layer + (1 + Condition * layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2 14 3231.3 3302.6 -1601.7   3203.3                            
## model  15 3222.6 3298.9 -1596.3   3192.6 10.718      1   0.001061 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation:cp600layer  *** "
## [1] "remove . ~ . -  cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: catinput
## Models:
## model2: abssum ~ cviolation + (1 + Condition * layer | sub)
## model: abssum ~ cviolation + cp600layer + (1 + Condition * layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2 13 3230.8 3297.0 -1602.4   3204.8                         
## model  14 3231.3 3302.6 -1601.7   3203.3 1.5408      1     0.2145
## [1] "########## Above comparison for  cp600layer "
## [1] "remove . ~ . -  cviolation "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: catinput
## Models:
## model2: abssum ~ (1 + Condition * layer | sub)
## model: abssum ~ cviolation + (1 + Condition * layer | sub)
##        Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
## model2 12 3228.8 3289.9 -1602.4   3204.8                        
## model  13 3230.8 3297.0 -1602.4   3204.8 2e-04      1     0.9877
## [1] "########## Above comparison for  cviolation "
```

```
## Note: df set to 9
```

```
## $`layer = word`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)
## Control - Violation == 0 -0.06268    0.21108  -0.297    0.773
## (Adjusted p values reported -- single-step method)
## 
## 
## $`layer = hidden`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)   
## Control - Violation == 0  -2.4042     0.6444  -3.731  0.00469 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

```r
  # figure for paper
#  mapping = aes_string(x="depth", y=dependMeasure, colour = "Condition",linetype="Condition")
  drawERP(catinput,dependMeasure,"Condition", timeline.y=4,span=0.8)
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE??n
    myggsave("img/cat.png",width=figwidth,height=figheight)
  }
  
n4P6cor = computeN4P6correlation(catinput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.53217912711318"
```

```r
n4P6cor = computeN4P6correlation2(catinput,"Control","Violation")
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.549173339243751"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex

This is an example of the stimuli used in the model.


\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
Control & the grandma was take -ing the nap -s . .\\
\hline
Violation & the grandma was take -ing the nap -ed . .\\
\hline
Control & the girl take -ed the nap -s . .\\
\hline
Violation & the girl take -ed the nap -ed . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + Condition * layer |sub)

Mixed Model Results
no main effect of cviolation ,$\beta$=1.2, SE=0.39, $\chi^2$(1)=0, p=0.988 
no main effect of cp600layer ,$\beta$=0.91, SE=0.4, $\chi^2$(1)=1.54, p=0.214 
@@an interaction of cviolation:cp600layer ,$\beta$=2.3, SE=0.56, $\chi^2$(1)=10.72, p=0.0011 
 
Posthoc tests for  layer = word 
 @@Posthoc: There was no difference for Control - Violation, p=0.7733 
Posthoc tests for  layer = hidden 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 2.4042, t(9)=3.73, p=0.0047 
ratio  Control - Violation   38.344  rev  0.026
```



## SUBCAT  Osterhout and Holcomb (1992)


```r
write("subcat", stderr())
sub.df = subset(adultdata.df,  str_detect(Condition,"(SUBCONT|SUBVIOL)") )
sub.df$Condition=factor(sub.df$Condition,labels=c("Control","Violation"))
preppos = which(sub.df$word %in% c("near","by"))
prep.df = sub.df[preppos,] #  verb

print(head(prep.df[prep.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##        word wordcat Condition                                   Example
## 114284 near    PREP   Control     a sister is nap -ing near the boy . .
## 114414 near    PREP Violation    a sister is take -ing near the boy . .
## 116143   by      BY   Control           the uncle -s walk by tea -s . .
## 116260   by      BY Violation          the uncle -s drink by tea -s . .
## 117677   by      BY   Control  a grandma jump -ed by the brother -s . .
## 117807   by      BY Violation   a grandma sip -ed by the brother -s . .
##        tick
## 114284    6
## 114414    6
## 116143    5
## 116260    5
## 117677    5
## 117807    5
```

```r
showWordOutError("near,by,the,per",c("Condition","measure"),prep.df)
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30-1.png)



```r
corrHiddenOutputInputDeriv(prep.df,"Condition")
```

```
## Condition
##   Control Violation 
##         8         8
```

![plot of chunk unnamed-chunk-31](figure/unnamed-chunk-31-1.png)


```r
#mapping = aes(x=depth, y=value, colour = Condition,linetype=Condition) 
norawprepnotargout.df=removeWordsDrawERP(prep.df,"value","Condition")
```

```
##   sub  epoch word wordcat contfunc                                Example
## 1  s0 100000 near    PREP        F  a sister is nap -ing near the boy . .
## 2  s0 100000 near    PREP        F  a sister is nap -ing near the boy . .
## 3  s0 100000 near    PREP        F  a sister is nap -ing near the boy . .
## 4  s0 100000 near    PREP        F  a sister is nap -ing near the boy . .
## 5  s0 100000 near    PREP        F  a sister is nap -ing near the boy . .
## 6  s0 100000 near    PREP        F  a sister is nap -ing near the boy . .
##   Condition tick    layer    measure depth variable      value
## 1   Control    6     word   outderiv     1   abssum  8.7414080
## 2   Control    6     word inputderiv     1   abssum  1.7712038
## 3   Control    6 compress   outderiv     2   abssum 20.6027973
## 4   Control    6 compress inputderiv     2   abssum  0.4111479
## 5   Control    6   hidden   outderiv     3   abssum  8.2239300
## 6   Control    6   hidden inputderiv     3   abssum  0.5146847
```


```r
interactionText="NO TEXT"
modellist2 = list()

# get example for table
catwordexample = subset(norawprepnotargout.df,measure == mainMeasure  & layer == "word")
exampleTable = catwordexample[1:4,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##        Condition                                 Example
## 114286   Control   a sister is nap -ing near the boy . .
## 114416 Violation  a sister is take -ing near the boy . .
## 116145   Control         the uncle -s walk by tea -s . .
## 116262 Violation        the uncle -s drink by tea -s . .
```

```r
subinput  = subset(norawprepnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj

  subinput$cviolation = ifelse(subinput$Condition=="Violation",0.5,-0.5) # one against two
  subinput$cp600layer = ifelse(subinput$layer==p600layer,0.5,-0.5)

  # this is the main mixed model with centered variables
  randomeff = paste(" + (1 + cviolation*cp600layer |sub)")
    if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ cviolation*cp600layer",randomeff))
  omnimodel = lmer(formu, subinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  modellist2 = modelComparison(omnimodel) # create anova table

  # this model is for posthocs
  formu2 = as.formula(paste(dependMeasure,"~ Condition*layer",randomeff))
  omnimodelFactor = lmer(formu2, subinput)
  #print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ Condition | layer)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)
    difflex = difflex + abs(posthocs$`layer = word`$test$coefficients[[1]])

}
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## abssum ~ cviolation * cp600layer + (1 + cviolation * cp600layer |  
##     sub)
##    Data: subinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 3627.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2327 -0.3512  0.0002  0.1081  7.7900 
## 
## Random effects:
##  Groups   Name                  Variance Std.Dev. Corr          
##  sub      (Intercept)           0.1307   0.3615                 
##           cviolation            0.6082   0.7798   0.94          
##           cp600layer            0.4659   0.6826   1.00 0.94     
##           cviolation:cp600layer 2.6521   1.6285   0.95 1.00 0.95
##  Residual                       1.1486   1.0717                 
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                       Estimate Std. Error t value
## (Intercept)             2.3365     0.1184  19.731
## cviolation              1.8520     0.2543   7.284
## cp600layer              1.0476     0.2245   4.666
## cviolation:cp600layer   2.9569     0.5296   5.583
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cp600l
## cviolation  0.879               
## cp600layer  0.928  0.881        
## cvltn:cp600 0.891  0.943  0.892 
## [1] "remove . ~ . -  cviolation:cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: subinput
## Models:
## model2: abssum ~ cviolation + cp600layer + (1 + cviolation * cp600layer | 
## model2:     sub)
## model: abssum ~ cviolation * cp600layer + (1 + cviolation * cp600layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 14 3660.6 3731.9 -1816.3   3632.6                             
## model  15 3647.7 3724.0 -1808.8   3617.7 14.942      1  0.0001109 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation:cp600layer  *** "
## [1] "remove . ~ . -  cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: subinput
## Models:
## model2: abssum ~ cviolation + (1 + cviolation * cp600layer | sub)
## model: abssum ~ cviolation + cp600layer + (1 + cviolation * cp600layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
## model2 13 3658.8 3724.9 -1816.4   3632.8                        
## model  14 3660.6 3731.9 -1816.3   3632.6 0.115      1     0.7346
## [1] "########## Above comparison for  cp600layer "
## [1] "remove . ~ . -  cviolation "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: subinput
## Models:
## model2: abssum ~ (1 + cviolation * cp600layer | sub)
## model: abssum ~ cviolation + (1 + cviolation * cp600layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2 12 3664.4 3725.4 -1820.2   3640.4                            
## model  13 3658.8 3724.9 -1816.4   3632.8 7.6001      1   0.005837 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation  *** "
```

```
## Note: df set to 9
```

```
## $`layer = word`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)   
## Control - Violation == 0 -0.37355    0.08854  -4.219  0.00224 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## $`layer = hidden`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)    
## Control - Violation == 0  -3.3304     0.5116   -6.51  0.00011 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

```r
  # figure for paper
    #mapping = aes_string(x="depth", y=dependMeasure, colour = "Condition",linetype="Condition")
  drawERP(subinput,dependMeasure,"Condition", timeline.y=4.7)
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/subjcat.png",width=figwidth,height=figheight)
  }
  
n4P6cor = computeN4P6correlation(subinput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.635890912765104"
```

```r
n4P6cor = computeN4P6correlation2(subinput,"Control","Violation")
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.189705337541494"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex

This is an example of the stimuli used in the model.


\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
Control & a sister is nap -ing near the boy . .\\
\hline
Violation & a sister is take -ing near the boy . .\\
\hline
Control & the uncle -s walk by tea -s . .\\
\hline
Violation & the uncle -s drink by tea -s . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + cviolation*cp600layer |sub)

Mixed Model Results
a main effect of cviolation ,$\beta$=1.9, SE=0.25, $\chi^2$(1)=7.6, p=0.0058 
no main effect of cp600layer ,$\beta$=1, SE=0.22, $\chi^2$(1)=0.11, p=0.735 
@@an interaction of cviolation:cp600layer ,$\beta$=3, SE=0.53, $\chi^2$(1)=14.94, p$<$0.001 
 
Posthoc tests for  layer = word 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 0.3735, t(9)=4.22, p=0.0022 
Posthoc tests for  layer = hidden 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 3.3304, t(9)=6.51, p$<$0.001 
ratio  Control - Violation   8.917  rev  0.112
```


## GARDEN PATH


```r
write("garden", stderr())
garden.df = subset(adultdata.df,  str_detect(Condition,"(GARDSC|GARDAMB)") & tick > 4 )
garden.df$Condition=factor(garden.df$Condition,labels=c("Unambiguous","Ambiguous"),levels=c("GARDSC","GARDAMB"))
garden.df$tarword = str_match(garden.df$Example,"(believe|know)(.*?) (the|a) ([A-z]+)( -s)* ([A-z.]+)*")[,7]
garden.df$strlen = sapply(gregexpr("[^ ]+", garden.df$Example), function(x) sum(x > 0))
gardenpos = which(garden.df$word == garden.df$tarword)
gardentar.df = garden.df[gardenpos,] #  verb
gardentar.df= subset(gardentar.df,strlen != tick)
gardentar.df$strlen = NULL
gardentar.df$tarword = NULL

print(head(gardentar.df[gardentar.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##        word wordcat   Condition
## 135110  run   VERBI Unambiguous
## 135240  run   VERBI   Ambiguous
## 150502  nap   VERBI Unambiguous
## 150632  nap   VERBI   Ambiguous
## 152998 walk   VERBI Unambiguous
## 153128 walk   VERBI   Ambiguous
##                                                   Example tick
## 135110  the driver believe -ed that the uncle run -ss . .    8
## 135240       the driver believe -ed the uncle run -ss . .    7
## 150502         man -s will know that the girl nap -ss . .    8
## 150632              man -s will know the girl nap -ss . .    7
## 152998    a grandpa know -ed that the sister walk -ss . .    8
## 153128         a grandpa know -ed the sister walk -ss . .    7
```

```r
showWordOutError("is,are,will,was,were,jump,run,walk,nap,per",c("Condition","measure"),gardentar.df)
```

![plot of chunk unnamed-chunk-35](figure/unnamed-chunk-35-1.png)



```r
corrHiddenOutputInputDeriv(gardentar.df,"Condition")
```

```
## Condition
## Unambiguous   Ambiguous 
##           8           8
```

![plot of chunk unnamed-chunk-36](figure/unnamed-chunk-36-1.png)



```r
#mapping = aes(x=depth, y=value, colour = Condition,linetype=Condition) 
norawgardennotargout.df=removeWordsDrawERP(gardentar.df,"value","Condition")
```

```
##   sub  epoch word wordcat contfunc
## 1  s0 100000  run   VERBI        C
## 2  s0 100000  run   VERBI        C
## 3  s0 100000  run   VERBI        C
## 4  s0 100000  run   VERBI        C
## 5  s0 100000  run   VERBI        C
## 6  s0 100000  run   VERBI        C
##                                              Example   Condition tick
## 1  the driver believe -ed that the uncle run -ss . . Unambiguous    8
## 2  the driver believe -ed that the uncle run -ss . . Unambiguous    8
## 3  the driver believe -ed that the uncle run -ss . . Unambiguous    8
## 4  the driver believe -ed that the uncle run -ss . . Unambiguous    8
## 5  the driver believe -ed that the uncle run -ss . . Unambiguous    8
## 6  the driver believe -ed that the uncle run -ss . . Unambiguous    8
##      layer    measure depth variable      value
## 1     word   outderiv     1   abssum  4.0180600
## 2     word inputderiv     1   abssum  1.5022473
## 3 compress   outderiv     2   abssum 18.6331062
## 4 compress inputderiv     2   abssum  0.2655750
## 5   hidden   outderiv     3   abssum  6.6314658
## 6   hidden inputderiv     3   abssum  0.1725179
```


```r
interactionText="NO TEXT"
modellist2 = list()

# get example for table
gardenexample = subset(norawgardennotargout.df,measure == mainMeasure  & layer == "word")
exampleTable = gardenexample[1:6,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##          Condition                                            Example
## 135112 Unambiguous  the driver believe -ed that the uncle run -ss . .
## 135242   Ambiguous       the driver believe -ed the uncle run -ss . .
## 150504 Unambiguous         man -s will know that the girl nap -ss . .
## 150634   Ambiguous              man -s will know the girl nap -ss . .
## 153000 Unambiguous    a grandpa know -ed that the sister walk -ss . .
## 153130   Ambiguous         a grandpa know -ed the sister walk -ss . .
```

```r
gardeninput  = subset(norawgardennotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj
  gardeninput$cviolation = ifelse(gardeninput$Condition=="Ambiguous",0.5,-0.5) # one against two
  gardeninput$cp600layer = ifelse(gardeninput$layer==p600layer,0.5,-0.5)

  # this is the main mixed model with centered variables
  randomeff = paste(" + (1+ cviolation*cp600layer |sub)") # Condition * layer
    if (noslopes){ randomeff = " + (1 | sub)"}
    print(paste("************        RandomEffect ->",randomeff))
  formu = as.formula(paste(dependMeasure,"~ cviolation*cp600layer",randomeff))
  omnimodel = lmer(formu, gardeninput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
    modellist2 = modelComparison(omnimodel) # create anova table

  # this model is for posthocs
  formu2 = as.formula(paste(dependMeasure,"~ Condition*layer",randomeff))
  omnimodelFactor = lmer(formu2, gardeninput)
  #print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ Condition | layer)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)
  difflex = difflex + abs(posthocs$`layer = word`$test$coefficients[[1]])

}
```

```
## [1] "************        RandomEffect ->  + (1+ cviolation*cp600layer |sub)"
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## abssum ~ cviolation * cp600layer + (1 + cviolation * cp600layer |  
##     sub)
##    Data: gardeninput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 1964.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.6090 -0.2296 -0.0007  0.1306  9.0168 
## 
## Random effects:
##  Groups   Name                  Variance Std.Dev. Corr          
##  sub      (Intercept)           0.1041   0.3227                 
##           cviolation            0.2872   0.5359   0.84          
##           cp600layer            0.3863   0.6215   1.00 0.82     
##           cviolation:cp600layer 1.2470   1.1167   0.83 1.00 0.81
##  Residual                       0.2805   0.5296                 
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                       Estimate Std. Error t value
## (Intercept)             1.6311     0.1032  15.809
## cviolation              0.8070     0.1722   4.686
## cp600layer             -0.4814     0.1989  -2.420
## cviolation:cp600layer   1.2523     0.3584   3.494
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cp600l
## cviolation  0.817               
## cp600layer  0.977  0.802        
## cvltn:cp600 0.806  0.969  0.790 
## [1] "remove . ~ . -  cviolation:cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: gardeninput
## Models:
## model2: abssum ~ cviolation + cp600layer + (1 + cviolation * cp600layer | 
## model2:     sub)
## model: abssum ~ cviolation * cp600layer + (1 + cviolation * cp600layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2 14 1988.1 2059.3 -980.05   1960.1                            
## model  15 1981.5 2057.9 -975.76   1951.5 8.5649      1   0.003427 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation:cp600layer  *** "
## [1] "remove . ~ . -  cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: gardeninput
## Models:
## model2: abssum ~ cviolation + (1 + cviolation * cp600layer | sub)
## model: abssum ~ cviolation + cp600layer + (1 + cviolation * cp600layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 13 2000.8 2067.0 -987.40   1974.8                             
## model  14 1988.1 2059.3 -980.05   1960.1 14.706      1  0.0001257 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cp600layer  *** "
## [1] "remove . ~ . -  cviolation "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: gardeninput
## Models:
## model2: abssum ~ (1 + cviolation * cp600layer | sub)
## model: abssum ~ cviolation + (1 + cviolation * cp600layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
## model2 12 2003.9 2064.9 -989.94   1979.9                           
## model  13 2000.8 2067.0 -987.40   1974.8 5.0748      1    0.02428 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation  *** "
```

```
## Note: df set to 9
```

```
## $`layer = word`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                              Estimate Std. Error t value Pr(>|t|)   
## Unambiguous - Ambiguous == 0 -0.18085    0.04402  -4.109  0.00264 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## $`layer = hidden`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                              Estimate Std. Error t value Pr(>|t|)   
## Unambiguous - Ambiguous == 0  -1.4331     0.3487   -4.11  0.00264 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

```r
  # figure for paper
#    mapping = aes_string(x="depth", y=dependMeasure, colour = "Condition",linetype="Condition")
  drawERP(gardeninput,dependMeasure, "Condition", timeline.y=2.2)
```

![plot of chunk unnamed-chunk-38](figure/unnamed-chunk-38-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/garden.png",width=figwidth,height=figheight)
  }
  
n4P6cor = computeN4P6correlation(gardeninput)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.490790299638552"
```

```r
n4P6cor = computeN4P6correlation2(gardeninput,"Unambiguous","Ambiguous")
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation 0.0201289893837347"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex
UPDATE THIS

This is an example of the stimuli used in the model.


\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
Unambiguous & the driver believe -ed that the uncle run -ss . .\\
\hline
Ambiguous & the driver believe -ed the uncle run -ss . .\\
\hline
Unambiguous & man -s will know that the girl nap -ss . .\\
\hline
Ambiguous & man -s will know the girl nap -ss . .\\
\hline
Unambiguous & a grandpa know -ed that the sister walk -ss . .\\
\hline
Ambiguous & a grandpa know -ed the sister walk -ss . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1+ cviolation*cp600layer |sub)

Mixed Model Results
a main effect of cviolation ,$\beta$=0.81, SE=0.17, $\chi^2$(1)=5.07, p=0.024 
a main effect of cp600layer ,$\beta$=-0.48, SE=0.2, $\chi^2$(1)=14.71, p$<$0.001 
@@an interaction of cviolation:cp600layer ,$\beta$=1.3, SE=0.36, $\chi^2$(1)=8.56, p=0.0034 
 
Posthoc tests for  layer = word 
 @@Posthoc: There was a significant difference for Unambiguous - Ambiguous, diff = 0.1809, t(9)=4.11, p=0.0026 
Posthoc tests for  layer = hidden 
 @@Posthoc: There was a significant difference for Unambiguous - Ambiguous, diff = 1.4331, t(9)=4.11, p=0.0026 
ratio  Unambiguous - Ambiguous   7.922  rev  0.126
```



## Semantic P6 (Kim and Osterhout)


```r
write("semP6", stderr())
semP6.df = subset(adultdata.df,  str_detect(Condition,"(PASSCONT|ACTCONT|SEMATTR|NOATTR)") )
semP6.df$Condition = factor(semP6.df$Condition, labels=c("Active Control","Passive Control", "Role Reversal"),levels = c("ACTCONT","PASSCONT","SEMATTR"))
#"NOATTR", "No Attractor",
verbpos = which(semP6.df$wordcat == "VERBT")
wordcatsamelen = which(semP6.df$wordcat!=semP6.df$wordcat[1])[1]-1
verb.df = semP6.df[verbpos,] #  verb
postverb.df = semP6.df[verbpos + wordcatsamelen,] # position after verb
postverb.df=postverb.df[!is.na(postverb.df$Condition),]

print(head(postverb.df[postverb.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##        word wordcat       Condition
## 114531 -par     PAR Passive Control
## 114661 -ing     ING  Active Control
## 114778 -ing     ING   Role Reversal
## 116377 -par     PAR Passive Control
## 116507 -ing     ING  Active Control
## 116624 -ing     ING   Role Reversal
##                                        Example tick
## 114531     the tea is sip -par by a friend . .    5
## 114661        the friend is sip -ing a tea . .    5
## 114778        the tea is sip -ing a friend . .    5
## 116377  a wine is drink -par by the driver . .    5
## 116507     a driver is drink -ing the wine . .    5
## 116624     a wine is drink -ing the driver . .    5
```

```r
showWordOutError("the,a,X.ss,X.ing,X.ed,X.par",c("Condition","measure"),postverb.df)
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-40-1.png)

```r
showWordOutError("the,a,X.ss,X.ing,X.ed,X.par",c("Condition","measure"),verb.df)
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-40-2.png)


```r
computeClozeERPCorr(postverb.df,dependMeasure,"word")
```

```
## [1] "Correlation output prediction and abssum error of word is -0.999999999999961"
```

```r
computeClozeERPCorr(postverb.df,dependMeasure,"hidden")
```

```
## [1] "Correlation output prediction and abssum error of hidden is -0.894997273980858"
```

```r
corrHiddenOutputInputDeriv(postverb.df,"Condition")
```

```
## Condition
##  Active Control Passive Control   Role Reversal 
##               8               8               8
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-41-1.png)



```r
#mapping = aes_string(x="depth", y="value", colour = "Condition",linetype="Condition")
norawpostverbnotargout.df=removeWordsDrawERP(postverb.df,"value","Condition")
```

```
##   sub  epoch word wordcat contfunc                              Example
## 1  s0 100000 -par     PAR        F  the tea is sip -par by a friend . .
## 2  s0 100000 -par     PAR        F  the tea is sip -par by a friend . .
## 3  s0 100000 -par     PAR        F  the tea is sip -par by a friend . .
## 4  s0 100000 -par     PAR        F  the tea is sip -par by a friend . .
## 5  s0 100000 -par     PAR        F  the tea is sip -par by a friend . .
## 6  s0 100000 -par     PAR        F  the tea is sip -par by a friend . .
##         Condition tick    layer    measure depth variable     value
## 1 Passive Control    5     word   outderiv     1   abssum 1.0241180
## 2 Passive Control    5     word inputderiv     1   abssum 0.0470991
## 3 Passive Control    5 compress   outderiv     2   abssum 1.4531393
## 4 Passive Control    5 compress inputderiv     2   abssum 0.0880563
## 5 Passive Control    5   hidden   outderiv     3   abssum 1.8791545
## 6 Passive Control    5   hidden inputderiv     3   abssum 0.1314669
```

```r
norawverbnotargout.df=removeWordsDrawERP(verb.df,"value","Condition")
```

```
##   sub  epoch word wordcat contfunc                              Example
## 1  s0 100000  sip   VERBT        C  the tea is sip -par by a friend . .
## 2  s0 100000  sip   VERBT        C  the tea is sip -par by a friend . .
## 3  s0 100000  sip   VERBT        C  the tea is sip -par by a friend . .
## 4  s0 100000  sip   VERBT        C  the tea is sip -par by a friend . .
## 5  s0 100000  sip   VERBT        C  the tea is sip -par by a friend . .
## 6  s0 100000  sip   VERBT        C  the tea is sip -par by a friend . .
##         Condition tick    layer    measure depth variable     value
## 1 Passive Control    4     word   outderiv     1   abssum 21.320910
## 2 Passive Control    4     word inputderiv     1   abssum  1.906195
## 3 Passive Control    4 compress   outderiv     2   abssum 24.644402
## 4 Passive Control    4 compress inputderiv     2   abssum  0.634280
## 5 Passive Control    4   hidden   outderiv     3   abssum 18.218561
## 6 Passive Control    4   hidden inputderiv     3   abssum  1.124015
```


```r
interactionText="NO TEXT"
modellist2 = list()
#norawpostverbnotargout.df=subset(norawpostverbnotargout.df,Condition != "No Attractor")
#norawpostverbnotargout.df=norawpostverbnotargout.df

# get example for table
semP6exampleword = subset(norawpostverbnotargout.df,measure == mainMeasure  & layer == "word")
exampleTable = semP6exampleword[1:12,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##              Condition                                   Example
## 114533 Passive Control       the tea is sip -par by a friend . .
## 114663  Active Control          the friend is sip -ing a tea . .
## 114780   Role Reversal          the tea is sip -ing a friend . .
## 116379 Passive Control    a wine is drink -par by the driver . .
## 116509  Active Control       a driver is drink -ing the wine . .
## 116626   Role Reversal       a wine is drink -ing the driver . .
## 117315 Passive Control  the water is drink -par by the clerk . .
## 117445  Active Control     the clerk is drink -ing the water . .
## 117562   Role Reversal     the water is drink -ing the clerk . .
## 117939 Passive Control       a tea is taste -par by the aunt . .
## 118069  Active Control          a aunt is taste -ing the tea . .
## 118186   Role Reversal          a tea is taste -ing the aunt . .
```

```r
semP6input  = subset(norawpostverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )
semP6inputverb  = subset(norawverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj

  options(contrasts=c("contr.treatment", "contr.treatment"))
#  semP6input$Condition = factor(semP6input$Condition,levels=c("Active Control", "Passive Control", "No Attractor", "Semantic Attractor"))
  semP6input$cpassive = 0
  semP6input$cpassive[semP6input$Condition=="Passive Control"]= 0.5
  semP6input$cpassive[semP6input$Condition=="Active Control"]=  -0.5
  semP6input$semattvsgram = 0
  semP6input$semattvsgram[semP6input$Condition=="Role Reversal"] = 1
  semP6input$semattvsgram[semP6input$Condition%in% c("Passive Control","Active Control")] = -0.5
    semP6input$cp600layer = ifelse(semP6input$layer==p600layer,0.5,-0.5)

  # this is the main mixed model with centered variables
  randomeff = paste(" + (1 + cp600layer  |sub)") # Condition * layer
    if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~   cp600layer*cpassive + cp600layer*semattvsgram ",randomeff))
  omnimodel = lmer(formu, semP6input,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  modellist2 = modelComparison(omnimodel) # create anova table

  maincond = anova(modellist2[[3]],modellist2[[5]])
  condlayer = anova(modellist2[[1]],modellist2[[3]])
  
    # this model is for posthocs
  semP6input$semattvsgramf = factor(semP6input$semattvsgram,labels=c("ActPas","RoleRev"))
  formu2 = as.formula(paste(dependMeasure,"~  layer*semattvsgramf",randomeff))
  omnimodelFactor = lmer(formu2, semP6input)
  #print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ semattvsgramf | layer)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)
  difflexSemP6 =  abs(posthocs$`layer = word`$test$coefficients[[1]])
  avedifflex = difflex/5
  print(paste("Ratio of lexical sem P6 vs other P6",difflexSemP6/avedifflex))
  
}
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cp600layer * cpassive + cp600layer * semattvsgram +  
##     (1 + cp600layer | sub)
##    Data: semP6input
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 4249.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.1154 -0.1134 -0.0044  0.0453  6.0360 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr
##  sub      (Intercept) 0.1400   0.3742       
##           cp600layer  0.5585   0.7474   1.00
##  Residual             0.5998   0.7744       
## Number of obs: 1800, groups:  sub, 10
## 
## Fixed effects:
##                         Estimate Std. Error t value
## (Intercept)              1.30643    0.11974  10.911
## cp600layer               1.27596    0.23914   5.336
## cpassive                 0.06970    0.04471   1.559
## semattvsgram             2.52607    0.02581  97.855
## cp600layer:cpassive      0.07700    0.08942   0.861
## cp600layer:semattvsgram  2.45799    0.05163  47.609
## 
## Correlation of Fixed Effects:
##             (Intr) cp600l cpassv smttvs cp600lyr:c
## cp600layer  0.977                                 
## cpassive    0.000  0.000                          
## semattvsgrm 0.000  0.000  0.000                   
## cp600lyr:cp 0.000  0.000  0.000  0.000            
## cp600lyr:sm 0.000  0.000  0.000  0.000  0.000     
## [1] "remove . ~ . -  cp600layer:semattvsgram "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: semP6input
## Models:
## model2: abssum ~ cp600layer + cpassive + semattvsgram + (1 + cp600layer | 
## model2:     sub) + cp600layer:cpassive
## model: abssum ~ cp600layer * cpassive + cp600layer * semattvsgram + 
## model:     (1 + cp600layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  9 5711.6 5761.0 -2846.8   5693.6                             
## model  10 4246.3 4301.3 -2113.2   4226.3 1467.2      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cp600layer:semattvsgram  *** "
## [1] "remove . ~ . -  cp600layer:cpassive "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: semP6input
## Models:
## model2: abssum ~ cp600layer + cpassive + semattvsgram + (1 + cp600layer | 
## model2:     sub)
## model: abssum ~ cp600layer + cpassive + semattvsgram + (1 + cp600layer | 
## model:     sub) + cp600layer:cpassive
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2  8 5709.9 5753.8 -2846.9   5693.9                         
## model   9 5711.6 5761.0 -2846.8   5693.6 0.3276      1     0.5671
## [1] "########## Above comparison for  cp600layer:cpassive "
## [1] "remove . ~ . -  semattvsgram "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: semP6input
## Models:
## model2: abssum ~ cp600layer + cpassive + (1 + cp600layer | sub)
## model: abssum ~ cp600layer + cpassive + semattvsgram + (1 + cp600layer | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
## model2  7 7878.8 7917.3 -3932.4   7864.8                            
## model   8 5709.9 5753.8 -2846.9   5693.9  2171      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  semattvsgram  *** "
## [1] "remove . ~ . -  cpassive "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: semP6input
## Models:
## model2: abssum ~ cp600layer + (1 + cp600layer | sub)
## model: abssum ~ cp600layer + cpassive + (1 + cp600layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2  6 7877.1 7910.1 -3932.6   7865.1                         
## model   7 7878.8 7917.3 -3932.4   7864.8 0.3191      1     0.5721
## [1] "########## Above comparison for  cpassive "
## [1] "remove . ~ . -  cp600layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: semP6input
## Models:
## model2: abssum ~ (1 + cp600layer | sub)
## model: abssum ~ cp600layer + (1 + cp600layer | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  5 7888.8 7916.3 -3939.4   7878.8                             
## model   6 7877.1 7910.1 -3932.6   7865.1 13.698      1  0.0002147 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cp600layer  *** "
```

```
## refitting model(s) with ML (instead of REML)
## refitting model(s) with ML (instead of REML)
```

```
## Note: df set to 1778
```

```
## $`layer = word`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                       Estimate Std. Error t value Pr(>|t|)    
## ActPas - RoleRev == 0 -1.94561    0.05478  -35.52   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## $`layer = hidden`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                       Estimate Std. Error t value Pr(>|t|)    
## ActPas - RoleRev == 0 -5.63260    0.05478  -102.8   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## [1] "Ratio of lexical sem P6 vs other P6 4.93299727995744"
```

```r
  # figure for paper
semP6input[semP6input$Condition == "Passive Control",dependMeasure]=   0.1+semP6input[semP6input$Condition == "Passive Control",dependMeasure]
#   mapping = aes_string(x="depth", y=dependMeasure, colour = "Condition",linetype="Condition")
  p = drawERP(semP6input,dependMeasure,"Condition", timeline.y=6)
  p = p + scale_linetype_manual(values=c("dotted","dashed","solid")) #
  p
```

![plot of chunk unnamed-chunk-43](figure/unnamed-chunk-43-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/semP6.png",width=figwidth,height=figheight)
  }
#   mapping = aes_string(x="depth", y=dependMeasure, colour = "Condition",linetype="Condition")
#  drawERP(semP6inputverb,mapping, timeline.y=6)
  
n4P6cor = computeN4P6correlation(semP6input)
print(paste("N400/P600 correlation",n4P6cor))
```

```
## [1] "N400/P600 correlation -0.893059986660004"
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex

This is an example of the stimuli used in the model.


\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
Passive Control & the tea is sip -par by a friend . .\\
\hline
Active Control & the friend is sip -ing a tea . .\\
\hline
Role Reversal & the tea is sip -ing a friend . .\\
\hline
Passive Control & a wine is drink -par by the driver . .\\
\hline
Active Control & a driver is drink -ing the wine . .\\
\hline
Role Reversal & a wine is drink -ing the driver . .\\
\hline
Passive Control & the water is drink -par by the clerk . .\\
\hline
Active Control & the clerk is drink -ing the water . .\\
\hline
Role Reversal & the water is drink -ing the clerk . .\\
\hline
Passive Control & a tea is taste -par by the aunt . .\\
\hline
Active Control & a aunt is taste -ing the tea . .\\
\hline
Role Reversal & a tea is taste -ing the aunt . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + cp600layer  |sub)
main effect $\chi^2$(2)=2171.27, p$<$0.001
interaction $\chi^2$(2)=1467.58, p$<$0.001

Mixed Model Results
a main effect of cp600layer ,$\beta$=1.3, SE=0.24, $\chi^2$(1)=13.7, p$<$0.001 
no main effect of cpassive ,$\beta$=0.07, SE=0.045, $\chi^2$(1)=0.32, p=0.572 
a main effect of semattvsgram ,$\beta$=2.5, SE=0.026, $\chi^2$(1)=2170.95, p$<$0.001 
no interaction of cp600layer:cpassive ,$\beta$=0.077, SE=0.089, $\chi^2$(1)=0.33, p=0.567 
@@an interaction of cp600layer:semattvsgram ,$\beta$=2.5, SE=0.052, $\chi^2$(1)=1467.25, p$<$0.001 
 
Posthoc tests for  layer = word 
 @@Posthoc: There was a significant difference for ActPas - RoleRev, diff = 1.9456, t(1778)=35.52, p$<$0.001 
Posthoc tests for  layer = hidden 
 @@Posthoc: There was a significant difference for ActPas - RoleRev, diff = 5.6326, t(1778)=102.82, p$<$0.001 
ratio  ActPas - RoleRev   2.895  rev  0.345
```

## WORDCAT DEVELOP


```r
write("dev wordcat", stderr())
cat.df = subset(develop.df,str_detect(Condition,"(CATCONT|CATVIOL)") & ! measure %in% c("error", "myoutputder","myinputder","outderiv")) 

cat.df$Condition=factor(cat.df$Condition,labels=c("Control","Violation"))
verbpos = which(cat.df$wordcat == "VERBI")
wordcatsamelen = which(cat.df$wordcat!=cat.df$wordcat[1])[1]-1
verb.df = cat.df[verbpos,] #  verb
postverb.df = cat.df[verbpos + wordcatsamelen,] # position after verb
postverb.df=postverb.df[!is.na(postverb.df$Condition),]
postverb.df=postverb.df[!is.na(postverb.df$Condition),]

print(head(postverb.df[postverb.df$measure=="target",c("word","wordcat","Condition","Example","tick","epoch")])) # check
```

```
##       word wordcat Condition                                    Example
## 31942   -s      PL   Control   the grandma was take -ing the nap -s . .
## 32072  -ed      ED Violation  the grandma was take -ing the nap -ed . .
## 65989   -s      PL   Control           the girl take -ed the nap -s . .
## 66106  -ed      ED Violation          the girl take -ed the nap -ed . .
## 79067   -s      PL   Control     a teacher was take -ing the nap -s . .
## 79197  -ed      ED Violation    a teacher was take -ing the nap -ed . .
##       tick epoch
## 31942    8 10000
## 32072    8 10000
## 65989    7 10000
## 66106    7 10000
## 79067    8 10000
## 79197    8 10000
```

```r
postverb2.df = subset(postverb.df, layer == "word")
postverb2.df$X.s=abs(postverb2.df$X.s)
postverb2.df$X.ss=abs(postverb2.df$X.ss)
postverb2.df$per=abs(postverb2.df$per)
p1=showWordOutError("X.ed,X.s,per",c("Condition","epoch","measure"),postverb2.df)
p1=p1+theme_bw()+ylab("Activation/Error")
#p1 =p1 + scale_fill_discrete(name="Word Unit",labels=c("3rdSingular", "Plural", "End of sentence"))
p1 = p1+ theme(legend.position="bottom")
p1
```

![plot of chunk unnamed-chunk-45](figure/unnamed-chunk-45-1.png)

```r
#myggsave("img/devoutput.png",width=figwidth,height=figheight)
```



```r
corrHiddenOutputInputDeriv(postverb.df,"Condition","epoch")
```

```
## Condition
##   Control Violation 
##         8         8
```

![plot of chunk unnamed-chunk-46](figure/unnamed-chunk-46-1.png)


```r
epochnum = which(names(postverb.df)=="epoch")
postverb2.df = postverb.df[,c(epochnum,1:epochnum-1,(epochnum+1):length(postverb.df))]
mapping = aes(x=depth, y=value, colour = Condition,linetype=Condition) 
norawpostverbnotargout.df=removeWordsDrawERP(postverb.df,"value", "Condition", "epoch")
```

```
##   sub epoch word wordcat contfunc
## 1  s0 10000   -s      PL        F
## 2  s0 10000   -s      PL        F
## 3  s0 10000   -s      PL        F
## 4  s0 10000  -ed      ED        F
## 5  s0 10000  -ed      ED        F
## 6  s0 10000  -ed      ED        F
##                                      Example Condition tick    layer
## 1   the grandma was take -ing the nap -s . .   Control    8     word
## 2   the grandma was take -ing the nap -s . .   Control    8 compress
## 3   the grandma was take -ing the nap -s . .   Control    8   hidden
## 4  the grandma was take -ing the nap -ed . . Violation    8     word
## 5  the grandma was take -ing the nap -ed . . Violation    8 compress
## 6  the grandma was take -ing the nap -ed . . Violation    8   hidden
##      measure depth variable    value
## 1 inputderiv     1   abssum 1.813802
## 2 inputderiv     2   abssum 1.961948
## 3 inputderiv     3   abssum 2.436346
## 4 inputderiv     1   abssum 1.999954
## 5 inputderiv     2   abssum 3.906386
## 6 inputderiv     3   abssum 7.498786
```


```r
interactionText="NO TEXT"
modellist2 = list()

childtime = 30000
certainepochs.df = subset(norawpostverbnotargout.df, epoch %in% c(childtime,100000))
#certainepochs.df = norawpostverbnotargout.df

# get example for table
catwordexample = subset(certainepochs.df,measure == mainMeasure  & layer == "word")
exampleTable = catwordexample[1:4,c("Condition","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##        Condition                                    Example
## 118342   Control   the grandma was take -ing the nap -s . .
## 118472 Violation  the grandma was take -ing the nap -ed . .
## 152389   Control           the girl take -ed the nap -s . .
## 152506 Violation          the girl take -ed the nap -ed . .
```

```r
catinput  = subset(certainepochs.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj

  catinput$cviolation = ifelse(catinput$Condition=="Violation",0.5,-0.5) # one against two
  catinput$cword = ifelse(catinput$layer=="word",0.5,-0.5)
  catinput$cepoch = scale(catinput$epoch)
  catinput$sub = factor(catinput$sub)
  # this is the main mixed model with centered variables
  randomeff = paste(" + (1 + Condition |sub)")   # Condition * epoch 
  if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ cviolation*cepoch*cword",randomeff))
  omnimodel = lmer(formu, catinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
#  modellist2 = modelComparison(omnimodel) # create anova table

  catinputword = subset(catinput,layer == "word")
  catinputword$cviolation = ifelse(catinputword$Condition=="Violation",0.5,-0.5) # one against two
  catinputword$cword = ifelse(catinputword$layer=="word",0.5,-0.5)
  catinputword$cepoch = scale(catinputword$epoch)
  catinputword$sub = factor(catinputword$sub)
  # this is the main mixed model with centered variables
  randomeff = paste(" + (1 + Condition  |sub)")   # Condition * epoch 
  if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ cviolation*cepoch",randomeff))
  omnimodel = lmer(formu, catinputword,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  
  modellist2 = modelComparison(omnimodel) # create anova table
  
  # this model is for posthocs
  catinputword$depoch = factor(catinputword$epoch)
  formu2 = as.formula(paste(dependMeasure,"~ Condition*depoch",randomeff))
  omnimodelFactor = lmer(formu2, catinputword)
  print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ Condition | depoch)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)

}
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cviolation * cepoch * cword + (1 + Condition | sub)
##    Data: catinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 7582.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3427 -0.5346 -0.0764  0.4336  5.6361 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr 
##  sub      (Intercept)        0.1789   0.4229        
##           ConditionViolation 0.5741   0.7577   -0.03
##  Residual                    1.3295   1.1530        
## Number of obs: 2400, groups:  sub, 10
## 
## Fixed effects:
##                         Estimate Std. Error t value
## (Intercept)              2.22154    0.17806  12.477
## cviolation               1.55626    0.24419   6.373
## cepoch                  -0.15265    0.02354  -6.484
## cword                   -1.15875    0.04707 -24.616
## cviolation:cepoch       -0.32290    0.04708  -6.858
## cviolation:cword        -2.39400    0.09415 -25.429
## cepoch:cword             0.25189    0.04708   5.350
## cviolation:cepoch:cword  0.05251    0.09417   0.558
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cepoch cword cvltn:cp cvltn:cw cpch:c
## cviolation  0.635                                              
## cepoch      0.000  0.000                                       
## cword       0.000  0.000  0.000                                
## cviltn:cpch 0.000  0.000  0.000  0.000                         
## cviltn:cwrd 0.000  0.000  0.000  0.000 0.000                   
## cepoch:cwrd 0.000  0.000  0.000  0.000 0.000    0.000          
## cvltn:cpch: 0.000  0.000  0.000  0.000 0.000    0.000    0.000 
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ cviolation * cepoch + (1 + Condition | sub)
##    Data: catinputword
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 779.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0293 -0.5367  0.0738  0.5620  2.7875 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr 
##  sub      (Intercept)        0.04928  0.2220        
##           ConditionViolation 0.09064  0.3011   -0.20
##  Residual                    0.10408  0.3226        
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)        1.642167   0.077183  21.276
## cviolation         0.359260   0.097011   3.703
## cepoch            -0.026705   0.009317  -2.866
## cviolation:cepoch -0.296705   0.018634 -15.923
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cepoch
## cviolation  0.429               
## cepoch      0.000  0.000        
## cviltn:cpch 0.000  0.000  0.000 
## [1] "remove . ~ . -  cviolation:cepoch "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: catinputword
## Models:
## model2: abssum ~ cviolation + cepoch + (1 + Condition | sub)
## model: abssum ~ cviolation * cepoch + (1 + Condition | sub)
##        Df    AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  7 1003.5 1039.15 -494.76   989.52                             
## model   8  775.5  816.22 -379.75   759.50 230.02      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation:cepoch  *** "
## [1] "remove . ~ . -  cepoch "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: catinputword
## Models:
## model2: abssum ~ cviolation + (1 + Condition | sub)
## model: abssum ~ cviolation + cepoch + (1 + Condition | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2  6 1008.3 1038.8 -498.14   996.27                            
## model   7 1003.5 1039.2 -494.76   989.52 6.7525      1   0.009361 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cepoch  *** "
## [1] "remove . ~ . -  cviolation "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: catinputword
## Models:
## model2: abssum ~ (1 + Condition | sub)
## model: abssum ~ cviolation + (1 + Condition | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
## model2  5 1015.5 1041.0 -502.76  1005.53                            
## model   6 1008.3 1038.8 -498.14   996.27 9.2577      1   0.002345 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation  *** "
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ Condition * depoch + (1 + Condition | sub)
##    Data: catinputword
## 
## REML criterion at convergence: 776.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0293 -0.5367  0.0738  0.5620  2.7875 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr 
##  sub      (Intercept)        0.04928  0.2220        
##           ConditionViolation 0.09064  0.3011   -0.20
##  Residual                    0.10408  0.3226        
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                                 Estimate Std. Error t value
## (Intercept)                      1.34094    0.07263  18.464
## ConditionViolation               0.65584    0.09878   6.639
## depoch100000                     0.24319    0.02634   9.232
## ConditionViolation:depoch100000 -0.59316    0.03725 -15.923
## 
## Correlation of Fixed Effects:
##             (Intr) CndtnV d10000
## ConditnVltn -0.233              
## depch100000 -0.181  0.133       
## CndV:100000  0.128 -0.189 -0.707
```

```
## Note: df set to 9
```

```
## $`depoch = 30000`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)    
## Control - Violation == 0 -0.65584    0.09878  -6.639 9.49e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## $`depoch = 100000`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                          Estimate Std. Error t value Pr(>|t|)
## Control - Violation == 0 -0.06268    0.09878  -0.634    0.542
## (Adjusted p values reported -- single-step method)
```

```r
form = as.formula(paste(dependMeasure," ~ Condition + epoch + layer",sep=""))
meandf = aggregate(form, catinput, mean)
mapping = aes_string(x="epoch", y=dependMeasure, fill = "Condition")
meandf$epoch= factor(meandf$epoch)
p=ggplot(meandf, mapping)
p=p+geom_bar(position="dodge",stat="identity")
p = p + scale_fill_brewer(palette = "Set1")
p=p+facet_wrap(~ layer)
p=p + theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))
p
```

![plot of chunk unnamed-chunk-48](figure/unnamed-chunk-48-1.png)

```r
catinputword = subset(catinput, layer == "word")

form = as.formula(paste(dependMeasure," ~ Condition + epoch ",sep=""))
meandf = aggregate(form, catinputword, mean)
mapping = aes_string(x="epoch", y=dependMeasure, fill = "Condition")
meandf$epoch= factor(meandf$epoch,labels = c(paste(childtime,"(child)",sep=""),"100000(adult)"))
p=ggplot(meandf, mapping)
p=p+geom_bar(position="dodge",stat="identity")+theme_bw()
#p = p + scale_fill_brewer(palette = "Set1")  
p = p + scale_fill_grey(start = 0.25, end = 0.75)
p = p +ylab("Sum Abs. Error")+xlab("Testing Epoch")
p=p + theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))
p
```

![plot of chunk unnamed-chunk-48](figure/unnamed-chunk-48-2.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE??n
    myggsave("img/catdevelop.png",width=figwidth,height=figheight)
  }
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex

This is an example of the stimuli used in the model.


\begin{tabular}{l|l}
\hline
Condition & Example\\
\hline
Control & the grandma was take -ing the nap -s . .\\
\hline
Violation & the grandma was take -ing the nap -ed . .\\
\hline
Control & the girl take -ed the nap -s . .\\
\hline
Violation & the girl take -ed the nap -ed . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + Condition  |sub)

Mixed Model Results
a main effect of cviolation ,$\beta$=0.36, SE=0.097, $\chi^2$(1)=9.26, p=0.0023 
a main effect of cepoch ,$\beta$=-0.027, SE=0.0093, $\chi^2$(1)=6.75, p=0.0094 
@@an interaction of cviolation:cepoch ,$\beta$=-0.3, SE=0.019, $\chi^2$(1)=230.02, p$<$0.001 
 
Posthoc tests for  depoch = 30000 
 @@Posthoc: There was a significant difference for Control - Violation, diff = 0.6558, t(9)=6.64, p$<$0.001 
Posthoc tests for  depoch = 100000 
 @@Posthoc: There was no difference for Control - Violation, p=0.5415 
ratio  Control - Violation   0.096  rev  10.459
```


## Coulson et al


```r
# This is Coulson et al 
write("Coulson et al", stderr())
coulagree.df = subset(couldf,  str_detect(Condition,"(SING|PLUR)") )
verbpos = which(coulagree.df$wordcat == "VERBT")
wordcatsamelen = which(coulagree.df$wordcat!=coulagree.df$wordcat[1])[1]-1
verb.df = coulagree.df[verbpos,] #  verb
postverbcoul.df = coulagree.df[verbpos + wordcatsamelen,] # position after verb
postverbcoul.df=postverbcoul.df[!is.na(postverbcoul.df$Condition),]

postverbcoul.df = cbind(Probability="",Number="",Agreement="",postverbcoul.df)
cond = str_split_fixed(postverbcoul.df$Condition,";",2)
postverbcoul.df$Number = cond[,1]
postverbcoul.df$Agreement = cond[,2]
postverbcoul.df$Number = factor(postverbcoul.df$Number,labels=c("Singular"))
postverbcoul.df$Agreement = factor(postverbcoul.df$Agreement,labels=c("Control","Violation"))
postverbcoul.df$Probability=NA
postverbcoul.df$Probability[postverbcoul.df$Agreement=="Violation" & str_detect(postverbcoul.df$coul,"vcoul1")] = "Improbable"
postverbcoul.df$Probability[postverbcoul.df$Agreement=="Violation" & str_detect(postverbcoul.df$coul,"vcoul2")] = "Probable"
postverbcoul.df$Probability[postverbcoul.df$Agreement=="Control" & str_detect(postverbcoul.df$coul,"vcoul2")] = "Improbable"
postverbcoul.df$Probability[postverbcoul.df$Agreement=="Control" & str_detect(postverbcoul.df$coul,"vcoul1")] = "Probable"
postverbcoul.df$Probability=factor(postverbcoul.df$Probability)

xtabs(~ Agreement +Probability,postverbcoul.df)
```

```
##            Probability
## Agreement   Improbable Probable
##   Control          600     2400
##   Violation        600     2400
```

```r
print(head(postverbcoul.df[postverbcoul.df$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##      word wordcat Condition                               Example tick
## 157   -ss      SS SING;CONT          the boy take -ss a stick . .    4
## 391  wine   NOUNI PLUR;CONT       the father -s taste wine -s . .    5
## 612     a     DET SING;VIOL            the mother push a kite . .    4
## 846   the     DET PLUR;CONT          the wife -s take the toy . .    5
## 1054  -ss      SS SING;CONT  the brother taste -ss the tea -s . .    4
## 1301  the     DET PLUR;CONT        the clerk -s drink the tea . .    5
```

```r
showWordOutError("the,a,X.ss,X.ed",c("Agreement","measure","coul"),postverbcoul.df)
```

![plot of chunk unnamed-chunk-50](figure/unnamed-chunk-50-1.png)



```r
corrHiddenOutputInputDeriv(postverbcoul.df,"Probability", "Agreement")
```

```
## Condition
##               ACTCONT               CATCONT               CATVIOL 
##                     0                     0                     0 
##                  CONG               GARDAMB                GARDSC 
##                     0                     0                     0 
##             HIGHCLOZE                 INCOH              LOWCLOZE 
##                     0                     0                     0 
##              PASSCONT             PLUR;CONT             PLUR;VIOL 
##                     0                    14                     2 
##               SEMATTR             SING;CONT             SING;VIOL 
##                     0                    12                     4 
##               STRONG    STRONGCONS;EXPECTED STRONGCONS;UNEXPECTED 
##                     0                     0                     0 
##               SUBCONT               SUBVIOL             TENSECONT 
##                     0                     0                     0 
##             TENSEVIOL             TEST-PRED           TEST-UNPRED 
##                     0                     0                     0 
##                  WEAK                 WEAK      WEAKCONS;EXPECTED 
##                     0                     0                     0 
##   WEAKCONS;UNEXPECTED             ́ZEROCLOZE 
##                     0                     0
```

![plot of chunk unnamed-chunk-51](figure/unnamed-chunk-51-1.png)



```r
# make erp figure with different measures
#mapping = aes(x=depth, y=value, colour = Agreement,linetype=Probability) 
coulnorawpostverbnotargout.df=removeWordsDrawERP(postverbcoul.df,"value","Probability","Agreement")
```

```
##   Probability    Number Agreement sub      coul devellearn  epoch word
## 1    Probable Singular2   Control  s0 0.1vcoul1       <NA> 100000  -ss
## 2    Probable Singular2   Control  s0 0.1vcoul1       <NA> 100000  -ss
## 3    Probable Singular2   Control  s0 0.1vcoul1       <NA> 100000  -ss
## 4    Probable Singular2   Control  s0 0.1vcoul1       <NA> 100000  -ss
## 5    Probable Singular2   Control  s0 0.1vcoul1       <NA> 100000  -ss
## 6    Probable Singular2   Control  s0 0.1vcoul1       <NA> 100000  -ss
##   wordcat contfunc                       Example Condition tick    layer
## 1      SS        F  the boy take -ss a stick . . SING;CONT    4     word
## 2      SS        F  the boy take -ss a stick . . SING;CONT    4     word
## 3      SS        F  the boy take -ss a stick . . SING;CONT    4 compress
## 4      SS        F  the boy take -ss a stick . . SING;CONT    4 compress
## 5      SS        F  the boy take -ss a stick . . SING;CONT    4   hidden
## 6      SS        F  the boy take -ss a stick . . SING;CONT    4   hidden
##      measure depth variable      value
## 1   outderiv     1   abssum  2.4385430
## 2 inputderiv     1   abssum  1.1798382
## 3   outderiv     2   abssum 26.3264316
## 4 inputderiv     2   abssum  0.1031967
## 5   outderiv     3   abssum  2.8054352
## 6 inputderiv     3   abssum  0.1231286
```



```r
interactionText="NO TEXT"
modellist2 = list()

# get example for table
agreeinputword = subset(coulnorawpostverbnotargout.df,measure == mainMeasure  & layer == "word")
exampleTable = agreeinputword[1:4,c("Number","Agreement","Example")]
tabletext = kable(exampleTable,format="latex",row.names=FALSE)
print(exampleTable)
```

```
##        Number Agreement                          Example
## 159 Singular2   Control     the boy take -ss a stick . .
## 393 Singular1   Control  the father -s taste wine -s . .
## 614 Singular2 Violation       the mother push a kite . .
## 848 Singular1   Control     the wife -s take the toy . .
```

```r
coulagreeinput  = subset(coulnorawpostverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )

if (nlevels(adultdata.df$sub) > 4){ # do mixed with more than 9 subj

  coulagreeinput$cplural = ifelse(coulagreeinput$Number=="Plural",0.5,-0.5)
  coulagreeinput$cviolation = ifelse(coulagreeinput$Agreement=="Violation",0.5,-0.5)
  coulagreeinput$cp6layer = ifelse(coulagreeinput$layer==p600layer,0.5,-0.5)
  coulagreeinput$cprob = ifelse(coulagreeinput$Probability=="Probable",0.5,-0.5)

  #- cviolation:cp6layer:cprob  -cviolation:cp6layer - cviolation:cprob - cp6layer:cprob - cp6layer
  # this is the main mixed model with centered variables
  #cviolation*cp6layer*cprob -cviolation:cp6layer:cprob -cviolation:cp6layer -cviolation:cprob - cp6layer:cprob - cp6layer -cviolation -cprob
    randomeff = paste(" + (1 + cviolation*cp6layer*cprob - cviolation:cp6layer:cprob |sub)")  #cviolation*cp6layer*cprob
      if (noslopes){ randomeff = " + (1 | sub)"}
  formu = as.formula(paste(dependMeasure,"~ cviolation*cp6layer*cprob",randomeff))
  omnimodel = lmer(formu, coulagreeinput,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
  print(summary(omnimodel))
  
  modellist2 = modelComparison(omnimodel) # create anova table
  # this model is for posthocs
  formu2 = as.formula(paste(dependMeasure,"~ Probability*layer",randomeff))
  omnimodelFactor = lmer(formu2, coulagreeinput)
#  print(summary(omnimodelFactor))
  model.lsmobj <- lsmeans(omnimodelFactor, ~ Probability | layer)
  posthocs = summary(as.glht(pairs(model.lsmobj)))
  print(posthocs)
  interactionText = printInteraction(posthocs)

}
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## abssum ~ cviolation * cp6layer * cprob + (1 + cviolation * cp6layer *  
##     cprob - cviolation:cp6layer:cprob | sub)
##    Data: coulagreeinput
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 4161.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6801 -0.4342 -0.0431  0.1782  7.1270 
## 
## Random effects:
##  Groups   Name                Variance Std.Dev. Corr                   
##  sub      (Intercept)         0.04227  0.2056                          
##           cviolation          0.02921  0.1709   -0.77                  
##           cp6layer            0.11735  0.3426    1.00 -0.74            
##           cprob               0.08865  0.2977   -0.84  0.80 -0.84      
##           cviolation:cp6layer 0.06507  0.2551   -0.12  0.73 -0.08  0.38
##           cviolation:cprob    0.17542  0.4188    0.59 -0.28  0.57 -0.06
##           cp6layer:cprob      0.28769  0.5364   -0.89  0.81 -0.89  1.00
##  Residual                     1.82580  1.3512                          
##             
##             
##             
##             
##             
##             
##   0.26      
##   0.35 -0.15
##             
## Number of obs: 1200, groups:  sub, 10
## 
## Fixed effects:
##                           Estimate Std. Error t value
## (Intercept)                2.02474    0.08127  24.914
## cviolation                 0.97278    0.11149   8.725
## cp6layer                   0.80395    0.14576   5.516
## cprob                     -0.84013    0.13555  -6.198
## cviolation:cp6layer        0.84837    0.21105   4.020
## cviolation:cprob           0.19242    0.23575   0.816
## cp6layer:cprob            -1.19948    0.25847  -4.641
## cviolation:cp6layer:cprob  0.14818    0.39006   0.380
## 
## Correlation of Fixed Effects:
##             (Intr) cviltn cp6lyr cprob  cvlt:6 cvltn: cp6ly:
## cviolation  -0.298                                          
## cp6layer     0.594 -0.267                                   
## cprob       -0.725  0.268 -0.436                            
## cvltn:cp6ly -0.038  0.135 -0.024  0.101                     
## cviltn:cprb  0.265 -0.511  0.239 -0.022  0.056              
## cp6lyr:cprb -0.465  0.259 -0.737  0.454  0.088 -0.055       
## cvltn:cp6l:  0.000  0.000  0.000  0.000 -0.554  0.000  0.000
## [1] "remove . ~ . -  cviolation:cp6layer:cprob "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: coulagreeinput
## Models:
## model2: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model2:     cprob - cviolation:cp6layer:cprob | sub) + cviolation:cp6layer + 
## model2:     cviolation:cprob + cp6layer:cprob
## model: abssum ~ cviolation * cp6layer * cprob + (1 + cviolation * cp6layer * 
## model:     cprob - cviolation:cp6layer:cprob | sub)
##        Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
## model2 36 4218.0 4401.2 -2073.0   4146.0                        
## model  37 4219.5 4407.8 -2072.7   4145.5 0.519      1     0.4713
## [1] "########## Above comparison for  cviolation:cp6layer:cprob "
## [1] "remove . ~ . -  cp6layer:cprob "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: coulagreeinput
## Models:
## model2: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model2:     cprob - cviolation:cp6layer:cprob | sub) + cviolation:cp6layer + 
## model2:     cviolation:cprob
## model: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model:     cprob - cviolation:cp6layer:cprob | sub) + cviolation:cp6layer + 
## model:     cviolation:cprob + cp6layer:cprob
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 35 4228.3 4406.4 -2079.1   4158.3                             
## model  36 4218.0 4401.2 -2073.0   4146.0 12.286      1  0.0004564 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cp6layer:cprob  *** "
## [1] "remove . ~ . -  cviolation:cprob "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: coulagreeinput
## Models:
## model2: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model2:     cprob - cviolation:cp6layer:cprob | sub) + cviolation:cp6layer
## model: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model:     cprob - cviolation:cp6layer:cprob | sub) + cviolation:cp6layer + 
## model:     cviolation:cprob
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2 34 4226.4 4399.4 -2079.2   4158.4                         
## model  35 4228.3 4406.4 -2079.1   4158.3 0.0997      1     0.7522
## [1] "########## Above comparison for  cviolation:cprob "
## [1] "remove . ~ . -  cviolation:cp6layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: coulagreeinput
## Models:
## model2: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model2:     cprob - cviolation:cp6layer:cprob | sub)
## model: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model:     cprob - cviolation:cp6layer:cprob | sub) + cviolation:cp6layer
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 33 4245.5 4413.4 -2089.7   4179.5                             
## model  34 4226.4 4399.4 -2079.2   4158.4 21.105      1  4.349e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation:cp6layer  *** "
## [1] "remove . ~ . -  cprob "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: coulagreeinput
## Models:
## model2: abssum ~ cviolation + cp6layer + (1 + cviolation * cp6layer * 
## model2:     cprob - cviolation:cp6layer:cprob | sub)
## model: abssum ~ cviolation + cp6layer + cprob + (1 + cviolation * cp6layer * 
## model:     cprob - cviolation:cp6layer:cprob | sub)
##        Df    AIC    BIC  logLik deviance Chisq Chi Df Pr(>Chisq)
## model2 32 4232.8 4395.7 -2084.4   4168.8                        
## model  33 4245.5 4413.4 -2089.7   4179.5     0      1          1
## [1] "########## Above comparison for  cprob "
## [1] "remove . ~ . -  cp6layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: coulagreeinput
## Models:
## model2: abssum ~ cviolation + (1 + cviolation * cp6layer * cprob - cviolation:cp6layer:cprob | 
## model2:     sub)
## model: abssum ~ cviolation + cp6layer + (1 + cviolation * cp6layer * 
## model:     cprob - cviolation:cp6layer:cprob | sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2 31 4231.0 4388.8 -2084.5   4169.0                         
## model  32 4232.8 4395.7 -2084.4   4168.8 0.2312      1     0.6307
## [1] "########## Above comparison for  cp6layer "
## [1] "remove . ~ . -  cviolation "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: coulagreeinput
## Models:
## model2: abssum ~ (1 + cviolation * cp6layer * cprob - cviolation:cp6layer:cprob | 
## model2:     sub)
## model: abssum ~ cviolation + (1 + cviolation * cp6layer * cprob - cviolation:cp6layer:cprob | 
## model:     sub)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2 30 4240.3 4393.0 -2090.1   4180.3                             
## model  31 4231.0 4388.8 -2084.5   4169.0 11.254      1  0.0007947 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cviolation  *** "
```

```
## Note: df set to 9
```

```
## $`layer = word`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                            Estimate Std. Error t value Pr(>|t|)  
## Improbable - Probable == 0   0.3323     0.1784   1.863   0.0954 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
## 
## 
## $`layer = hidden`
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Linear Hypotheses:
##                            Estimate Std. Error t value Pr(>|t|)    
## Improbable - Probable == 0   2.4676     0.2674   9.228 6.95e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

```r
xtabs(~ coul + Agreement, coulagreeinput)
```

```
##            Agreement
## coul        Control Violation
##   0.1vcoul1     480       120
##   0.1vcoul2     120       480
```

```r
xtabs(~ Probability + Agreement, coulagreeinput)
```

```
##             Agreement
## Probability  Control Violation
##   Improbable     120       120
##   Probable       480       480
```

```r
# mapping = aes_string(x="depth", y=dependMeasure, colour = "Probability",linetype="Probability")
      p = drawERP(coulagreeinput,dependMeasure,"Probability",iv4="Agreement", span=1, timeline.y=4,timesize=2,xmax=7)
 #     p = p + facet_wrap(~ Agreement)
      p
```

![plot of chunk unnamed-chunk-53](figure/unnamed-chunk-53-1.png)

```r
  if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/agreelearn.png",width=figwidth,height=figheight)
  }
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex

This is an example of the stimuli used in the model.


\begin{tabular}{l|l|l}
\hline
Number & Agreement & Example\\
\hline
Singular2 & Control & the boy take -ss a stick . .\\
\hline
Singular1 & Control & the father -s taste wine -s . .\\
\hline
Singular2 & Violation & the mother push a kite . .\\
\hline
Singular1 & Control & the wife -s take the toy . .\\
\hline
\end{tabular}
The maximal model for the data had random slopes  + (1 + cviolation*cp6layer*cprob - cviolation:cp6layer:cprob |sub)

Mixed Model Results
a main effect of cviolation ,$\beta$=0.97, SE=0.11, $\chi^2$(1)=11.25, p$<$0.001 
no main effect of cp6layer ,$\beta$=0.8, SE=0.15, $\chi^2$(1)=0.23, p=0.631 
no main effect of cprob ,$\beta$=-0.84, SE=0.14, $\chi^2$(1)=0, p=1.000 
an interaction of cviolation:cp6layer ,$\beta$=0.85, SE=0.21, $\chi^2$(1)=21.1, p$<$0.001 
no interaction of cviolation:cprob ,$\beta$=0.19, SE=0.24, $\chi^2$(1)=0.1, p=0.752 
an interaction of cp6layer:cprob ,$\beta$=-1.2, SE=0.26, $\chi^2$(1)=12.29, p$<$0.001 
@@no interaction of cviolation:cp6layer:cprob ,$\beta$=0.15, SE=0.39, $\chi^2$(1)=0.52, p=0.471 
 
Posthoc tests for  layer = word 
 @@Posthoc: There was no difference for Improbable - Probable, p=0.0954 
Posthoc tests for  layer = hidden 
 @@Posthoc: There was a significant difference for Improbable - Probable, diff = 2.4676, t(9)=9.23, p$<$0.001 
ratio  Improbable - Probable   7.426  rev  0.135
```


## Rommer Federmeier


```r
# This is Coulson et al 
write("Rommer Federmeier", stderr())
romFinalN = subset(rommerdf, wordcat %in% c('NOUNI'))
showWordOutError("coffee,tea,wine,beer,water,cake",c("Condition","measure"),romFinalN)
```

![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-55-1.png)

```r
romFinalN$vrom=NULL
romFinalN$Condition = str_replace(romFinalN$Condition," +","")
romFinalNtest= subset(romFinalN, Condition %in% c("TEST-UNPRED","TEST-PRED"))

print(head(romFinalNtest[romFinalNtest$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##        word wordcat   Condition                          Example tick
## 19657 water   NOUNI   TEST-PRED   he is taste -ing the water . .    6
## 19852 water   NOUNI TEST-UNPRED   he is taste -ing the water . .    6
## 20047 water   NOUNI   TEST-PRED  the uncle taste -ss a water . .    6
## 20242 water   NOUNI TEST-UNPRED  the uncle taste -ss a water . .    6
## 20437 water   NOUNI   TEST-PRED      she taste -ed the water . .    5
## 20632 water   NOUNI TEST-UNPRED      she taste -ed the water . .    5
```

```r
# check that layers are the right size

showWordOutError("coffee,tea,wine,beer,water,cake",c("Condition","measure"),romFinalNtest)
```

![plot of chunk unnamed-chunk-55](figure/unnamed-chunk-55-2.png)


```r
# make erp figure with different measures
#mapping = aes(x=depth, y=value, colour = Condition,linetype=Condition)
romrawpostverbnotargout.df=removeWordsDrawERP(romFinalNtest,"value","Condition","measure","variable",showFig=TRUE)
```

```
##   sub devellearn  epoch  word wordcat contfunc
## 1  s0       <NA> 100000 water   NOUNI        C
## 2  s0       <NA> 100000 water   NOUNI        C
## 3  s0       <NA> 100000 water   NOUNI        C
## 4  s0       <NA> 100000 water   NOUNI        C
## 5  s0       <NA> 100000 water   NOUNI        C
## 6  s0       <NA> 100000 water   NOUNI        C
##                           Example Condition tick    layer    measure depth
## 1  he is taste -ing the water . . TEST-PRED    6     word   outderiv     1
## 2  he is taste -ing the water . . TEST-PRED    6     word inputderiv     1
## 3  he is taste -ing the water . . TEST-PRED    6 compress   outderiv     2
## 4  he is taste -ing the water . . TEST-PRED    6 compress inputderiv     2
## 5  he is taste -ing the water . . TEST-PRED    6   hidden   outderiv     3
## 6  he is taste -ing the water . . TEST-PRED    6   hidden inputderiv     3
##   variable      value
## 1   abssum  4.5751810
## 2   abssum  1.5628583
## 3   abssum 10.0684366
## 4   abssum  0.3121806
## 5   abssum  4.4232054
## 6   abssum  0.3329172
```

![plot of chunk unnamed-chunk-56](figure/unnamed-chunk-56-1.png)

```r
rominput  = subset(romrawpostverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )
rominput$Prime = factor(rominput$Condition,labels=c("Predictable","Unpredictable"))

romNotFinalN = subset(rommerNotdf, wordcat %in% c('NOUNI'))
showWordOutError("coffee,tea,wine,beer,water,cake",c("Condition","measure"),romNotFinalN)
```

![plot of chunk unnamed-chunk-56](figure/unnamed-chunk-56-2.png)

```r
romNotFinalN$vromNot=NULL
romNotFinalN$Condition = str_replace(romNotFinalN$Condition," +","")
romNotFinalNtest= subset(romNotFinalN, Condition %in% c("TEST-UNPRED","TEST-PRED"))

print(head(romNotFinalNtest[romNotFinalNtest$measure=="target",c("word","wordcat","Condition","Example","tick")])) # check
```

```
##        word wordcat   Condition                          Example tick
## 14015 water   NOUNI   TEST-PRED   he is taste -ing the water . .    6
## 14184 water   NOUNI TEST-UNPRED   he is taste -ing the water . .    6
## 14379 water   NOUNI   TEST-PRED  the uncle taste -ss a water . .    6
## 14561 water   NOUNI TEST-UNPRED  the uncle taste -ss a water . .    6
## 14756 water   NOUNI   TEST-PRED      she taste -ed the water . .    5
## 14912 water   NOUNI TEST-UNPRED      she taste -ed the water . .    5
```

```r
# check that layers are the right size

showWordOutError("coffee,tea,wine,beer,water,cake",c("Condition","measure"),romNotFinalNtest)
```

![plot of chunk unnamed-chunk-56](figure/unnamed-chunk-56-3.png)


```r
romNotrawpostverbnotargout.df=removeWordsDrawERP(romNotFinalNtest,"value","Condition","measure","variable",showFig=TRUE)
```

```
##   sub devellearn  epoch  word wordcat contfunc
## 1  s0       <NA> 100000 water   NOUNI        C
## 2  s0       <NA> 100000 water   NOUNI        C
## 3  s0       <NA> 100000 water   NOUNI        C
## 4  s0       <NA> 100000 water   NOUNI        C
## 5  s0       <NA> 100000 water   NOUNI        C
## 6  s0       <NA> 100000 water   NOUNI        C
##                           Example Condition tick    layer    measure depth
## 1  he is taste -ing the water . . TEST-PRED    6     word   outderiv     1
## 2  he is taste -ing the water . . TEST-PRED    6     word inputderiv     1
## 3  he is taste -ing the water . . TEST-PRED    6 compress   outderiv     2
## 4  he is taste -ing the water . . TEST-PRED    6 compress inputderiv     2
## 5  he is taste -ing the water . . TEST-PRED    6   hidden   outderiv     3
## 6  he is taste -ing the water . . TEST-PRED    6   hidden inputderiv     3
##   variable      value
## 1   abssum  4.5751810
## 2   abssum  1.5628583
## 3   abssum 10.0684366
## 4   abssum  0.3121806
## 5   abssum  4.4232054
## 6   abssum  0.3329172
```

![plot of chunk unnamed-chunk-57](figure/unnamed-chunk-57-1.png)



```r
interactionText="NO TEXT"
modellist2 = list()

romNotinput  = subset(romNotrawpostverbnotargout.df,measure == mainMeasure & layer %in% c("word",p600layer) )
romNotinput$Prime = factor(romNotinput$Condition,labels=c("Predictable","Unpredictable"))

rominput$exp = "repetition"
romNotinput$exp = "norepetition"
rominput2= rbind(rominput,romNotinput) 
aggregate(abssum ~ Condition + exp + layer, rominput2,mean)
```

```
##     Condition          exp  layer    abssum
## 1   TEST-PRED norepetition   word 1.5131503
## 2 TEST-UNPRED norepetition   word 1.6354943
## 3   TEST-PRED   repetition   word 1.5131503
## 4 TEST-UNPRED   repetition   word 1.3608776
## 5   TEST-PRED norepetition hidden 0.7347092
## 6 TEST-UNPRED norepetition hidden 0.6662879
## 7   TEST-PRED   repetition hidden 0.7347092
## 8 TEST-UNPRED   repetition hidden 0.6168264
```

```r
rominput4= subset(rominput2,!(exp == "norepetition" & Condition == "TEST-PRED"))
xtabs(~ Condition + exp, rominput4)
```

```
##              exp
## Condition     norepetition repetition
##   TEST-PRED              0        300
##   TEST-UNPRED          300        300
```

```r
rominput4$Condition2=as.character(rominput4$Condition)
rominput4$Condition2[rominput4$exp == "norepetition"]="Not Previously Seen"
rominput4$Condition2 = factor(rominput4$Condition2,labels=c("Not Previously Seen","Previously Predictable","Previously Unpredictable"))
rominput4$cp6layer = ifelse(rominput4$layer==p600layer,0.5,-0.5)

romexamdf = subset(rommerdf,measure == mainMeasure  & layer == "word" & tick == 1,c(1:12))
exampleTable = romexamdf[c(1,3,4),c("Condition","Example")]
exampleTable$Condition = factor(exampleTable$Condition,labels =c("Predictable Prime","Target","Unpredictable Prime"))
romNotexamdf = subset(rommerNotdf,measure == mainMeasure  & layer == "word" & tick == 1,c(1:12))
exampleTable2 = romNotexamdf[c(1,3,4),c("Condition","Example")]
exampleTable2$Condition = factor(exampleTable$Condition,labels =c("Predictable Prime","Target","Unpredictable Prime"))

exampleTable3 = rbind(exampleTable,exampleTable2)
exampleTable3 = exampleTable3[c(1,2,5,6),]
exampleTable3$Condition = c("Previously Predictable","Previously Unpredictable","Not Previously Seen","Critical Target")
exampleTable3[,c(2,1)]
```

```
##                               Example                Condition
## 19503     she drink -ss the water . .   Previously Predictable
## 19698     she sniff -ss the water . . Previously Unpredictable
## 14056         a grandma will jump . .      Not Previously Seen
## 14121  he is taste -ing the water . .          Critical Target
```

```r
tabletext3 = kable(exampleTable3[,c(2,1)],format="latex",booktab=T,row.names=FALSE)
print(exampleTable3)
```

```
##                      Condition                         Example
## 19503   Previously Predictable     she drink -ss the water . .
## 19698 Previously Unpredictable     she sniff -ss the water . .
## 14056      Not Previously Seen         a grandma will jump . .
## 14121          Critical Target  he is taste -ing the water . .
```

```r
randomeff = paste(" + (1 +layer |sub)") 
rominput4$seen =ifelse(rominput4$Condition2=="Not Previously Seen",1,-0.5)
rominput4$pred = 0
rominput4$pred[rominput4$Condition2=="Previously Predictable"]=0.5
rominput4$pred[rominput4$Condition2=="Previously Unpredictable"]=-0.5
contrasts(rominput4$Condition2)<-contr.helmert(3)[c(3,2,1),]

if (noslopes){ randomeff = " + (1 | sub)"}
formu = as.formula(paste(dependMeasure,"~ seen*cp6layer +  pred*cp6layer",randomeff))
omnimodel = lmer(formu, rominput4,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
print(summary(omnimodel))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ seen * cp6layer + pred * cp6layer + (1 + layer | sub)
##    Data: rominput4
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: -469.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8464 -0.4697 -0.0111  0.3998  5.3280 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  sub      (Intercept) 0.01534  0.1239        
##           layerhidden 0.06867  0.2621   -0.51
##  Residual             0.03099  0.1760        
## Number of obs: 900, groups:  sub, 10
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)    1.087891   0.040211  27.055
## seen           0.063000   0.008299   7.592
## cp6layer      -0.830566   0.083695  -9.924
## pred           0.135078   0.014374   9.397
## seen:cp6layer -0.138640   0.016598  -8.353
## cp6layer:pred -0.034390   0.028748  -1.196
## 
## Correlation of Fixed Effects:
##             (Intr) seen  cp6lyr pred  sn:cp6
## seen        0.000                           
## cp6layer    0.525  0.000                    
## pred        0.000  0.000 0.000              
## seen:cp6lyr 0.000  0.000 0.000  0.000       
## cp6layr:prd 0.000  0.000 0.000  0.000 0.000
```

```r
modellist2 = modelComparison(omnimodel) # create anova table
```

```
## [1] "remove . ~ . -  cp6layer:pred "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4
## Models:
## model2: abssum ~ seen + cp6layer + pred + (1 + layer | sub) + seen:cp6layer
## model: abssum ~ seen * cp6layer + pred * cp6layer + (1 + layer | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2  9 -484.67 -441.45 251.34  -502.67                         
## model  10 -484.11 -436.08 252.05  -504.11 1.4364      1     0.2307
## [1] "########## Above comparison for  cp6layer:pred "
## [1] "remove . ~ . -  seen:cp6layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4
## Models:
## model2: abssum ~ seen + cp6layer + pred + (1 + layer | sub)
## model: abssum ~ seen + cp6layer + pred + (1 + layer | sub) + seen:cp6layer
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  8 -419.34 -380.92 217.67  -435.34                             
## model   9 -484.67 -441.45 251.34  -502.67 67.335      1  2.291e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  seen:cp6layer  *** "
## [1] "remove . ~ . -  pred "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4
## Models:
## model2: abssum ~ seen + cp6layer + (1 + layer | sub)
## model: abssum ~ seen + cp6layer + pred + (1 + layer | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  7 -342.89 -309.28 178.45  -356.89                             
## model   8 -419.34 -380.92 217.67  -435.34 78.444      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  pred  *** "
## [1] "remove . ~ . -  cp6layer "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4
## Models:
## model2: abssum ~ seen + (1 + layer | sub)
## model: abssum ~ seen + cp6layer + (1 + layer | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  6 -320.09 -291.28 166.05  -332.09                             
## model   7 -342.89 -309.28 178.45  -356.89 24.801      1  6.357e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  cp6layer  *** "
## [1] "remove . ~ . -  seen "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4
## Models:
## model2: abssum ~ (1 + layer | sub)
## model: abssum ~ seen + (1 + layer | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  5 -274.43 -250.42 142.22  -284.43                             
## model   6 -320.09 -291.28 166.05  -332.09 47.662      1  5.065e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  seen  *** "
```

```r
  # this model is for posthocs

rominput4word = subset(rominput4,layer=="word")
randomeff2 = paste(" + (1  |sub)") 
formu = as.formula(paste(dependMeasure,"~ pred + seen",randomeff2))
omnimodelw = lmer(formu, rominput4word ,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
print(summary(omnimodelw))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ pred + seen + (1 | sub)
##    Data: rominput4word
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: -854.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.7482 -0.4860  0.0008  0.5856  2.8554 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  sub      (Intercept) 0.015860 0.12594 
##  Residual             0.007627 0.08733 
## Number of obs: 450, groups:  sub, 10
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) 1.503174   0.040037   37.55
## pred        0.152273   0.010084   15.10
## seen        0.132320   0.005822   22.73
## 
## Correlation of Fixed Effects:
##      (Intr) pred 
## pred 0.000       
## seen 0.000  0.000
```

```r
modellist3w = modelComparison(omnimodelw) # create anova table
```

```
## [1] "remove . ~ . -  seen "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4word
## Models:
## model2: abssum ~ pred + (1 | sub)
## model: abssum ~ pred + seen + (1 | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  4 -523.99 -507.55 266.00  -531.99                             
## model   5 -864.74 -844.19 437.37  -874.74 342.75      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  seen  *** "
## [1] "remove . ~ . -  pred "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4word
## Models:
## model2: abssum ~ (1 | sub)
## model: abssum ~ pred + (1 | sub)
##        Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  3 -431.74 -419.41 218.87  -437.74                             
## model   4 -523.99 -507.55 266.00  -531.99 94.249      1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  pred  *** "
```

```r
rominput4hid = subset(rominput4,layer=="hidden")
omnimodelh = lmer(formu, rominput4hid,control=lmerControl(optimizer = "bobyqa",optCtrl=list(maxfun=50000)))
print(summary(omnimodelh))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: abssum ~ pred + seen + (1 | sub)
##    Data: rominput4hid
## Control: lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 50000))
## 
## REML criterion at convergence: 16.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8909 -0.6411 -0.0735  0.5026  4.0139 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  sub      (Intercept) 0.05013  0.2239  
##  Residual             0.05436  0.2331  
## Number of obs: 450, groups:  sub, 10
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  0.67261    0.07165   9.388
## pred         0.11788    0.02692   4.379
## seen        -0.00632    0.01554  -0.407
## 
## Correlation of Fixed Effects:
##      (Intr) pred 
## pred 0.000       
## seen 0.000  0.000
```

```r
modellist3h = modelComparison(omnimodelh) # create anova table
```

```
## [1] "remove . ~ . -  seen "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4hid
## Models:
## model2: abssum ~ pred + (1 | sub)
## model: abssum ~ pred + seen + (1 | sub)
##        Df     AIC    BIC   logLik deviance  Chisq Chi Df Pr(>Chisq)
## model2  4  9.1517 25.589 -0.57584  1.15167                         
## model   5 10.9856 31.532 -0.49281  0.98561 0.1661      1     0.6836
## [1] "########## Above comparison for  seen "
## [1] "remove . ~ . -  pred "
```

```
## refitting model(s) with ML (instead of REML)
```

```
## Data: rominput4hid
## Models:
## model2: abssum ~ (1 | sub)
## model: abssum ~ pred + (1 | sub)
##        Df     AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## model2  3 25.9969 38.325 -9.9984  19.9969                             
## model   4  9.1517 25.589 -0.5758   1.1517 18.845      1  1.418e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## [1] "########## Above comparison for  pred  *** "
```

```r
rominput4$Prime = rominput4$Condition2
  p = drawERP(rominput4,dependMeasure, "Prime", span=1, timeline.y=2)
  p
```

![plot of chunk unnamed-chunk-58](figure/unnamed-chunk-58-1.png)

```r
if (!noslopes){ # when doing final version, change noslopes to FALSE
    myggsave("img/rommers2.png",width=figwidth,height=figheight)
}
```

CUT AND PASTE INTO LATEX DOC
click in box and use right arrow to see whole text

```latex

This is an example of the stimuli used in the model.


\begin{tabular}{ll}
\toprule
Example & Condition\\
\midrule
she drink -ss the water . . & Previously Predictable\\
she sniff -ss the water . . & Previously Unpredictable\\
a grandma will jump . . & Not Previously Seen\\
he is taste -ing the water . . & Critical Target\\
\bottomrule
\end{tabular}
The maximal model for the data had random slopes  + (1 +layer |sub)

Mixed Model Results
a main effect of seen ,$\beta$=0.063, SE=0.0083, $\chi^2$(1)=47.66, p$<$0.001 
a main effect of cp6layer ,$\beta$=-0.83, SE=0.084, $\chi^2$(1)=24.8, p$<$0.001 
a main effect of pred ,$\beta$=0.14, SE=0.014, $\chi^2$(1)=78.44, p$<$0.001 
an interaction of seen:cp6layer ,$\beta$=-0.14, SE=0.017, $\chi^2$(1)=67.33, p$<$0.001 
@@no interaction of cp6layer:pred ,$\beta$=-0.034, SE=0.029, $\chi^2$(1)=1.44, p=0.231 

Mixed Model Results
a main effect of pred ,$\beta$=0.15, SE=0.01, $\chi^2$(1)=94.25, p$<$0.001 
@@a main effect of seen ,$\beta$=0.13, SE=0.0058, $\chi^2$(1)=342.75, p$<$0.001 

Mixed Model Results
a main effect of pred ,$\beta$=0.12, SE=0.027, $\chi^2$(1)=18.85, p$<$0.001 
@@no main effect of seen ,$\beta$=-0.0063, SE=0.016, $\chi^2$(1)=0.17, p=0.684 
```

## Overall accuracy results


```r
test = read.csv("testdata.csv")
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
testsum = aggregate(cbind(corr,total) ~ epoch, test, sum)
```

```
## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type
```

```r
testsum=rbind(testsum,data.frame(epoch=0,corr=0,total=200))
```

```
## Error in rbind(testsum, data.frame(epoch = 0, corr = 0, total = 200)): object 'testsum' not found
```

```r
testsum$perc = 100* testsum$corr/testsum$total
```

```
## Error in eval(expr, envir, enclos): object 'testsum' not found
```

```r
p1=ggplot(testsum,aes(x=epoch,y=perc))+geom_line()+ylim(0,100) 
```

```
## Error in ggplot(testsum, aes(x = epoch, y = perc)): object 'testsum' not found
```

```r
  p1 = p1 + scale_colour_brewer( palette="Set1")
  p1 = p1 + theme_bw() + xlab("Number of patterns trained") 
p1
```

![plot of chunk unnamed-chunk-60](figure/unnamed-chunk-60-1.png)

```r
paste("@@Test Accuracy ",tail(aggregate(perc ~ epoch, test, mean),1))
```

```
## Error in terms.formula(formula, data = data): 'data' argument is of the wrong type
```

## Example data for introduction
- These figures below are made up data that help to explain the model
- Word Level: The children went out to ...


```
##         cond Context value       variable Lexical
## 1   Expected  Strong   0.7 Sum Abs. Error    move
## 2 Unexpected  Strong   1.5 Sum Abs. Error    move
## 3   Expected    Weak   1.1 Sum Abs. Error    move
## 4 Unexpected    Weak   1.5 Sum Abs. Error    move
```

![plot of chunk unnamed-chunk-61](figure/unnamed-chunk-61-1.png)

- N400 is sensitive to cloze probabilites of words

## Agreement Lexical Level: The spoilt child throws the toys on the floor


```
##   Grammaticality value       variable Lexical
## 1          Gram.   1.2 Sum Abs. Error     SUM
## 2        Ungram.   1.6 Sum Abs. Error     SUM
```

```
##    Grammaticality Lexical  variable value
## 1           Gram.     the Predicted   0.3
## 2           Gram.       a Predicted   0.3
## 3           Gram.     -ed Predicted   0.1
## 4           Gram.     -ss Predicted   0.1
## 5         Ungram.     the Predicted   0.3
## 6         Ungram.       a Predicted   0.3
## 7         Ungram.     -ed Predicted   0.1
## 8         Ungram.     -ss Predicted   0.1
## 9           Gram.     the    Target   1.0
## 10          Gram.       a    Target   0.0
## 11          Gram.     -ed    Target   0.0
## 12          Gram.     -ss    Target   0.0
## 13        Ungram.     the    Target   0.0
## 14        Ungram.       a    Target   0.0
## 15        Ungram.     -ed    Target   1.0
## 16        Ungram.     -ss    Target   0.0
## 17          Gram.     the     Error  -0.7
## 18          Gram.       a     Error   0.3
## 19          Gram.     -ed     Error   0.1
## 20          Gram.     -ss     Error   0.1
## 21        Ungram.     the     Error   0.3
## 22        Ungram.       a     Error   0.3
## 23        Ungram.     -ed     Error  -0.9
## 24        Ungram.     -ss     Error   0.1
```

![plot of chunk unnamed-chunk-62](figure/unnamed-chunk-62-1.png)

- Syntactic distinctions do not show an N400

## Agreement Syntactic Level: The spoilt child throws the toys on the floor


```
##   Grammaticality Lexical Predicted Target Error absError Weight
## 1          Gram.     the       0.3      1  -0.7      0.7      4
## 2          Gram.       a       0.3      0   0.3      0.3      4
## 3          Gram.     -ed       0.1      0   0.1      0.1     -1
## 4          Gram.     -ss       0.1      0   0.1      0.1     -1
## 5        Ungram.     the       0.3      0   0.3      0.3      4
## 6        Ungram.       a       0.3      0   0.3      0.3      4
## 7        Ungram.     -ed       0.1      1  -0.9      0.9     -1
## 8        Ungram.     -ss       0.1      0   0.1      0.1     -1
##   NetinputBack Weight2 NetinputBack2
## 1         -2.8      -1           0.7
## 2          1.2      -1          -0.3
## 3         -0.1       4           0.4
## 4         -0.1       4           0.4
## 5          1.2      -1          -0.3
## 6          1.2      -1          -0.3
## 7          0.9       4          -3.6
## 8         -0.1       4           0.4
```

```
##   Grammaticality NetinputBack NetinputBack2 Error
## 1          Gram.         -1.8           1.2     3
## 2        Ungram.          3.2          -3.8     7
```

![plot of chunk unnamed-chunk-63](figure/unnamed-chunk-63-1.png)

## Agreement Wave ERP: The spoilt child throws the toys on the floor

![plot of chunk unnamed-chunk-64](figure/unnamed-chunk-64-1.png)

## Draw Sequencing-Hidden Network


```
##   Grammaticality Lexical Predicted Target Error absError Weight
## 1          Gram.     the       0.3      1  -0.7      0.7      4
## 2          Gram.       a       0.3      0   0.3      0.3      4
## 3          Gram.     -ed       0.1      0   0.1      0.1     -1
## 4          Gram.     -ss       0.1      0   0.1      0.1     -1
## 5        Ungram.     the       0.3      0   0.3      0.3      4
## 6        Ungram.       a       0.3      0   0.3      0.3      4
## 7        Ungram.     -ed       0.1      1  -0.9      0.9     -1
## 8        Ungram.     -ss       0.1      0   0.1      0.1     -1
##   NetinputBack Weight2 NetinputBack2 Sequencing
## 1         -2.8      -1           0.7         S1
## 2          1.2      -1          -0.3         S1
## 3         -0.1       4           0.4         S1
## 4         -0.1       4           0.4         S1
## 5          1.2      -1          -0.3         S1
## 6          1.2      -1          -0.3         S1
## 7          0.9       4          -3.6         S1
## 8         -0.1       4           0.4         S1
```

![plot of chunk unnamed-chunk-65](figure/unnamed-chunk-65-1.png)



## box diagram




## Word Level

![plot of chunk unnamed-chunk-67](figure/unnamed-chunk-67-1.png)



## Draw Network

![plot of chunk unnamed-chunk-68](figure/unnamed-chunk-68-1.png)


## Draw Brain1

![plot of chunk unnamed-chunk-69](figure/unnamed-chunk-69-1.png)

## Draw Brain2

![plot of chunk unnamed-chunk-70](figure/unnamed-chunk-70-1.png)


## Draw Brain3

![plot of chunk unnamed-chunk-71](figure/unnamed-chunk-71-1.png)
