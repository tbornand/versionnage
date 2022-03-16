#library
library(haven) # read_sav import spss files
library(labelled)
library(questionr)
library(sjlabelled)
library(tidyverse) #ggplot
library(survey)
library(gtools) #smartbind fusion de df

### importer la database puis travailler à partir du sub database sans "02"

#A)
#L'outil d'importation R ne fonctionne pas pour les données cumulée. 
#Il faut donc charger les données via le site en format "spss" puis importer les données via l'outil d'importation
# -> BE02_18

BE02_18 <- read_sav("ESS1-9e01_1.sav")


#B) créer la variable "regbe" pour regions Belge. Cette variable compile l'info sous deux variables différentes dans le 
# data frame "cregion" et "regionbe"

BE02_18$regbe[BE02_18$cregion =="BE10"] <-1
BE02_18$regbe[BE02_18$cregion %in% c("BE21", "BE22", "BE23", "BE24", "BE25")] <-2
BE02_18$regbe[BE02_18$cregion %in% c("BE31", "BE32", "BE33", "BE34", "BE35")] <-3
BE02_18$regbe[BE02_18$regionbe ==2] <-1
BE02_18$regbe[BE02_18$regionbe ==1] <-2
BE02_18$regbe[BE02_18$regionbe ==3] <-3
library(sjlabelled)
BE02_18$regbe <- set_labels(BE02_18$regbe, labels = c("Bruxelles"=1, "Flandre"=2, "Wallonie"=3))
library(sjmisc)
frq(BE02_18$regbe)




# creation des indices ----------------------------------------------------


#'incdice conifance dans les insitutions "trstinst": moyenne des 4 institutions et valeurs supérieure à 5 (échelle 0 à 10)
# - parlement fédéral "trstprl"
# - justice "trstlgl"
# - femmes/hommes pol "trstplt"
# - les partis pol "trstprt"

#moyenne des 4 institutions
library(tidyverse)
BE <- transform( BE,
                      trstinst.4m = rowMeans(subset(BE, select = c(trstprl,
                                                                     trstplt,
                                                                     trstprt,
                                                                     trstlgl)), na.rm = FALSE))

BE$trstinst.4d <-cut(BE$trstinst.4m,c(0,5,10), include.lowest = TRUE) #indice moyen dichotomisé <=5; >5


##indice satisfaction démocratique supérieur à 5 (échelle 0 à 10)
BE$stfdem.d <-cut(BE$stfdem,c(0,5,10), include.lowest = TRUE) 

## creation des variables de "cntry_ess" (BE*ESS)


BE$cntry_ess <- ifelse(BE$cntry=="BE" & BE$essround==2, "BE04", 
                ifelse(BE$cntry=="BE" & BE$essround==3, "BE06", 
                ifelse(BE$cntry=="BE" & BE$essround==4, "BE08", 
                ifelse(BE$cntry=="BE" & BE$essround==5, "BE10",
                ifelse(BE$cntry=="BE" & BE$essround==6, "BE12", 
                ifelse(BE$cntry=="BE" & BE$essround==7, "BE14", 
                ifelse(BE$cntry=="BE" & BE$essround==8, "BE16", 
                ifelse(BE$cntry=="BE" & BE$essround==9, "BE18", NA))))))))


## création de la variable "reg_ess (reg*ESS) !!!cette variable devient caduque avec "regyear"

BE$reg_ess <- ifelse(BE$regbe==3 & BE$essround==2, "WAL04",
              ifelse(BE$regbe==3 & BE$essround==3, "WAL06",
              ifelse(BE$regbe==3 & BE$essround==4, "WAL08",
              ifelse(BE$regbe==3 & BE$essround==5, "WAL10",
              ifelse(BE$regbe==3 & BE$essround==6, "WAL12",
              ifelse(BE$regbe==3 & BE$essround==7, "WAL14",     
              ifelse(BE$regbe==3 & BE$essround==8, "WAL16",
              ifelse(BE$regbe==3 & BE$essround==9, "WAL18",NA))))))))


# recodage de la variable revenu en revenu quintile

BE$hinctnta_rec <- as.character(BE$hinctnta)
BE$hinctnta_rec <- fct_recode(BE$hinctnta_rec,
                              "1" = "1",
                              "1" = "2",
                              "2" = "3",
                              "2" = "4",
                              "3" = "5",
                              "3" = "6",
                              "4" = "7",
                              "4" = "8",
                              "5" = "9",
                              "5" = "10" 
)
str(BE$hinctnta_rec)
BE$hinctnta_rec <- factor(BE$hinctnta_rec, levels = c("1", "2", "3", "4", "5")) #remet les niveau de facteur dans le bon ordre

###calcul indicateur pour 2018 uniquement
## Table BE denière année
BE18 <- subset(BE, essround == 9) 

## création de la variable "education"

# regroupe les modalités 
# 1-4 =>1 "primaire et inférieur"
# 5-11=>2 "secondaire"
# 12-18=>3 "supérieur"

BE18$edu.3m <- cut(BE18$edlvebe,c(1,4,11,18), labels = FALSE, include.lowest = TRUE) #labels= F pour ne pas avoir de "levels"
library(sjlabelled)
BE18$edu.3m  <- set_labels(BE18$edu.3m , labels = c("prim" = 1, "sec" = 2, "sup" = 3)) # ajoute des values labels

## création de la variable "sex.edu"
BE18$sex.edu <- ifelse(BE18$gndr==1 & BE18$edu.3m==1 , "HPrim", 
                       ifelse(BE18$gndr==1 & BE18$edu.3m==2 , "HSec",
                              ifelse(BE18$gndr==1 & BE18$edu.3m==3 , "HSup",
                                     ifelse(BE18$gndr==2 & BE18$edu.3m==1 , "FPrim",
                                            ifelse(BE18$gndr==2 & BE18$edu.3m==2 , "FSec",
                                                   ifelse(BE18$gndr==2 & BE18$edu.3m==3 , "FSup",NA))))))


## création de la variable "walsex.edu" c'est à dire sex.edu seulement pour les wallon.ne.s

BE18$walsex.edu <- ifelse(BE18$regbe==3 & BE18$gndr==1 & BE18$edu.3m==1 , "HPrim", 
                          ifelse(BE18$regbe==3 & BE18$gndr==1 & BE18$edu.3m==2 , "HSec",
                                 ifelse(BE18$regbe==3 & BE18$gndr==1 & BE18$edu.3m==3 , "HSup",
                                        ifelse(BE18$regbe==3 & BE18$gndr==2 & BE18$edu.3m==1 , "FPrim",
                                               ifelse(BE18$regbe==3 & BE18$gndr==2 & BE18$edu.3m==2 , "FSec",
                                                      ifelse(BE18$regbe==3 & BE18$gndr==2 & BE18$edu.3m==3 , "FSup",NA))))))






##pondération des table
library(survey)
BEw <- svydesign(ids = ~1, data = BE, weights = ~ BE$pspwght)
BE18w <- svydesign(ids = ~1, data = BE18, weights = ~ BE18$pspwght)

# Résultats des indicateurs -----------------------------------------------

### 1) résultat longitudinal indicateur "confiance politique"  avec et sans BXL

synt.reg<-svyby(~trstinst.4d, ~regyear, BEw, svyciprop, vartype="ci")

synt.bel<-svyby(~trstinst.4d, ~cntry_ess, BEw, svyciprop, vartype="ci")

library(gtools) # fusion des 2 df à l'aide de smartbind
synt2<-smartbind(synt.reg, synt.bel)

#ajoute les variables année, région, et recalcul les indices en % (2 digits)
synt2$year <- c("04",   
                "06", 
                "08", 
                "10", 
                "12",
                "14", 
                "16", 
                "18")

synt2$reg <- rep(c("BXL", "FLA", "WAL", "BEL"), each=8) # creat vector with repeated value
synt2$confinst <- round(synt2$trstinst.4d*100, digits = 2)
synt2$ci_inf <- round(synt2$ci_l*100, digits = 2)
synt2$ci_sup <- round(synt2$ci_u*100, digits = 2)
synt2$ci_marge <- round((synt2$ci_u - synt2$ci_l)*100/2, digits = 2)

# Plot
synt2 %>%
  ggplot( aes(x=year, y=confinst, group=reg, color=reg)) +
  geom_line() +
  geom_point(size = 0.1) +
  geom_text(aes(label = confinst), size = 2,hjust = 1.2, vjust = 2)+
  geom_errorbar(aes(ymax = ci_sup, ymin = ci_inf)) +
  ylim(20, 80) +
  labs(y= "Confiance politique en %", x = "année")


### sans BXL

synt3 <- subset(synt2, reg != "BXL") 

synt3 %>%
  ggplot( aes(x=year, y=confinst, group=reg, color=reg)) +
  geom_line() +
  geom_point(size = 0.1) +
  geom_text(aes(label = confinst), size = 2,hjust = 1.2, vjust = 2)+
  geom_errorbar(aes(ymax = ci_sup, ymin = ci_inf)) +
  ylim(20, 80) +
  labs(y= "Confiance politique en %", x = "année")


####2) Confiance politique par revenu quintile Wallonie - BElgique

#résultat confiance politique en revenu
synt.bel18.rev <-svyby(~trstinst.4d, ~hinctnta_rec, BE18w, svyciprop, vartype="ci")
synt.bel18.rev$reg <- "Belgique" # ajoute variable "région", modalité "Belgique""
synt.wal18.rev <-svyby(~trstinst.4d, ~hinctnta_rec, subset(BE18w, BE18w$variables$regbe ==3), svyciprop, vartype="ci") # subset pour avoir le calcul que pour la Wallonie
synt.wal18.rev$reg <- "Wallonie"


#Fusion des deux
synt.rev<-smartbind(synt.bel18.rev,synt.wal18.rev)    
synt.rev$rev2<-c("1QB", "2QB", "3QB", "4QB", "5QB",
                 "1QW", "2QW", "3QW", "4QW", "5QW"
)
synt.rev$rev2<- factor(synt.rev$rev2, levels = c("1QB", "2QB", "3QB", "4QB", "5QB",# pour mettre le facteur dans le bon ordre des modalités
                                                 "1QW", "2QW", "3QW", "4QW", "5QW"
))
synt.rev$conf <- round(synt.rev$trstinst.4d*100, digits = 2)
synt.rev$ci_inf <- round(synt.rev$ci_l*100, digits = 2)
synt.rev$ci_sup <- round(synt.rev$ci_u*100, digits = 2)
synt.rev$ci_marge <- round((synt.rev$ci_u - synt.rev$ci_l)*100/2, digits = 2)

## Plot

ggplot(synt.rev) +
  geom_bar( aes(x=rev2, y=conf), stat="identity", fill="skyblue", width = 0.5) +
  coord_cartesian(ylim=c(20,80)) + # pour limiter l'axe des x dans "geom_bar 
  geom_errorbar( aes(x=rev2, ymin=ci_inf, ymax=ci_sup), width=0.4, colour="dark blue", alpha=0.9, size=1.3) +
  xlab("Belgique                               Wallonie") +
  ylab("Confiance politique")  +
  geom_text(aes(x=rev2, y=conf, label = conf), size = 2.5,hjust = 1.2, vjust =- 1)




#### 3) résultat confiance politique en croisant Sexe 
synt.bel18.sex <-svyby(~trstinst.4d, ~gndr, BE18w, svyciprop, vartype="ci")
synt.bel18.sex$reg <- "Belgique" # ajoute variable "région", modalité "Belgique""
synt.wal18.sex <-svyby(~trstinst.4d, ~gndr, subset(BE18w, BE18w$variables$regbe ==3), svyciprop, vartype="ci") # subset pour avoir le calcul que pour la Wallonie
synt.wal18.sex$reg <- "Wallonie"

#Fusion des deux
synt.sex<-smartbind(synt.bel18.sex, synt.wal18.sex)    
synt.sex$sex<-c("B.H", "B.F", "W.H", "W.F")
synt.sex$conf <- round(synt.sex$trstinst.4d*100, digits = 2)
synt.sex$ci_inf <- round(synt.sex$ci_l*100, digits = 2)
synt.sex$ci_sup <- round(synt.sex$ci_u*100, digits = 2)
synt.sex$ci_marge <- round((synt.sex$ci_u - synt.sex$ci_l)*100/2, digits = 2)


#Plot
ggplot(synt.sex) +
  geom_bar( aes(x=sex, y=conf), stat="identity", fill="skyblue", width = 0.5) +
  coord_cartesian(ylim=c(20,80)) + # pour limiter l'axe des x dans "geom_bar 
  geom_errorbar( aes(x=sex, ymin=ci_inf, ymax=ci_sup), width=0.4, colour="dark blue", alpha=0.9, size=1.3) +
  xlab("Belgique                               Wallonie") +
  ylab("Confiance politique")   +
  geom_text(aes(x=sex, y=conf, label = conf), size = 2.5,hjust = 1.2, vjust =- 1)


###4) résultat longitudinal "satisfaction démocratique"  

sat.wal<-svyby(~stfdem.d, ~reg_ess, BEw, svyciprop, vartype="ci")
sat.bel<-svyby(~stfdem.d, ~cntry_ess, BEw, svyciprop, vartype="ci")

library(gtools) # fusion des 2 df à l'aide de smartbind
sat<-smartbind(sat.wal, sat.bel)

#ajoute les variables année, région, et recalcul les indice en % (2 digits)
sat$year <- c("2004",   # ajoute la variable année
              "2006", 
              "2008", 
              "2010", 
              "2012",
              "2014", 
              "2016", 
              "2018")
reg<- c("Wal","Wal","Wal","Wal","Wal","Wal","Wal","Wal",
        "Bel","Bel","Bel","Bel","Bel","Bel","Bel","Bel")
sat$reg <- reg
sat$satdem <- round(sat$stfdem.d*100, digits = 1)
sat$ci_inf <- round(sat$ci_l*100, digits = 1)
sat$ci_sup <- round(sat$ci_u*100, digits = 1)
sat$ci_marge <- round((sat$ci_u - sat$ci_l)*100/2, digits = 1)

# Plot
sat %>%
  ggplot( aes(x=year, y=satdem, group=reg, color=reg)) +
  geom_line() +
  geom_point(size = 0.1) +
  geom_text(aes(label = satdem), size = 2,hjust = 1.2, vjust = 2)+
  geom_errorbar(aes(ymax = ci_sup, ymin = ci_inf)) +
  ylim(20, 80) +
  labs(y= "Satisfaction envers la démocratie %", x = "année")


### 5) satisfaction démocratique avec revenu 5 déciles

sat.bel18.r5 <-svyby(~stfdem.d, ~hinctnta_rec, BE18w, svyciprop, vartype="ci")
sat.bel18.r5$reg <- "Belgique" # ajoute variable "région", modalité "Belgique""
sat.wal18.r5 <-svyby(~stfdem.d, ~hinctnta_rec, subset(BE18w, BE18w$variables$regbe ==3), svyciprop, vartype="ci") # subset pour avoir le calcul que pour la Wallonie
sat.wal18.r5$reg <- "Wallonie"


#Fusion des deux
sat.r5<-smartbind(sat.bel18.r5, sat.wal18.r5)    
sat.r5$rev2<-c("1QB", "2QB", "3QB", "4QB", "5QB",
               "1QW", "2QW", "3QW", "4QW", "5QW"
)
sat.r5$rev2<- factor(synt.rev$rev2, levels = c("1QB", "2QB", "3QB", "4QB", "5QB",# pour mettre le facteur dans le bon ordre des modalités
                                               "1QW", "2QW", "3QW", "4QW", "5QW"
))
sat.r5$sat <- round(sat.r5$stfdem.d*100, digits = 1)
sat.r5$ci_inf <- round(sat.r5$ci_l*100, digits = 1)
sat.r5$ci_sup <- round(sat.r5$ci_u*100, digits = 1)
sat.r5$ci_marge <- round((sat.r5$ci_u - sat.r5$ci_l)*100/2, digits = 1)

## Plot

ggplot(sat.r5) +
  geom_bar( aes(x=rev2, y=sat), stat="identity", fill="skyblue", width = 0.5) +
  coord_cartesian(ylim=c(20,80)) + # pour limiter l'axe des x dans "geom_bar 
  geom_errorbar( aes(x=rev2, ymin=ci_inf, ymax=ci_sup), width=0.4, colour="dark blue", alpha=0.9, size=1.3) +
  xlab("Belgique                               Wallonie") +
  ylab("Confiance politique")  +
  geom_text(aes(x=rev2, y=sat, label = sat), size = 2.5,hjust = 1.2, vjust =- 1)


#### 6) résultat satisfaction en croisant Sexe 
sat.bel18.sex <-svyby(~stfdem.d, ~gndr, BE18w, svyciprop, vartype="ci")
sat.bel18.sex$reg <- "Belgique" # ajoute variable "région", modalité "Belgique""
sat.wal18.sex <-svyby(~stfdem.d, ~gndr, subset(BE18w, BE18w$variables$regbe ==3), svyciprop, vartype="ci") # subset pour avoir le calcul que pour la Wallonie
sat.wal18.sex$reg <- "Wallonie"

#Fusion des deux
sat.sex<-smartbind(sat.bel18.sex, sat.wal18.sex)    
sat.sex$sex<-c("B.H", "B.F", "W.H", "W.F")
sat.sex$sat <- round(sat.sex$stfdem.d*100, digits = 1)
sat.sex$ci_inf <- round(sat.sex$ci_l*100, digits = 1)
sat.sex$ci_sup <- round(sat.sex$ci_u*100, digits = 1)
sat.sex$ci_marge <- round((sat.sex$ci_u - sat.sex$ci_l)*100/2, digits = 1)


#Plot
ggplot(sat.sex) +
  geom_bar( aes(x=sex, y=sat), stat="identity", fill="skyblue", width = 0.5) +
  coord_cartesian(ylim=c(20,80)) + # pour limiter l'axe des x dans "geom_bar 
  geom_errorbar( aes(x=sex, ymin=ci_inf, ymax=ci_sup), width=0.4, colour="dark blue", alpha=0.9, size=1.3) +
  xlab("Belgique                               Wallonie") +
  ylab("Satisfaction démocratie")   +
  geom_text(aes(x=sex, y=sat, label = sat), size = 2.5,hjust = 1.2, vjust =- 1)




### export des data en .xls


library(openxlsx)
#confiance politique
dataset_names <- list('Evol' = synt3, 'Revenu' = synt.rev, 'Sexe' = synt.sex) # export dans plusieurs feuilles .xls
write.xlsx(dataset_names, file = 'data confiance politique.xlsx')
#satisfaction démocratique
dataset_names <- list('Evol' = sat, 'Revenu' = sat.r5, 'Sexe' = sat.sex) # export dans plusieurs feuilles .xls
write.xlsx(dataset_names, file = 'data satisfaction democratique.xlsx')



