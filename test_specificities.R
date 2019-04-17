source("functionsForGeoKnowledgeApp.R", local = T)
all.data=readRDS("all.unique.data.rds")
#add i row for facilitating the subsetting
all.data$i=seq(1:nrow(all.data))
doc.term.matrix=readRDS("2Orgs_sparseMatrix-2019-04-03.rds")

specificites = quanteda::textstat_keyness(x=doc.term.matrix, target=all.data$organization=="Quake Map", measure="chi2", sort=TRUE)

#filter NA
specificites=specificites[is.na(specificites$chi2)==F,]

#set p-value to 0.05, but could be 0.01 if we want to select only reliable specificities 
specificites=specificites[specificites$p<0.05]
View(specificites)
n=30
top.spec=top_n(x = specificites, n=n, wt=chi2)
top.spec$ORGANIZATION="Quake Map"
bottom.spec=top_n(x = specificites, n=-n, wt=chi2)
bottom.spec$ORGANIZATION="Doctors without borders"

dat=rbind(top.spec,bottom.spec)
ggplot(data = dat, aes(x = reorder(feature, chi2), y = chi2, fill=ORGANIZATION)) +
  geom_bar(stat = "identity", width = .9, position = position_dodge(width = 2)) +
  #geom_text(aes(label=feature,vjust=0.25, hjust=ifelse(chi2 >= 0, -0.25, 1.25)))+
  #scale_y_continuous(labels = scales::percent_format())+
  ylab("Specificityies score (chi2)")+
  xlab("Lexical Specificities")+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()+
  scale_fill_manual(values = c("sky blue", "grey28"))
  #coord_cartesian(xlim=c(0,23))
  #geom_text(aes(label=feature), nudge_y = 1, vjust=0.25)

  

