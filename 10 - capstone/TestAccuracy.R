library(tidyr)
library(dplyr)
TestSet<-readRDS("TestSet2.rds")
TestSet<-separate(TestSet,label,c("pre1","pre2","pre3","post"),sep=" ")
set.seed(123)
rowselect<-NULL
SubSetTest <- function(n) {
      set.seed(123)
      TestSet$prob<-TestSet$freq/sum(TestSet$freq)
      TestSet$rowsel<-rbinom(nrow(TestSet),n,TestSet$prob)
      TestSetSub<-filter(TestSet,rowsel>0)
      return(TestSetSub)
}

TestSetSub<-SubSetTest(5000)
print(nrow(TestSetSub))
#comment all the above if testsetsub already there
TestSetSub<-TestSetSub[,1:7]
TestSetSub <- TestSetSub %>% rowwise()
print("2")
t2<-system.time(TestSetSub<-TestSetSub %>% mutate(best2.pred=paste(findBest2U(pre3),collapse=".")) %>%
      separate(best2.pred,c("best2.pred1","best2.pred2","best2.pred3",
                            "best2.pred4","best2.pred5"),sep="\\."))
acc.best2<-sum((TestSetSub$post==TestSetSub$best2.pred1)*
                      1,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best2<-sum((TestSetSub$post==TestSetSub$best2.pred1 |
                       TestSetSub$post==TestSetSub$best2.pred2 |
                       TestSetSub$post==TestSetSub$best2.pred3)*1
                     ,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best2<-sum((TestSetSub$post==TestSetSub$best2.pred1 |
                       TestSetSub$post==TestSetSub$best2.pred2 |
                       TestSetSub$post==TestSetSub$best2.pred3 |
                       TestSetSub$post==TestSetSub$best2.pred4 |
                       TestSetSub$post==TestSetSub$best2.pred5)*
                      1,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best2)
print(acc3.best2)
print(acc5.best2)
print("3")
t3<-system.time(TestSetSub<-TestSetSub %>% mutate(best3.pred=paste(findBest3U(c(pre2,pre3)),
                                  collapse=".")) %>%
      separate(best3.pred,c("best3.pred1","best3.pred2","best3.pred3",
                            "best3.pred4","best3.pred5"),
               sep="\\."))
acc.best3<-sum((TestSetSub$post==TestSetSub$best3.pred1)*
                      1,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best3<-sum((TestSetSub$post==TestSetSub$best3.pred1 |
                       TestSetSub$post==TestSetSub$best3.pred2 |
                       TestSetSub$post==TestSetSub$best3.pred3)*
                     1,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best3<-sum((TestSetSub$post==TestSetSub$best3.pred1 |
                       TestSetSub$post==TestSetSub$best3.pred2 |
                       TestSetSub$post==TestSetSub$best3.pred3 |
                       TestSetSub$post==TestSetSub$best3.pred4 |
                       TestSetSub$post==TestSetSub$best3.pred5)*
                      1,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best3)
print(acc3.best3)
print(acc5.best3)
print("4")
t4<-system.time(TestSetSub<-TestSetSub %>% mutate(best4.pred=paste(findBest4U(c(pre1,pre2,pre3)),
                                  collapse=".")) %>%
      separate(best4.pred,c("best4.pred1","best4.pred2","best4.pred3",
                            "best4.pred4","best4.pred5"),
               sep="\\."))
acc.best4<-sum((TestSetSub$post==TestSetSub$best4.pred1)*
                      1,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best4<-sum((TestSetSub$post==TestSetSub$best4.pred1 |
                       TestSetSub$post==TestSetSub$best4.pred2 |
                       TestSetSub$post==TestSetSub$best4.pred3)*
                     1,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best4<-sum((TestSetSub$post==TestSetSub$best4.pred1 |
                       TestSetSub$post==TestSetSub$best4.pred2 |
                       TestSetSub$post==TestSetSub$best4.pred3 |
                       TestSetSub$post==TestSetSub$best4.pred4 |
                       TestSetSub$post==TestSetSub$best4.pred5)*
                      1,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best4)
print(acc3.best4)
print(acc5.best4)
if (FALSE) {
t3.noRoll<-system.time(TestSetSub<-TestSetSub %>% mutate(best3.pred.noRoll=
                        paste(findBest3UnoRoll(c(pre2,pre3)),
                        collapse=".")) %>%
      separate(best3.pred.noRoll,c("best3.pred1.noRoll","best3.pred2.noRoll",
                                   "best3.pred3.noRoll",
                                   "best3.pred4.noRoll","best3.pred5.noRoll"),
               sep="\\."))
acc.best3.noRoll<-sum((TestSetSub$post==TestSetSub$best3.pred1.noRoll)*
                             1,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best3.noRoll<-sum((TestSetSub$post==TestSetSub$best3.pred1.noRoll |
                             TestSetSub$post==TestSetSub$best3.pred2.noRoll |
                             TestSetSub$post==TestSetSub$best3.pred3.noRoll)*
                            1,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best3.noRoll<-sum((TestSetSub$post==TestSetSub$best3.pred1.noRoll |
                              TestSetSub$post==TestSetSub$best3.pred2.noRoll |
                              TestSetSub$post==TestSetSub$best3.pred3.noRoll |
                              TestSetSub$post==TestSetSub$best3.pred4.noRoll |
                              TestSetSub$post==TestSetSub$best3.pred5.noRoll)*
                             1,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best3.noRoll)
print(acc3.best3.noRoll)
print(acc5.best3.noRoll)

t4.noRoll<-system.time(TestSetSub<-TestSetSub %>% mutate(best4.pred.noRoll=
                        paste(findBest4UnoRoll(c(pre1,pre2,pre3)),
                        collapse=".")) %>%
      separate(best4.pred.noRoll,c("best4.pred1.noRoll",
                                   "best4.pred2.noRoll","best4.pred3.noRoll",
                                   "best4.pred4.noRoll","best4.pred5.noRoll"),
               sep="\\."))
acc.best4.noRoll<-sum((TestSetSub$post==TestSetSub$best4.pred1.noRoll)*
                             TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best4.noRoll<-sum((TestSetSub$post==TestSetSub$best4.pred1.noRoll |
                              TestSetSub$post==TestSetSub$best4.pred2.noRoll |
                              TestSetSub$post==TestSetSub$best4.pred3.noRoll)*
                             TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best4.noRoll<-sum((TestSetSub$post==TestSetSub$best4.pred1.noRoll |
                              TestSetSub$post==TestSetSub$best4.pred2.noRoll |
                              TestSetSub$post==TestSetSub$best4.pred3.noRoll |
                              TestSetSub$post==TestSetSub$best4.pred4.noRoll |
                              TestSetSub$post==TestSetSub$best4.pred5.noRoll)*
                             TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best4.noRoll)
print(acc3.best4.noRoll)
print(acc5.best4.noRoll)

t3.ood<-system.time(TestSetSub<-TestSetSub %>% mutate(best3.pred.ood=
                                        paste(findBest3ood(c(pre2,pre3)),
                                              collapse=".")) %>%
      separate(best3.pred.ood,c("best3.pred1.ood",
                                "best3.pred2.ood","best3.pred3.ood",
                                   "best3.pred4.ood","best3.pred5.ood"),
               sep="\\."))

acc.best3.ood<-sum((TestSetSub$post==TestSetSub$best3.pred1.ood)*
                         TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best3.ood<-sum((TestSetSub$post==TestSetSub$best3.pred1.ood |
                              TestSetSub$post==TestSetSub$best3.pred2.ood |
                              TestSetSub$post==TestSetSub$best3.pred3.ood)*
                             TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best3.ood<-sum((TestSetSub$post==TestSetSub$best3.pred1.ood |
                              TestSetSub$post==TestSetSub$best3.pred2.ood |
                              TestSetSub$post==TestSetSub$best3.pred3.ood |
                              TestSetSub$post==TestSetSub$best3.pred4.ood |
                              TestSetSub$post==TestSetSub$best3.pred5.ood)*
                             TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best3.ood)
print(acc3.best3.ood)
print(acc5.best3.ood)

t4.ood<-system.time(TestSetSub<-TestSetSub %>% mutate(best4.pred.ood=
                                        paste(findBest4ood(c(pre1,pre2,pre3)),
                                              collapse=".")) %>%
      separate(best4.pred.ood,c("best4.pred1.ood",
                                "best4.pred2.ood","best4.pred3.ood",
                                "best4.pred4.ood","best4.pred5.ood"),
               sep="\\."))

acc.best4.ood<-sum((TestSetSub$post==TestSetSub$best4.pred1.ood)*
                         TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best4.ood<-sum((TestSetSub$post==TestSetSub$best4.pred1.ood |
                           TestSetSub$post==TestSetSub$best4.pred2.ood |
                           TestSetSub$post==TestSetSub$best4.pred3.ood)*
                          TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best4.ood<-sum((TestSetSub$post==TestSetSub$best4.pred1.ood |
                           TestSetSub$post==TestSetSub$best4.pred2.ood |
                           TestSetSub$post==TestSetSub$best4.pred3.ood |
                           TestSetSub$post==TestSetSub$best4.pred4.ood |
                           TestSetSub$post==TestSetSub$best4.pred5.ood)*
                          TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best4.ood)
print(acc3.best4.ood)
print(acc5.best4.ood)

TestSetSub<-TestSetSub %>% mutate(best4.pred.oodi=
                                        paste(findBest4oodi(c(pre1,pre2,pre3))[1:5,"label"],
                                              collapse=".")) %>%
      separate(best4.pred.oodi,c("best4.pred1.oodi",
                                "best4.pred2.oodi","best4.pred3.oodi",
                                "best4.pred4.oodi","best4.pred5.oodi"),
               sep="\\.")

acc.best4.oodi<-sum((TestSetSub$post==TestSetSub$best4.pred1.oodi)*
                         TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc3.best4.oodi<-sum((TestSetSub$post==TestSetSub$best4.pred1.oodi |
                           TestSetSub$post==TestSetSub$best4.pred2.oodi |
                           TestSetSub$post==TestSetSub$best4.pred3.oodi)*
                          TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
acc5.best4.oodi<-sum((TestSetSub$post==TestSetSub$best4.pred1.oodi |
                           TestSetSub$post==TestSetSub$best4.pred2.oodi |
                           TestSetSub$post==TestSetSub$best4.pred3.oodi |
                           TestSetSub$post==TestSetSub$best4.pred4.oodi |
                           TestSetSub$post==TestSetSub$best4.pred5.oodi)*
                          TestSetSub$rowsel,na.rm=T)/sum(TestSetSub$rowsel)
print(acc.best4.oodi)
print(acc3.best4.oodi)
print(acc5.best4.oodi)
}