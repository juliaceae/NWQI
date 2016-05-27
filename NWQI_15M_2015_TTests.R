#National Water Quality Initiative 
#Fifteenmile Sediment 
#pre and post BMP implementation comparison
#Julia Crown May 2016
#

# Load data file 
RBS <- read.csv('\\\\Deqhq1\\TMDL\\NWQI\\Fifteenmile\\2015 Monitoring\\NWQI Field Data 2015\\NWQI-git\\Data\\DataAnalysisRBS.csv', header=T, stringsAsFactors=FALSE)
Wolman <- read.csv('\\\\Deqhq1\\TMDL\\NWQI\\Fifteenmile\\2015 Monitoring\\NWQI Field Data 2015\\NWQI-git\\Data\\DataAnalysisWolman.csv', header=T, stringsAsFactors=FALSE)
RBS[RBS$Post.Percent.Sand.and.Fines..PCT_SAFN.=="23,81", "Post.Percent.Sand.and.Fines..PCT_SAFN."] <- 23.81

View(RBS)
View(Wolman)

# Null hypothesis: The post-implementation values are greater than the pre-implementation values (Mu>0). 
# alpha less than 0.05(?) rejects the null hypothesis, and we will have demonstrated a decrease in values over time. 
# na.omit is true
# graph options
boxplot(RBS$Pre.LRBS, as.numeric(RBS$Post.LRBS))
qqnorm(RBS$Post.LRBS-RBS$Pre.LRBS)
qqline(RBS$Post.LRBS-RBS$Pre.LRBS)
plot(RBS$Post.LRBS-RBS$Pre.LRBS,
     pch = 16,
     ylab="Difference (Post – Pre)"
     )
abline(0,0, col="blue", lwd=2)

#2005/06 and 2015 RBS LRBS values
t.test(RBS$Pre.LRBS, RBS$Post.LRBS, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  RBS$Pre.LRBS and RBS$Post.LRBS
# t = 1.1801, df = 11, p-value = 0.8686
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 0.3979918
# sample estimates:
#   mean of the differences 
# 0.1578175 
# Conclusion: Post-implementation LRBS values are greater than or equal to Pre-implementation LRBS values.

#2005/06 and 2015 RBS percent sand and fines values
t.test(RBS$Pre.Percent.Sand.and.Fines..PCT_SAFN., as.numeric(RBS$Post.Percent.Sand.and.Fines..PCT_SAFN.), paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  RBS$Pre.Percent.Sand.and.Fines..PCT_SAFN. and as.numeric(RBS$Post.Percent.Sand.and.Fines..PCT_SAFN.)
# t = -0.17097, df = 11, p-value = 0.4337
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 4.558904
# sample estimates:
#   mean of the differences 
# -0.4796667 

#2005/06 and 2015 RBS percent sand values
t.test(RBS$Pre.Percent.Sand..PCT_SA., RBS$Post.Percent.Sand..PCT_SA., paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  RBS$Pre.Percent.Sand..PCT_SA. and RBS$Post.Percent.Sand..PCT_SA.
# t = -0.78705, df = 4, p-value = 0.2376
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 5.724318
# sample estimates:
#   mean of the differences 
# -3.3502 

#2005/06 and 2015 RBS percent fines values
t.test(RBS$Pre.Percent.Fines..PCT_FN., RBS$Post.Percent.Fines..PCT_FN., paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  RBS$Pre.Percent.Fines..PCT_FN. and RBS$Post.Percent.Fines..PCT_FN.
# t = 1.0441, df = 4, p-value = 0.8223
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 16.71406
# sample estimates:
#   mean of the differences 
# 5.4948 

#1994 and 2015 Wolman D50 values
t.test(Wolman$Pre.1994.D50..mm, Wolman$Post.2015.D50..mm, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.1994.D50..mm and Wolman$Post.2015.D50..mm
# t = -2.5404, df = 3, p-value = 0.04232
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -0.2730085
# sample estimates:
#   mean of the differences 
# -3.7075 
# Conclusion: At 4 sites, D50 from Wolman pebble counts in 2015 D50 were less than D50 in 1994.

#1994 and 2015 Wolman percent less than 1 mm values
t.test(Wolman$Pre.1994.percent.less.1mm, Wolman$Post.2015.Percent.less.1mm, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.1994.percent.less.1mm and Wolman$Post.2015.Percent.less.1mm
# t = 3.5614, df = 3, p-value = 0.9811
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 22.26291
# sample estimates:
#   mean of the differences 
# 13.405 

#1994 and 2015 Wolman percent sand and fines values
t.test(Wolman$Pre.1994.percent.SAFN, Wolman$Post.2015.Percent.SAFN, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.1994.percent.SAFN and Wolman$Post.2015.Percent.SAFN
# t = 2.7951, df = 3, p-value = 0.9659
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 16.86315
# sample estimates:
#   mean of the differences 
# 9.155 

#1994 and 2015 Wolman percent gravel values
t.test(Wolman$Pre.1994.percent.gravel...fines, Wolman$Post.2015Percent.gravel...fines, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.1994.percent.gravel...fines and Wolman$Post.2015Percent.gravel...fines
# t = 2.4381, df = 3, p-value = 0.9537
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 12.09106
# sample estimates:
#   mean of the differences 
# 6.1525 

#2000 and 2015 Wolman D50 values
t.test(Wolman$Pre.2000.D50..mm, Wolman$Post.2015.D50..mm, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.2000.D50..mm and Wolman$Post.2015.D50..mm
# t = -0.53835, df = 10, p-value = 0.3011
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 5.383153
# sample estimates:
#   mean of the differences 
# -2.274545 

#2000 and 2015 Wolman percent less than 1 mm values
t.test(Wolman$Pre.2000.Percent.Less.1mm, Wolman$Post.2015.Percent.less.1mm, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.2000.Percent.Less.1mm and Wolman$Post.2015.Percent.less.1mm
# t = 0.088766, df = 10, p-value = 0.5345
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 7.59378
# sample estimates:
#   mean of the differences 
# 0.3545455 

#2000 and 2015 Wolman percent sand and fines values
t.test(Wolman$Pre.2000.PercentSAFN, Wolman$Post.2015.Percent.SAFN, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.2000.PercentSAFN and Wolman$Post.2015.Percent.SAFN
# t = -0.80785, df = 10, p-value = 0.219
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 3.38588
# sample estimates:
#   mean of the differences 
# -2.722727 

#2000 and 2015 Wolman percent gravel values
t.test(Wolman$Pre.2000.Percent.Gravel...fines, Wolman$Post.2015Percent.gravel...fines, paired=TRUE, alternative="less")
# Paired t-test
# 
# data:  Wolman$Pre.2000.Percent.Gravel...fines and Wolman$Post.2015Percent.gravel...fines
# t = -0.56704, df = 10, p-value = 0.2916
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 4.594405
# sample estimates:
#   mean of the differences 
# -2.091818 

##################################################################################
#Are any trends getting worse?

#2005/06 and 2015 RBS LRBS values
t.test(RBS$Pre.LRBS, RBS$Post.LRBS, paired=TRUE, alternative="greater")

#2005/06 and 2015 RBS percent sand and fines values
t.test(RBS$Pre.Percent.Sand.and.Fines..PCT_SAFN., as.numeric(RBS$Post.Percent.Sand.and.Fines..PCT_SAFN.), paired=TRUE, alternative="greater")

#2005/06 and 2015 RBS percent sand values
t.test(RBS$Pre.Percent.Sand..PCT_SA., RBS$Post.Percent.Sand..PCT_SA., paired=TRUE, alternative="greater")

#2005/06 and 2015 RBS percent fines values
t.test(RBS$Pre.Percent.Fines..PCT_FN., RBS$Post.Percent.Fines..PCT_FN., paired=TRUE, alternative="greater")

#1994 and 2015 Wolman D50 values
t.test(Wolman$Pre.1994.D50..mm, Wolman$Post.2015.D50..mm, paired=TRUE, alternative="greater")

#1994 and 2015 Wolman percent less than 1 mm values
t.test(Wolman$Pre.1994.percent.less.1mm, Wolman$Post.2015.Percent.less.1mm, paired=TRUE, alternative="greater")
# Paired t-test
# 
# data:  Wolman$Pre.1994.percent.less.1mm and Wolman$Post.2015.Percent.less.1mm
# t = 3.5614, df = 3, p-value = 0.01889
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   4.547092      Inf
# sample estimates:
#   mean of the differences 
# 13.405 
# Conclusion: At 4 sites, Wolman pebble counts <1mm in 2015  were greater than Wolman pebble counts <1mm in 1994.


#1994 and 2015 Wolman percent sand and fines values
t.test(Wolman$Pre.1994.percent.SAFN, Wolman$Post.2015.Percent.SAFN, paired=TRUE, alternative="greater")
# Paired t-test
# 
# data:  Wolman$Pre.1994.percent.SAFN and Wolman$Post.2015.Percent.SAFN
# t = 2.7951, df = 3, p-value = 0.03406
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   1.446849      Inf
# sample estimates:
#   mean of the differences 
# 9.155 
# Conclusion: At 4 sites, Wolman pebble counts percent SAFN in 2015  were greater than Wolman pebble counts percent SAFN in 1994.

#1994 and 2015 Wolman percent gravel values
t.test(Wolman$Pre.1994.percent.gravel...fines, Wolman$Post.2015Percent.gravel...fines, paired=TRUE, alternative="greater")
# Paired t-test
# 
# data:  Wolman$Pre.1994.percent.gravel...fines and Wolman$Post.2015Percent.gravel...fines
# t = 2.4381, df = 3, p-value = 0.04633
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   0.2139409       Inf
# sample estimates:
#   mean of the differences 
# 6.1525 
# Conclusion: At 4 sites, Wolman pebble counts percent gravel in 2015  were greater than Wolman pebble counts percent gravel in 1994.

#2000 and 2015 Wolman D50 values
t.test(Wolman$Pre.2000.D50..mm, Wolman$Post.2015.D50..mm, paired=TRUE, alternative="greater")

#2000 and 2015 Wolman percent less than 1 mm values
t.test(Wolman$Pre.2000.Percent.Less.1mm, Wolman$Post.2015.Percent.less.1mm, paired=TRUE, alternative="greater")

#2000 and 2015 Wolman percent sand and fines values
t.test(Wolman$Pre.2000.PercentSAFN, Wolman$Post.2015.Percent.SAFN, paired=TRUE, alternative="greater")

#2000 and 2015 Wolman percent gravel values
t.test(Wolman$Pre.2000.Percent.Gravel...fines, Wolman$Post.2015Percent.gravel...fines, paired=TRUE, alternative="greater")

##################################################################################
#Delete the RBS notes with Google slope values 

RBS.slope.adj <- RBS[RBS$Post.Notes == "",]

#2005/06 and 2015 RBS LRBS values
t.test(RBS.slope.adj$Pre.LRBS, RBS.slope.adj$Post.LRBS, paired=TRUE, alternative="less")
t.test(RBS.slope.adj$Pre.LRBS, RBS.slope.adj$Post.LRBS, paired=TRUE, alternative="greater")
