#National Water Quality Initiative 
#Fifteenmile Sediment 
#pre and post BMP implementation comparison
#Julia Crown May 2016

# Create new directory
dir.create(paste("\\\\Deqhq1\\TMDL\\NWQI\\Fifteenmile\\2015 Monitoring\\NWQI Field Data 2015\\",Sys.Date(), sep="")) 

# Load data file 
RBS <- read.csv('\\\\Deqhq1\\TMDL\\NWQI\\Fifteenmile\\2015 Monitoring\\NWQI Field Data 2015\\DataAnalysisRBS.csv', header=T, stringsAsFactors=FALSE)
Wolman <- read.csv('\\\\Deqhq1\\TMDL\\NWQI\\Fifteenmile\\2015 Monitoring\\NWQI Field Data 2015\\DataAnalysisWolman.csv', header=T, stringsAsFactors=FALSE)
RBS[RBS$Post.Percent.Sand.and.Fines..PCT_SAFN.=="23,81", "Post.Percent.Sand.and.Fines..PCT_SAFN."] <- 23.81

View(RBS)
View(Wolman)

# Null hypothesis: The pre-implementation values are greater than the post-implementation values. 
# alpha less than 0.05(?) rejects the null hypothesis, and we will have demonstrated a decrease in values over time. 
# na.omit is true
boxplot(RBS$Pre.LRBS, as.numeric(RBS$Post.LRBS))
qqnorm(RBS$Post.LRBS-RBS$Pre.LRBS)
qqline(RBS$Post.LRBS-RBS$Pre.LRBS)
plot(RBS$Post.LRBS-RBS$Pre.LRBS,
     pch = 16,
     ylab="Difference (Post – Pre)"
     )
abline(0,0, col="blue", lwd=2)
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

boxplot(RBS$Pre.Percent.Sand.and.Fines..PCT_SAFN., as.numeric(RBS$Post.Percent.Sand.and.Fines..PCT_SAFN.))
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
# Conclusion: Post-implementation values are greater than or equal to Pre-implementation values.

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
t.test(Wolman$Pre.1994.percent.less.1mm, Wolman$Post.2015.Percent.less.1mm, paired=TRUE, alternative="less")
t.test(Wolman$Pre.1994.percent.SAFN, Wolman$Post.2015.Percent.SAFN, paired=TRUE, alternative="less")
t.test(Wolman$Pre.1994.percent.gravel...fines, Wolman$Post.2015Percent.gravel...fines, paired=TRUE, alternative="less")

t.test(Wolman$Pre.2000.D50..mm, Wolman$Post.2015.D50..mm, paired=TRUE, alternative="less")
t.test(Wolman$Pre.2000.Percent.Less.1mm, Wolman$Post.2015.Percent.less.1mm, paired=TRUE, alternative="less")
t.test(Wolman$Pre.2000.PercentSAFN, Wolman$Post.2015.Percent.SAFN, paired=TRUE, alternative="less")
t.test(Wolman$Pre.2000.Percent.Gravel...fines, Wolman$Post.2015Percent.gravel...fines, paired=TRUE, alternative="less")

#