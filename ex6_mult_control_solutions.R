# PSYC 4330 - Seminar in Statistics
# Exercise 6 - Multiplicity Control


# With no multiplicity control, Grad_Prof scored 
# significantly lower than Coll_Univ and HS

# With Bonferroni FWE control, there were no
# statistically significant differences

# With Benjamini-Hochberg FDR control, Grad_Prof scored 
# significantly lower than Coll_Univ and HS


# 2) Control Over a Mix of Effects

m2 <- lm(Mean_Animal_reminder ~ Age + Gender + Education,
         data = d)
summary(m2)
m3 <- lm(Mean_Contamination ~ Age + Gender + Education,
         data = d)
summary(m3)
m4 <- lm(Mean_core ~ Age + Gender + Education,
   data = d)
summary(m4)
pvals<-c(summary(m2)$coefficients[2:4,4],
         summary(m3)$coefficients[2:4,4],
         summary(m4)$coefficients[2:4,4])
pvals
names(pvals) <- c("age_ar", "gender_ar", "educ_ar",
                  "age_con", "gender_con", "educ_con",
                  "age_cor", "gender_cor", "educ_cor")
p.adjust(pvals, method = "none")
p.adjust(pvals, method = "bonferroni")
p.adjust(pvals, method = "holm")
p.adjust(pvals, method = "fdr")

# With no multiplicity control, all effects were 
# statistically significant

# With Bonferroni FWE multiplicity control, all 
# effects were significant except educ(con), 
# age(cor), and educ(cor)

# With Holm multiplicity control, all effects were
# significant except educ(con)

# With FDR multiplicity control, all effects were
# significant except educ(con)


# Recall: we are just focusing on hypothesis testing/
# p-values right now, but don't forget about computing
# and interpreting the magnitude and precision of 
# relevant effect size measures in all situations