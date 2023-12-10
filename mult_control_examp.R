# PSYC 4330 - Seminar in Statistics
# Example - Multiplicity Control

# For this example we are using data from the
# 2019-2020 NHL Hockey Season

# We will use alpha = .10

d <- read.csv(file.choose())
names(d)

# 1) Pairwise Comparisons with a Single 
# Categorical Variable

# Difference in Penalty Minutes (PIM) Between Players
# of different Positions (Pos)

# Note: What you see are "adjusted p values"
# You compare these direclty against alpha

d$Pos <- factor(d$Pos)

tapply(d$PIM, d$Pos, mean)

# No Multiplicity Control
pairwise.t.test(x = d$PIM, g = d$Pos, 
                p.adjust.method = "none")
# Bonferroni FWE Multiplicity Control
pairwise.t.test(x = d$PIM, g = d$Pos, 
                p.adjust.method = "bonferroni")
# Holm FWE Multiplicity Control
pairwise.t.test(x = d$PIM, g = d$Pos, 
                p.adjust.method = "holm")
# Benjamini-Hochberg FDR Multiplicity Control
pairwise.t.test(x = d$PIM, g = d$Pos, 
                p.adjust.method = "fdr")



# 2) Pairwise Comparisons on a Single Categorical
# Variable, controlling for another variable(s)

# Difference in Penalty Minutes (PIM) Between Players
# of different Positions (Pos), controlling for Time
# on the Ice (TOI)

library(emmeans)
m <- lm(PIM ~ Pos + TOI, data = d)
summary(m)
mns <- emmeans(m, "Pos")
pairs(mns, adjust="none")
pairs(mns, adjust="bonferroni")
pairs(mns, adjust="holm")
pairs(mns, adjust="fdr")


# 3) Control Over a Mix of Effects

# Tests = Pairwise Comparisons for Pos + Shots (Continuous) + 
# Shoots (Dichotomous) + TOI (continuous)
m2 <- lm(PIM ~ Pos + Shots + Shoots + TOI, data = d)
summary(m2)
mns <- emmeans(m2, "Pos")
pairs(mns, adjust="none")
pvals<-c(.001967, .924387,.69487,.0112, .0413, 
         .2051, .4150, .2339, .6371)
pvals
names(pvals) <- c("Shots", "Shoots", "TOI", "C-D","C-L","C-R","D-L","D-R","L-R")
p.adjust(pvals, method = "none")
p.adjust(pvals, method = "bonferroni")
p.adjust(pvals, method = "holm")
p.adjust(pvals, method = "fdr")

# With no multiplicity control, Shots, C-D and C-L were 
# statistically significant

# With Bonferroni FWE multiplicity control, only Shots was significant

# With Holm multiplicity control, Shots and C-D were significant

# With FDR multiplicity control, Shots and C-D were significant
