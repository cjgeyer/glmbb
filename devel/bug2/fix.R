
 library(glmbb, lib.loc = "../../package/glmbb.Rcheck")
 packageVersion("glmbb")

 options(width = 132)

 library(CatDataAnalysis)
 data(exercise_8.28)
 sapply(exercise_8.28, class)

 out <- glmbb(counts ~ Contact * Influence * Housing * Satisfaction,
    little = ~ Contact * Influence * Housing + Satisfaction,
    data = exercise_8.28)
 summary(out)
 
 # above is correct AFAIK
 # now for the bug

 bug.out <- glmbb(counts ~ Satisfaction * Contact * Influence * Housing,
    little = ~ Satisfaction + Contact * Influence * Housing,
    data = exercise_8.28)
 summary(bug.out)

 # Fixed!
