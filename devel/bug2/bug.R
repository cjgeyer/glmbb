
 packageVersion("glmbb")

 options(width = 132)

 library(CatDataAnalysis)
 data(exercise_8.28)
 sapply(exercise_8.28, class)

 library(glmbb)
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

 # Just by rearranging the formulas, every model is listed twice,
 # which makes the weights half what they should be

