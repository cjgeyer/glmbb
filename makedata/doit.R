
# requires file all.txt, which is http://www.stat.ufl.edu/~aa/cda/data.html
# saved as plain text

foo <- system("grep -n \"^\\*[0-9]\" all.txt", intern = TRUE)
foo

# weird! note that there are 2 *22 (the second being at position 26)
# but these are different data sets
foo.lineno <- as.numeric(sub(":.*$", "", foo))
foo.lineno

# do section 1
idx <- 1
bar <- scan("all.txt", skip = foo.lineno[idx], what = character(0),
    sep = "\n", blank.lines.skip = FALSE,
    nlines = foo.lineno[idx + 1] - foo.lineno[idx] - 1)
bar
inies <- grepl("^[[:alnum:]. ]+$", bar)
inies
stopifnot(any(inies))
skip.begin <- min(which(inies)) - 1
skip.end <- length(inies) - max(which(inies))
bar <- scan("all.txt", skip = foo.lineno[idx] + skip.begin,
    what = character(0), blank.lines.skip = FALSE,
    nlines = foo.lineno[idx + 1] - foo.lineno[idx] - 1 - skip.begin - skip.end)
bar
vars <- grepl("[a-z]", bar)
vars
colnames <- bar[vars]
bar <- bar[! vars]
bar <- as.numeric(bar)
bar <- matrix(bar, byrow = TRUE, ncol = length(colnames))
colnames(bar) <- colnames
crabs <- as.data.frame(bar)
crabs

# need to convert color and spline to factor
color <- crabs$color
spine <- crabs$spine
sort(unique(color))
sort(unique(spine))
# See explaination in table caption, the numbers are 2 through 5
# so "foo" is not a real level
color <- c("foo", "light", "medium", "dark", "darker")[color]
spine <- c("good", "middle", "bad")[spine]
crabs$color <- as.factor(color)
crabs$spine <- as.factor(spine)
any(is.na(crabs))
levels(crabs$color)
levels(crabs$spine)

write.table(crabs, file = "../package/glmbb/data/crabs.txt",
    quote = FALSE, row.names = FALSE)
