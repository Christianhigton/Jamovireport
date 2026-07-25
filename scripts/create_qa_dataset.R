#!/usr/bin/env Rscript

set.seed(20260725)
n <- 72L
group <- factor(rep(c("Control", "Treatment"), times = c(30L, 42L)))
sex <- factor(rep(c("Female", "Male", "Non-binary"), length.out = n))
age <- round(rnorm(n, 34, 9), 1)
baseline <- rnorm(n, 50, ifelse(group == "Control", 6, 11))
mid <- baseline + rnorm(n, 2, 4) + ifelse(group == "Treatment", 2, 0)
post <- baseline + rnorm(n, 4, 4) + ifelse(group == "Treatment", 5, 0)
skewed <- round(rexp(n, rate = 0.12), 2)
binary <- factor(ifelse(
    plogis(-1 + 0.05 * (age - 34) + 0.6 * (group == "Treatment")) >
        runif(n),
    "Yes", "No"
))
category3 <- factor(rep(c("Low", "Medium", "High"), length.out = n))

audit <- data.frame(
    id = seq_len(n),
    group = group,
    sex = sex,
    age = age,
    baseline = round(baseline, 2),
    mid = round(mid, 2),
    post = round(post, 2),
    skewed_outcome = skewed,
    binary_outcome = binary,
    category_three = category3,
    constant = 1,
    predictor = round(age + rnorm(n, 0, 2), 2),
    predictor_collinear = NA_real_,
    stringsAsFactors = FALSE
)
audit$predictor_collinear <- audit$predictor * 2
audit$baseline[c(4, 19, 55)] <- NA
audit$post[c(8, 19, 63)] <- NA
audit$category_three[c(12, 48)] <- NA

dir.create("qa", showWarnings = FALSE)
utils::write.csv(audit, "qa/jreport-audit-data.csv", row.names = FALSE, na = "")
cat("Wrote qa/jreport-audit-data.csv with", nrow(audit), "rows.\n")
