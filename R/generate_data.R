# generate_data.R
# Creates a synthetic A/B test dataset for a website checkout experiment.
# Group A = control, Group B = new checkout flow with a (true) small lift.
# Output: data/ab_test.csv

set.seed(42)

n_per_group <- 8000

# True underlying conversion rates (B has a real but modest lift).
p_control   <- 0.118
p_treatment <- 0.134

# Revenue per converting user ~ gamma; treatment nudges AOV up slightly too.
make_group <- function(group, n, p_conv, aov_shape, aov_scale) {
  converted <- rbinom(n, 1, p_conv)
  revenue <- ifelse(
    converted == 1,
    round(rgamma(n, shape = aov_shape, scale = aov_scale), 2),
    0
  )
  data.frame(
    user_id   = NA_integer_,
    group     = group,
    device    = sample(c("desktop", "mobile", "tablet"), n,
                       replace = TRUE, prob = c(0.5, 0.42, 0.08)),
    converted = converted,
    revenue   = revenue,
    stringsAsFactors = FALSE
  )
}

control   <- make_group("A", n_per_group, p_control,   aov_shape = 4, aov_scale = 14)
treatment <- make_group("B", n_per_group, p_treatment, aov_shape = 4, aov_scale = 15)

df <- rbind(control, treatment)
df <- df[sample(nrow(df)), ]
df$user_id <- seq_len(nrow(df))
rownames(df) <- NULL

dir.create("data", showWarnings = FALSE)
write.csv(df, "data/ab_test.csv", row.names = FALSE)

cat(sprintf("Wrote %d rows to data/ab_test.csv\n", nrow(df)))
cat(sprintf("Observed conversion  A: %.3f  B: %.3f\n",
            mean(df$converted[df$group == "A"]),
            mean(df$converted[df$group == "B"])))
