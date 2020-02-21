#Task06 ec

model4 <- coal_model(c(15, 19), 5) + 
  feat_migration(5, pop_from = 1, pop_to = 2, symmetric = FALSE, time = 0.5) + feat_mutation(10) + 
  feat_recombination(10) + 
  feat_selection(strength_AA = 4, strength_Aa = 2, strength_aa = 6, strength_A = NULL, population = "all", time = par_expr(5), start = TRUE, start_frequency = 5e-04, Ne = 10000, position = 0.5, force_keep = TRUE, locus_group = "all") + 
  sumstat_nucleotide_div(name = "pi", population = 2, transformation = identity)

check_model (model4)
simulate(model4)

?sumstat_sfs()
?coal_model()

