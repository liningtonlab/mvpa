# optimal number of components

    Code
      optimal_nr_components(cost_func_vals = data.frame(a = c(12:22), b = c(23:33),
      c = c(1:11)), validation_threshold = 0.5)
    Output
      $A_optimal
      [1] 2
      
      $fractions
      [1] 1 0
      

---

    Code
      optimal_nr_components(cost_func_vals = data.frame(a = c(12:21), b = c(rep(4, 4),
      rep(4, 6)), b = c(rep(3, 6), rep(4, 4))), validation_threshold = 0.5)
    Output
      $A_optimal
      [1] 2
      
      $fractions
      [1] 0.0 0.4
      

