# enhance dataset

    Code
      enhance_dataset(data = dataset, data_to_add = new_data)
    Output
                 a new  b  c
      object_1   1   1 12 23
      object_2   2   2 13 24
      object_3   3   3 14 25
      object_4   4   4 15 26
      object_5   5   5 16 27
      object_6   6   6 17 28
      object_7   7   7 18 29
      object_8   8   8 19 30
      object_9   9   9 20 31
      object_10 10  10 21 32
      object_11 11  11 22 33

# add unique IDs (leading zeros)

    Code
      add_unique_ids(data = dataset)
    Output
          a  b  c
      01  1 12 23
      02  2 13 24
      03  3 14 25
      04  4 15 26
      05  5 16 27
      06  6 17 28
      07  7 18 29
      08  8 19 30
      09  9 20 31
      10 10 21 32
      11 11 22 33

# impute missing values

    Code
      impute_missing_values(data = dataset, method = "custom", custom_value = 4)
    Output
          a  b  c d
      1   1 11 21 1
      2   2 12 22 4
      3   3 13 23 1
      4   4 14 24 4
      5   5 15 25 1
      6   6 16 26 4
      7   7 17 27 1
      8   8 18 28 4
      9   9 19 29 1
      10 10 20 30 4

---

    Code
      impute_missing_values(data = dataset, method = "mean", custom_value = 4)
    Output
          a  b  c d
      1   1 11 21 1
      2   2 12 22 1
      3   3 13 23 1
      4   4 14 24 1
      5   5 15 25 1
      6   6 16 26 1
      7   7 17 27 1
      8   8 18 28 1
      9   9 19 29 1
      10 10 20 30 1

---

    Code
      impute_missing_values(data = dataset, method = "median", custom_value = 4)
    Output
          a  b  c d
      1   1 11 21 1
      2   2 12 22 1
      3   3 13 23 1
      4   4 14 24 1
      5   5 15 25 1
      6   6 16 26 1
      7   7 17 27 1
      8   8 18 28 1
      9   9 19 29 1
      10 10 20 30 1

# introduce dummies

    Code
      introduce_dummies(data = dataset, col_names = NULL, add_indicator = FALSE)
    Output
          a  b c_a c_b c_c c_d c_e
      1   1 11   1   0   0   0   0
      2   2 12   0   1   0   0   0
      3   3 13   0   0   1   0   0
      4   4 14   0   0   0   1   0
      5   5 15   0   0   0   0   1
      6   6 16   1   0   0   0   0
      7   7 17   0   1   0   0   0
      8   8 18   0   0   1   0   0
      9   9 19   0   0   0   1   0
      10 10 20   0   0   0   0   1

---

    Code
      introduce_dummies(data = dataset, col_names = NULL, add_indicator = FALSE)
    Output
          a  b c_a c_b c_c c_d c_e
      1   1 11   1   0   0   0   0
      2   2 12   0   1   0   0   0
      3   3 13   0   0   1   0   0
      4   4 14   0   0   0   1   0
      5   5 15   0   0   0   0   1
      6   6 16   1   0   0   0   0
      7   7 17   0   1   0   0   0
      8   8 18   0   0   1   0   0
      9   9 19   0   0   0   1   0
      10 10 20   0   0   0   0   1

---

    Code
      introduce_dummies(data = dataset, col_names = "c", add_indicator = TRUE)
    Output
          a  b c_a c_b c_c c_d c_e
      1   1 11   1   0   0   0   0
      2   2 12   0   1   0   0   0
      3   3 13   0   0   1   0   0
      4   4 14   0   0   0   1   0
      5   5 15   0   0   0   0   1
      6   6 16   1   0   0   0   0
      7   7 17   0   1   0   0   0
      8   8 18   0   0   1   0   0
      9   9 19   0   0   0   1   0
      10 10 20   0   0   0   0   1

---

    Code
      introduce_dummies(data = dataset_binary, col_names = "c", add_indicator = FALSE)
    Output
          a  b c
      1   1 11 1
      2   2 12 0
      3   3 13 1
      4   4 14 0
      5   5 15 1
      6   6 16 0
      7   7 17 1
      8   8 18 0
      9   9 19 1
      10 10 20 0

# introduce offset

    Code
      introduce_offset(data = dataset, new_min = 5, col_names = NULL)
    Output
         a b c
      1  8 5 1
      2  5 6 2
      3 10 8 3

---

    Code
      introduce_offset(data = dataset, new_min = 5, col_names = c("a"))
    Message <rlang_message>
      Note: Using an external vector in selections is ambiguous.
      i Use `all_of(col_names)` instead of `col_names` to silence this message.
      i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      This message is displayed once per session.
    Output
         a b c
      1  8 0 1
      2  5 1 2
      3 10 3 3

# scale / transformations

    Code
      scale_data(data = dataset, col_names = c("b", "c"), method = "sqrt",
      n_for_root = 3)
    Output
         a        b        c
      1  0 0.000000 1.000000
      2 -3 1.000000 1.414214
      3  2 1.732051 1.732051

---

    Code
      scale_data(data = dataset, col_names = c("b", "c"), method = "nth_root",
      n_for_root = 3)
    Output
         a       b        c
      1  0 0.00000 1.000000
      2 -3 1.00000 1.259921
      3  2 1.44225 1.442250

---

    Code
      scale_data(data = dataset, col_names = c("b", "c"), method = "normalize_to_max",
      n_for_root = 3)
    Output
         a         b         c
      1  0 0.0000000 0.3333333
      2 -3 0.3333333 0.6666667
      3  2 1.0000000 1.0000000

---

    Code
      scale_data(data = dataset, col_names = c("b", "c"), method = "min_max",
      n_for_root = 3)
    Output
         a         b   c
      1  0 0.0000000 0.0
      2 -3 0.3333333 0.5
      3  2 1.0000000 1.0

# unique combinations

    Code
      introduce_unique_combinations(data = dataset, method = "*", ignore_variables = NULL)
    Output
         a b c a_x_b a_x_c b_x_c
      1  0 0 1     0     0     0
      2 -3 1 2    -3    -6     2
      3  2 3 3     6     6     9

---

    Code
      introduce_unique_combinations(data = dataset, method = "/", ignore_variables = NULL)
    Output
         a b c      a_x_b      a_x_c b_x_c
      1  0 0 1        NaN  0.0000000   0.0
      2 -3 1 2 -3.0000000 -1.5000000   0.5
      3  2 3 3  0.6666667  0.6666667   1.0

---

    Code
      introduce_unique_combinations(data = dataset, method = "*", ignore_variables = "b")
    Output
         a b c a_x_c
      1  0 0 1     0
      2 -3 1 2    -6
      3  2 3 3     6

