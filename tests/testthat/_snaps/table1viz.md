# invalid type overrides give clear errors

    Code
      table1viz(d, type = list("continuous"))
    Condition
      Error:
      ! `type` must be a named list with unique variable names.

---

    Code
      table1viz(d, type = list(x = "continuous", x = "categorical"))
    Condition
      Error:
      ! `type` must be a named list with unique variable names.

---

    Code
      table1viz(d, type = list(absent = "continuous"))
    Condition
      Error:
      ! `type` names must identify non-grouping variables in `d`.

---

    Code
      table1viz(d, "grp", type = list(grp = "categorical"))
    Condition
      Error:
      ! `type` names must identify non-grouping variables in `d`.

---

    Code
      table1viz(d, type = list(x = "invalid"))
    Condition
      Error:
      ! `type` values must be "continuous" or "categorical".

---

    Code
      table1viz(d, type = list(text = "continuous"))
    Condition
      Error:
      ! Variables overridden as "continuous" must be numeric.

