# quantity() records a standard unit

    Code
      quantity(1, 2)
    Condition
      Error in `quantity()`:
      ! `unit` must be a single string, not the number 2.
    Code
      quantity("a", "mL")
    Condition
      Error in `quantity()`:
      ! Can't convert `x` <character> to <double>.
    Code
      quantity_unit(1)
    Condition
      Error in `quantity_unit()`:
      ! `x` must be numbers with a unit, not the number 1.

# quantity_convert() converts units of the same kind

    Code
      quantity_convert(quantity(1, "mL"), "s")
    Condition
      Error in `quantity_convert()`:
      ! Can't convert "mL" to "s".
    Code
      quantity_convert(quantity(1, "furlongs"), "mL")
    Condition
      Error in `quantity_convert()`:
      ! Can't convert "furlongs" to "mL".

# quantities compare and do arithmetic with numbers

    Code
      x * x
    Condition
      Error in `vec_arith()`:
      ! <quantity<mL>> * <quantity<mL>> is not permitted
    Code
      x + quantity(1, "s")
    Condition
      Error in `vec_arith()`:
      ! Can't convert `y` <quantity<s>> to <quantity<mL>>.

# combining quantities converts to the unit of the first

    Code
      vctrs::vec_c(psi, quantity(1, "mL"))
    Condition
      Error in `vctrs::vec_c()`:
      ! Can't combine `..1` <quantity<psi>> and `..2` <quantity<mL>>.

# quantities print and plot with their unit

    Code
      dat$volume
    Output
      <quantity<mL>[2]>
      [1] 1.0 2.5
    Code
      dat
    Output
      # A tibble: 2 x 1
        volume
          <mL>
      1    1  
      2    2.5

