# absorbance() records the wavelength and unit

    Code
      absorbance(1, unit = "OD")
    Condition
      Error in `absorbance()`:
      ! `unit` must be one of "mAU" or "AU", not "OD".
    Code
      absorbance(1, wavelength = "280")
    Condition
      Error in `absorbance()`:
      ! `wavelength` must be a number or `NA`, not the string "280".
    Code
      absorbance_unit(1)
    Condition
      Error in `absorbance_unit()`:
      ! `x` must be absorbance, not the number 1.

# absorbance can't be assigned values at another wavelength

    Code
      x[1] <- absorbance(1, wavelength = 260)
    Condition
      Error in `[<-`:
      ! Can't convert `value` <absorbance<260 nm, mAU>> to <absorbance<280 nm, AU>>.

# absorbance does arithmetic

    Code
      x * a260
    Condition
      Error in `vec_arith()`:
      ! <absorbance<280 nm, AU>> * <absorbance<260 nm, mAU>> is not permitted

# absorbance prints with its unit

    Code
      x
    Output
      <absorbance<280 nm, AU>[2]>
      [1] 0.10 0.25
    Code
      absorbance(1)
    Output
      <absorbance<mAU>[1]>
      [1] 1
    Code
      tibble::tibble(a280 = x)
    Output
      # A tibble: 2 x 1
        a280
        <AU>
      1 0.1 
      2 0.25

