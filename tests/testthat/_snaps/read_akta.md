# chrom_read_akta_csv() errors for other files

    Code
      chrom_read_akta_csv(path)
    Condition
      Error in `chrom_read_akta_csv()`:
      ! '<file>' isn't a UNICORN export of chromatogram curves.
      i Its first line should name the chromatogram of each curve, such as "Chrom.1".

# chrom_read_akta_result() errors for other files

    Code
      chrom_read_akta_result(path)
    Condition
      Error in `chrom_read_akta_result()`:
      ! '<file>' isn't a UNICORN result.
      i Results are '.zip' archives with a 'Result.xml' file.

# chrom_read_ngc_fractions() reads the fraction table

    Code
      chrom_read_ngc_fractions(path)
    Condition
      Error in `chrom_read_ngc_fractions()`:
      ! '<file>' isn't an NGC fraction table.
      i It should start with the run's name, then a table of fractions with a Rack/Tube column.

