-- | Statistics functions parameterised over the concrete
-- representation of reals.

module type statistics = {
  type t
  -- | `mean vs` returns the arithmetic mean of the values contained in `vs`.
  val mean : []t -> t
  -- | `gmean vs` returns the geometric mean of the values contained in `vs`.
  val gmean : []t -> t
  -- | `hmean vs` returns the harmonic mean of the values contained in `vs`.
  val hmean : []t -> t
  -- | `qmean vs` returns the quadratic mean of the values contained
  -- in `vs`.  Also known as "root mean square".
  val qmean : []t -> t
  -- | `variance vs` returns the variance of the values contained in
  -- `vs`. The variance is the square of the standard deviation.
  val variance : []t -> t
  -- | `stddev vs` returns the standard deviation of the values
  -- contained in `vs`. The standard deviation is the square root of
  -- the variance.
  val stddev : []t -> t

  --- RANK STATISTICS

  -- | Median value of array.
  val median [n]: [n]t -> t
  -- | Median value of sorted array.
  val median_sorted [n]: [n]t -> t
  -- | Quantile of array.
  val quantile [n]: [n]t -> t -> t
  -- | Quantile of sorted array.
  val quantile_sorted [n]: [n]t -> t -> t
  -- | The most frequently occuring element of an array.
  val mode [n]: [n]t -> t
  -- | The most frequently occuring element of a sorted array.
  val mode_sorted [n]: [n]t -> t

  -- | `skewness vs` returns the skewness of the values contained in
  -- `vs`. The skewness measures the assymetry of the values in
  -- `vs`. If the skewness is positive, the upper tail is thicker than
  -- the lower tail, whereas, if the skewness is negative, the lower
  -- tail is thicker than the upper tail. The skewness of a set of
  -- normally distributed values is zero.
  val skewness : []t -> t
}

module mk_statistics (R: float) : statistics with t = R.t = {
  type t = R.t

  import "../sorts/radix_sort"
  import "../segmented/segmented"

  let argmax (none: t) (xs: []t): i32 =
    let max (x1, y1) (x2, y2) =
      if R.(y1 < y2) then (x2, y2) else (x1, y1)
    in reduce max (-1, none) (zip (iota (length xs)) xs) |> (.1)

  let mean [n] (vs: [n]t) : t =
    R.(sum vs / i32 n)

  let gmean [n] (xs: [n]t) : t =
    R.(product xs ** (i32 1/i32 n))

  let hmean [n] (xs: [n]t) : t =
    R.(i32 n / sum (map (i32 1/) xs))

  let qmean [n] (xs: [n]t) : t =
    R.((sum(map (**i32 2) xs)/i32 n)**(i32 1/i32 2))

  let sq (x:t) : t = R.(x*x)
  let cube (x:t) : t = R.(x*x*x)

  let variance [n] (vs: [n]t) : t =
    let m = mean vs
    let xs = map (\x -> R.(sq(x-m))) vs
    in (R.sum xs) R./ (R.i32(i32.(n-1)))

  let stddev vs =
    variance vs |> R.sqrt

  let skewness [n] (vs: [n]t) : t =
    let m = mean vs
    let s = stddev vs
    let xs = map (\x -> R.(cube((x-m)/s))) vs
    in (R.sum xs) R./ (R.i32 n)

  let median_sorted [n] (xs: [n]t) : t =
    let i = n/2
    let j = i-1
    in if n % 2 == 0 then R.((xs[j]+xs[i]) / (i32 2) )
       else xs[i]

  let median = radix_sort_float R.num_bits R.get_bit >-> median_sorted

  let quantile_sorted [n] (xs: [n]t) (p: t) : t =
    let (alphap, betap) = (R.f32 0.4, R.f32 0.4) -- Default in SciPy.
    let m = R.(alphap + p*(i32 1 - alphap - betap))
    let aleph = R.(i32 n*p + m)
    let k = i32.max 1 (i32.min (n-1) (R.to_i32 aleph))
    let gamma = R.(aleph-i32 k) |> R.min (R.i32 1) |> R.max (R.i32 0)
    in R.(i32 1-gamma) R.* xs[k-1] R.+ gamma R.* xs[k]

  let quantile = radix_sort_float R.num_bits R.get_bit >-> quantile_sorted

  let mode_sorted [n] (xs: [n]t) : t =
    let xs_rotated = rotate (n-1) xs
    let xs_zip = zip xs xs_rotated
    let flags = map2 (R.!=) xs xs_rotated
    let vals = replicate n (R.i32 1)
    let ys = segmented_scan (R.+) (R.i32 0) flags vals
    let i = argmax (R.i32 0) ys
    in xs[i]

  let mode = radix_sort_float R.num_bits R.get_bit >-> mode_sorted
}
