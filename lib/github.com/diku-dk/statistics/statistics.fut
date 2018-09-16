-- | Statistics functions parameterised over the concrete
-- | representation of reals.

import "/futlib/math"

module type statistics = {
  type t
  -- | `mean vs` returns the mean of the values contained in `vs`.
  val mean : []t -> t
  -- | `variance vs` returns the variance of the values contained in
  -- | `vs`. The variance is the square of the standard deviation.
  val variance : []t -> t
  -- | `stddev vs` returns the standard deviation of the values
  -- | contained in `vs`. The standard deviation is the square root of
  -- | the variance.
  val stddev : []t -> t
  -- | `skewness vs` returns the skewness of the values contained in
  -- | `vs`. The skewness measures the assymetry of the values in
  -- | `vs`. If the skewness is positive, the upper tail is thicker
  -- | than the lower tail, whereas, if the skewness is negative, the
  -- | lower tail is thicker than the upper tail. The skewness of a
  -- | set of normally distributed values is zero.
  val skewness : []t -> t
}

module statistics (R: real) : statistics with t = R.t = {
  type t = R.t

  let mean [n] (vs: [n]t) : t =
    if n == 0 then R.i32 0 else R.(sum vs / i32 n)

  let sq (x:t) : t = R.(x*x)
  let cube (x:t) : t = R.(x*x*x)

  let variance [n] (vs: [n]t) : t =
    if n < 2 then R.i32 0
    else let m = mean vs
         let xs = map (\x -> R.(sq(x-m))) vs
         in (R.sum xs) R./ (R.i32(i32.(n-1)))

  let stddev vs =
    variance vs |> R.sqrt

  let skewness [n] (vs: [n]t) : t =
    if n < 2 then R.i32 0
    else let m = mean vs
         let s = stddev vs
         let xs = map (\x -> R.(cube((x-m)/s))) vs
         in (R.sum xs) R./ (R.i32 n)

}
