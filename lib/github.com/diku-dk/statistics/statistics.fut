-- | Statistics functions parameterised over the concrete
-- | representation of reals.

import "/futlib/math"

module type statistics = {
  type t
  -- | `mean vs` returns the mean of the values contained in `vs`.
  val mean : []t -> t
  -- | `stddev vs` returns the standard deviation of the values contained in `vs`.
  val stddev : []t -> t
  -- | `variance vs` returns the variance of the values contained in `vs`.
  val variance : []t -> t
}

module statistics (R: real) : statistics with t = R.t = {
  type t = R.t

  let mean [n] (vs: [n]t) : t =
    if n == 0 then R.i32 0 else R.(sum vs / i32 n)

  let sq (x:t) : t = R.(x*x)

  let variance [n] (vs: [n]t) : t =
    if n < 2 then R.i32 0
    else let m = mean vs
         let xs = map (\x -> R.(sq(x-m))) vs
         in (R.sum xs) R./ (R.i32(i32.(n-1)))

  let stddev vs =
    variance vs |> R.sqrt
}
