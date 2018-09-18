-- | Statistics functions parameterised over the concrete
-- representation of reals.

import "gammaln"

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

  -- | `variance vs` returns the sample variance of the values
  -- contained in `vs`. The sample variance is the square of the
  -- sample standard deviation.
  val variance : []t -> t

  -- | `stddev vs` returns the sample standard deviation of the values
  -- contained in `vs`. The sample standard deviation is the square
  -- root of the sample variance.
  val stddev : []t -> t

  -- | `variance_pop vs` returns the population variance of the values
  -- contained in `vs`. The population variance is the square of the
  -- population standard deviation.
  val variance_pop : []t -> t

  -- | `stddev_pop vs` returns the population standard deviation of
  -- the values contained in `vs`. The population standard deviation
  -- is the square root of the population variance.
  val stddev_pop : []t -> t

  -- | `skewness vs` returns the skewness of the values contained in
  -- `vs`. The skewness measures the assymetry of the values in
  -- `vs`. If the skewness is positive, the upper tail is thicker than
  -- the lower tail, whereas, if the skewness is negative, the lower
  -- tail is thicker than the upper tail. The skewness of a set of
  -- normally distributed values is zero.
  val skewness : []t -> t

  -- | `skewness_adj vs` returns the adjusted Fisher-Pearson coefficient of
  -- skewness for the values contained in `vs`.
  val skewness_adj : []t -> t

  -- | `kurtosis vs` returns the (non-excess) kurtosis of the values
  -- contained in `vs`.
  val kurtosis : []t -> t

  -- | `kurtosis_excess vs` returns the excess kurtosis of the values
  -- contained in `vs`.
  val kurtosis_excess : []t -> t

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

  -- | `erf x` returns a polynomial approximation to the Gauss error
  -- function applied to `x`. The maximal approximation error is
  -- 0.00000012 for any argument `x`.
  val erf : t -> t

  -- | `gamma x` returns the value `(x-1)!` for positive integer
  -- values `x`. Extended to work for positive non-integer values.
  val gamma : t -> t

  -- | `gammaln x` returns `ln((x-1)!)`, extended to work with
  -- positive non-integer values. Notice that `gammaln` is numerically
  -- stable in contrast to calculating `log(gamma x)`.
  val gammaln : t -> t

  -- | Generic type for distributions. Discrete distributions have
  -- type `dist i32`, whereas continues distributions have type
  -- `dist t`.
  type^ dist 'a

  val mk_poison : {lambda:t} -> dist i32
  val mk_normal : {mu:t,sigma:t} -> dist t

  val pmf : dist i32 -> i32 -> t       -- probability mass function
  val pdf : dist t -> t -> t           -- probability distribution function
  val cdf 'a : dist a -> a -> t        -- commulative
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

  let pow2 (x:t) : t = R.(x*x)
  let pow3 (x:t) : t = R.(x*x*x)
  let pow4 (x:t) : t = pow2(pow2 x)

  let variance [n] (vs: [n]t) : t =
    let m = mean vs
    let xs = map (\x -> R.(pow2(x-m))) vs
    in (R.sum xs) R./ (R.i32(i32.(n-1)))

  let stddev vs =
    variance vs |> R.sqrt

  let variance_pop [n] (vs: [n]t) : t =
    R.((i32 n - i32 1) / i32 n * variance vs)

  let stddev_pop vs =
    variance_pop vs |> R.sqrt

  let skewness [n] (vs: [n]t) : t =
    R.(let m = mean vs
       let s = stddev_pop vs
       let xs = map (\x -> pow3((x-m)/s)) vs
       in sum xs / i32 n)

  let skewness_adj [n] (vs: [n]t) : t =
    R.(sqrt(i32 n * (i32 n - i32 1)) / (i32 n - i32 2) * skewness vs)

  let kurtosis [n] (vs: [n]t) : t =
    R.(let m = mean vs
       let s = stddev_pop vs
       let xs = map (\x -> pow4((x-m)/s)) vs
       in sum xs / i32 n)

  let kurtosis_excess [n] (vs: [n]t) : t =
    R.(kurtosis vs - i32 3)

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

  -- The Gamma function
  let gamma_big (z:t) : t =  -- z >= 0.5
    R.(let p = [f64 676.5203681218851,
                negate(f64 1259.1392167224028),
                f64 771.32342877765313,
                negate (f64 176.61502916214059),
                f64 12.507343278686905,
                negate (f64 0.13857109526572012),
                f64 9.9843695780195716e-6,
                f64 1.5056327351493116e-7]
       in let z = z - i32 1
          let x = f64 0.99999999999980993
          let x = x + reduce (+) (i32 0) (map2 (\i pval ->
                                                pval / (z+i32 i+i32 1)) (iota(length p)) p)
          let t = z + i32(length p) - f64 0.5
          in sqrt(i32 2 * pi) * t**(z+f64 0.5) * exp(negate t) * x)

  let gamma (z:t) : t =
    R.(if z < f64 0.5 then
         pi / (sin(pi*z) * gamma_big (i32 1 - z)) --Reflection formula
       else gamma_big z)

  module Gammaln = mk_gammaln R

  let gammaln (x:t) : t = Gammaln.gammaln x

  -- DISTRIBUTIONS
  type dist 'a = {pXf:a -> t,cdf:a -> t}

  let poison_pmf (lambda:t) (x:i32) : t =
    R.(exp (i32 x * log lambda - lambda - gammaln (i32 x + i32 1) ))

  let fac n =
    R.(reduce (*) (i32 1) (map (\x -> i32 x + i32 1) (iota n)))

  let poison_cdf (lambda:t) (x:i32) : t =
    R.(exp (negate(lambda)) * reduce (+) (i32 0) (map (\i -> lambda ** (i32 i) / fac i) (iota x)))

  let normal_pdf (sigma:t) (mu:t) (x:t) : t =
    R.(exp(negate(pow2(x-mu) / (i32 2 * pow2 sigma))) / sqrt(i32 2 * pi * pow2 sigma))

  let erf (x:t) : t =
    R.(let t = i32 1 / (i32 1 + (abs x / i32 2))
       let t2 = t*t
       let t3 = t*t2
       let t4 = t2*t2
       let t5 = t3*t2
       let t6 = t3*t3
       let t7 = t3*t4
       let t8 = t4*t4
       let t9 = t4*t5
       let tau = t * exp(i32 0 - pow2 x - f64 1.26551223 + f64 1.00002368*t + f64 0.37409196*t2
                         + f64 0.09678418*t3 - f64 0.18628806*t4 + f64 0.27886807*t5 - f64 1.13520398*t6
                         + f64 1.48851587*t7 - f64 0.82215223*t8 + f64 0.17087277*t9)
       in if x >= i32 0 then i32 1 - tau
          else tau - i32 1)

  let normal_cdf (sigma:t) (mu:t) (x:t) : t =
    R.((i32 1 + erf((x-mu)/(sigma * sqrt(i32 2)))) / i32 2)

  let mk_poison {lambda:t} : dist i32 =
    {pXf=poison_pmf lambda,cdf=poison_cdf lambda}

  let mk_normal {sigma:t,mu:t} : dist t =
    {pXf=normal_pdf sigma mu,cdf=normal_cdf sigma mu}

  let pmf ({pXf,cdf} : dist i32) x = pXf x
  let pdf ({pXf,cdf} : dist t) x = pXf x
  let cdf {pXf,cdf} x = cdf x

  -- Cumulative Normal Distribution Function; J.C.Hull, Section 13.9 *)
  let cum_norm_dist_pos x =
    R.(let k = i32 1 / (i32 1 + f64 0.2316419 * x)
       let a1 = f64 0.319381530
       let a2 = f64 (-0.356563782)
       let a3 = f64 1.781477937
       let a4 = f64 (-1.821255978)
       let a5 = f64 1.330274429
       let N' x = exp(negate(x*x)) / sqrt(i32 2 * pi)
       in i32 1 - ((N' x) * (a1*k + a2*k*k + a3*k*k*k + a4*k*k*k*k
                             + a5*k*k*k*k*k)))

  let cum_norm_dist x =
    R.(if x < i32 0 then i32 1 - cum_norm_dist_pos(negate x)
       else cum_norm_dist_pos x)

}
