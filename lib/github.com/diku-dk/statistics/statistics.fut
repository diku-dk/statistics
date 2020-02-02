-- | Statistics functions parameterised over the concrete
-- representation of reals.

import "gammaln"

module type statistics = {
  type t

  -- | `mean vs` returns the arithmetic mean of the values contained in `vs`.
  val mean [n] : [n]t -> t

  -- | `gmean vs` returns the geometric mean of the values contained in `vs`.
  val gmean [n] : [n]t -> t

  -- | `hmean vs` returns the harmonic mean of the values contained in `vs`.
  val hmean [n] : [n]t -> t

  -- | `qmean vs` returns the quadratic mean of the values contained
  -- in `vs`.  Also known as "root mean square".
  val qmean [n] : [n]t -> t

  -- | `variance vs` returns the sample variance of the values
  -- contained in `vs`. The sample variance is the square of the
  -- sample standard deviation.
  val variance [n] : [n]t -> t

  -- | `stddev vs` returns the sample standard deviation of the values
  -- contained in `vs`. The sample standard deviation is the square
  -- root of the sample variance.
  val stddev [n] : [n]t -> t

  -- | `covariance xs ys` returns the sample covariance between the
  -- values contained in `xs` and `ys`.
  val covariance [n] : [n]t -> [n]t -> t

  -- | `correlation xs ys` returns the sample (Pearson) correlation
  -- between the values contained in `xs` and `ys`.
  val correlation [n] : [n]t -> [n]t -> t

  -- | `variance_pop vs` returns the population variance of the values
  -- contained in `vs`. The population variance is the square of the
  -- population standard deviation.
  val variance_pop [n] : [n]t -> t

  -- | `stddev_pop vs` returns the population standard deviation of
  -- the values contained in `vs`. The population standard deviation
  -- is the square root of the population variance.
  val stddev_pop [n] : [n]t -> t

  -- | `skewness vs` returns the skewness of the values contained in
  -- `vs`. The skewness measures the assymetry of the values in
  -- `vs`. If the skewness is positive, the upper tail is thicker than
  -- the lower tail, whereas, if the skewness is negative, the lower
  -- tail is thicker than the upper tail. The skewness of a set of
  -- normally distributed values is zero.
  val skewness [n] : [n]t -> t

  -- | `skewness_adj vs` returns the adjusted Fisher-Pearson
  -- coefficient of skewness for the values contained in `vs`.
  val skewness_adj [n] : [n]t -> t

  -- | `kurtosis vs` returns the (non-excess) kurtosis of the values
  -- contained in `vs`.
  val kurtosis [n] : [n]t -> t

  -- | `kurtosis_excess vs` returns the excess kurtosis of the values
  -- contained in `vs`.
  val kurtosis_excess [n] : [n]t -> t

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
  -- stable in contrast to calculating `log(gamma x)` with large `x`s.
  val gammaln : t -> t

  -- | Generic type for distributions. Discrete distributions have
  -- type `dist i32`, whereas continuous distributions have type
  -- `dist t`.
  type^ dist 'a

  val mk_poison : {lambda:t} -> dist i32
  val mk_normal : {mu:t,sigma:t} -> dist t
  val mk_uniform : {a:t,b:t} -> dist t

  val pmf : dist i32 -> i32 -> t   -- probability mass function
  val pdf : dist t -> t -> t       -- probability distribution function
  val cdf 'a : dist a -> a -> t    -- cumulative distriubtion function

  -- | `sample d r` returns a sample from the distribution `d` given a
  -- real value `r` taken from a uniform distribution U(0,1).
  val sample 'a : dist a -> t -> a
}

module mk_statistics (R: float) : statistics with t = R.t = {
  type t = R.t

  import "../sorts/radix_sort"
  import "../segmented/segmented"

  let argmax [n] (none: t) (xs: [n]t): i32 =
    let max (x1, y1) (x2, y2) =
      if R.(y1 < y2) then (x2, y2) else (x1, y1)
    in reduce max (-1, none) (zip (iota n) xs) |> (.0)

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

  let covariance0 [n] (xs:[n]t) (xsm:t) (ys:[n]t) (ysm:t) : t =
    R.(reduce (+) (i32 0) (map2 (\x y -> (x - xsm) * (y - ysm)) xs ys))
        R./ (R.i32 (n - 1))

  let covariance [n] (xs:[n]t) (ys:[n]t) : t =
    covariance0 xs (mean xs) ys (mean ys)

  let correlation xs ys =
    R.(covariance xs ys / (stddev xs * stddev ys))

  let covariance_matrix [n] (xss: [n][]t) : [n][n]t =
    let means = map mean xss
    in map2 ( \xs xsm ->
              map2 (\ys ysm ->
                    covariance0 xs xsm ys ysm
                   ) xss means
            ) xss means

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
    let gammav = R.(aleph-i32 k) |> R.min (R.i32 1) |> R.max (R.i32 0)
    in R.(i32 1-gammav) R.* xs[k-1] R.+ gammav R.* xs[k]

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
                                                pval / (z+i32 i+i32 1)) (iota 8) p)
          let t = z + i32(length p) - f64 0.5
          in sqrt(i32 2 * pi) * t**(z+f64 0.5) * exp(negate t) * x)

  let gamma (z:t) : t =
    R.(if z < f64 0.5 then
         pi / (sin(pi*z) * gamma_big (i32 1 - z)) --Reflection formula
       else gamma_big z)

  module Gammaln = mk_gammaln R

  let gammaln (x:t) : t = Gammaln.gammaln x

  -- DISTRIBUTIONS
  type^ dist 'a = {pXf:a -> t,cdf:a -> t,cdfi:t -> a}

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

  let normal_cdf_inv sigma mu p =
    R.(let A1 = negate (f64 3.969683028665376e+01)
       let A2 = f64 2.209460984245205e+02
       let A3 = negate(f64 2.759285104469687e+02)
       let A4 = f64 1.383577518672690e+02
       let A5 = negate (f64 3.066479806614716e+01)
       let A6 = f64 2.506628277459239e+00

       let B1 = negate(f64 5.447609879822406e+01)
       let B2 = f64 1.615858368580409e+02
       let B3 = negate(f64 1.556989798598866e+02)
       let B4 = f64 6.680131188771972e+01
       let B5 = negate(f64 1.328068155288572e+01)

       let C1 = negate (f64 7.784894002430293e-03)
       let C2 = negate (f64 3.223964580411365e-01)
       let C3 = negate (f64 2.400758277161838e+00)
       let C4 = negate (f64 2.549732539343734e+00)
       let C5 = f64 4.374664141464968e+00
       let C6 = f64 2.938163982698783e+00

       let D1 = f64 7.784695709041462e-03
       let D2 = f64 3.224671290700398e-01
       let D3 = f64 2.445134137142996e+00
       let D4 = f64 3.754408661907416e+00

       let P_LOW = f64 0.02425
       -- P_high = 1 - p_low
       let P_HIGH = f64 0.97575

       let x =
          if i32 0 < p && p < P_LOW then
            let q = sqrt(negate(i32 2*log p))
            in (((((C1*q+C2)*q+C3)*q+C4)*q+C5)*q+C6) / ((((D1*q+D2)*q+D3)*q+D4)*q+i32 1)
          else if P_LOW <= p && p <= P_HIGH then
            let q = p - f64 0.5
            let r = q*q
            in (((((A1*r+A2)*r+A3)*r+A4)*r+A5)*r+A6)*q /(((((B1*r+B2)*r+B3)*r+B4)*r+B5)*r+i32 1)
          else if P_HIGH < p && p < i32 1 then
            let q = sqrt(negate(i32 2*log(i32 1-p)))
            in negate(((((C1*q+C2)*q+C3)*q+C4)*q+C5)*q+C6) / ((((D1*q+D2)*q+D3)*q+D4)*q+i32 1)
          else i32 0
       in mu + sigma * x
      )


  -- let normal_cdf_inv x =
  --   R.(let rational_approx t =
  --        -- Abramowitz and Stegun formula 26.2.23.
  --        -- The absolute value of the error should be less than 4.5 e-4.
  --        let c0 = f64 2.515517
  --        let c1 = f64 0.802853
  --        let c2 = f64 0.010328
  --        let d0 = f64 1.432788
  --        let d1 = f64 0.189269
  --        let d2 = f64 0.001308
  --        in t - ((c2*t + c1)*t + c0) / (((d2*t + d1)*t + d0)*t + i32 1)
  --      in if p < 0.5 then -- F^-1(p) = - G^-1(p)
  --           negate <| rational_approx( sqrt(negate(2.0*log p)))
  --         else -- F^-1(p) = G^-1(1-p)
  --           rational_approx( sqrt(negate(2.0*log(1-p)))))

  let poison_cdf_inv _lambda _x: i32 = 2

  let mk_poison {lambda:t} : dist i32 =
    {pXf=poison_pmf lambda,cdf=poison_cdf lambda, cdfi=poison_cdf_inv lambda}

  let mk_normal {sigma:t,mu:t} : dist t =
    {pXf=normal_pdf sigma mu,cdf=normal_cdf sigma mu,cdfi=normal_cdf_inv sigma mu}

  let mk_uniform {a:t,b:t} : dist t =
    {pXf=\x -> R.(if x < a || x > b then i32 0
                  else i32 1 / (b-a)),
     cdf=\x -> R.(if x < a then i32 0
                  else if x > b then i32 1
                  else (x-a)/(b-a)),
     cdfi=\x -> R.(a + (b-a)*x)}

  let pmf ({pXf,cdf=_,cdfi=_} : dist i32) x = pXf x
  let pdf ({pXf,cdf=_,cdfi=_} : dist t) x = pXf x
  let cdf {pXf=_,cdf,cdfi=_} x = cdf x

  let sample {pXf=_,cdf=_,cdfi} x = cdfi x

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
