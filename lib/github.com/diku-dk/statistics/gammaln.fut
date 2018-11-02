-- The original C code, the long comment, copyright, license, and the constants
-- are from
--   https://svnweb.freebsd.org/base/release/9.3.0/lib/msun/src/e_lgamma_r.c?revision=268523&view=co
--
-- The implementation follows the original, but has been modified for Futhark.

-- * ====================================================
-- * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
-- *
-- * Developed at SunPro, a Sun Microsystems, Inc. business.
-- * Permission to use, copy, modify, and distribute this
-- * software is freely granted, provided that this notice
-- * is preserved.
-- * ====================================================


-- * __ieee754_lgamma_r(x, signgamp)
-- *    Reentrant version of the logarithm of the Gamma function
-- * with user provided pointer for the sign of Gamma(x).
-- *
-- * Method:
-- *   1. Argument Reduction for 0 < x <= 8
-- *      Since gamma(1+s)=s*gamma(s), for x in [0,8], we may
-- *      reduce x to a number in [1.5,2.5] by
-- *              lgamma(1+s) = log(s) + lgamma(s)
-- *      for example,
-- *              lgamma(7.3) = log(6.3) + lgamma(6.3)
-- *                          = log(6.3*5.3) + lgamma(5.3)
-- *                          = log(6.3*5.3*4.3*3.3*2.3) + lgamma(2.3)
-- *   2. Polynomial approximation of lgamma around its
-- *      minimum (ymin=1.461632144968362245) to maintain monotonicity.
-- *      On [ymin-0.23, ymin+0.27] (i.e., [1.23164,1.73163]), use
-- *              Let z = x-ymin;
-- *              lgamma(x) = -1.214862905358496078218 + z**2*poly(z)
-- *              poly(z) is a 14 degree polynomial.
-- *   2. Rational approximation in the primary interval [2,3]
-- *      We use the following approximation:
-- *              s = x-2.0;
-- *              lgamma(x) = 0.5*s + s*P(s)/Q(s)
-- *      with accuracy
-- *              |P/Q - (lgamma(x)-0.5s)| < 2**-61.71
-- *      Our algorithms are based on the following observation
-- *
-- *                             zeta(2)-1    2    zeta(3)-1    3
-- * lgamma(2+s) = s*(1-Euler) + --------- * s  -  --------- * s  + ...
-- *                                 2                 3
-- *
-- *      where Euler = 0.5772156649... is the Euler constant, which
-- *      is very close to 0.5.
-- *
-- *   3. For x>=8, we have
-- *      lgamma(x)~(x-0.5)log(x)-x+0.5*log(2pi)+1/(12x)-1/(360x**3)+....
-- *      (better formula:
-- *         lgamma(x)~(x-0.5)*(log(x)-1)-.5*(log(2pi)-1) + ...)
-- *      Let z = 1/x, then we approximation
-- *              f(z) = lgamma(x) - (x-0.5)(log(x)-1)
-- *      by
-- *                                  3       5             11
-- *              w = w0 + w1*z + w2*z  + w3*z  + ... + w6*z
-- *      where
-- *              |w - f(z)| < 2**-58.74
-- *
-- *   4. For negative x, since (G is gamma function)
-- *              -x*G(-x)*G(x) = pi/sin(pi*x),
-- *      we have
-- *              G(x) = pi/(sin(pi*x)*(-x)*G(-x))
-- *      since G(-x) is positive, sign(G(x)) = sign(sin(pi*x)) for x<0
-- *      Hence, for x<0, signgam = sign(sin(pi*x)) and
-- *              lgamma(x) = log(|Gamma(x)|)
-- *                        = log(pi/(|x*sin(pi*x)|)) - lgamma(-x);
-- *      Note: one should avoid computing pi*(-x) directly in the
-- *            computation of sin(pi*(-x)).
-- *
-- *   5. Special Cases
-- *      lgamma(2+s) ~ s*(1-Euler) for tiny s
-- *      lgamma(1)=lgamma(2)=0
-- *      lgamma(x) ~ -log(x) for tiny x
-- *      lgamma(0) = lgamma(inf) = inf
-- *      lgamma(-integer) = +-inf

module type gammaln = {
  type t
  val gammaln : t -> t
}

module mk_gammaln (R: real) : gammaln with t = R.t = {

type t = R.t

let A1C = R.f64 7.72156649015328655494e-02 -- 0x3FB3C467E37DB0C8
let A1 = [R.f64 6.73523010531292681824e-02, -- 0x3FB13E001A5562A7
          R.f64 7.38555086081402883957e-03, -- 0x3F7E404FB68FEFE8
          R.f64 1.19270763183362067845e-03, -- 0x3F538A94116F3F5D
          R.f64 2.20862790713908385557e-04, -- 0x3F2CF2ECED10E54D
          R.f64 2.52144565451257326939e-05  -- 0x3EFA7074428CFA52
         ]
let A2C = R.f64 3.22467033424113591611e-01 -- 0x3FD4A34CC4A60FAD
let A2 = [R.f64 2.05808084325167332806e-02, -- 0x3F951322AC92547B
          R.f64 2.89051383673415629091e-03, -- 0x3F67ADD8CCB7926B
          R.f64 5.10069792153511336608e-04, -- 0x3F40B6C689B99C00
          R.f64 1.08011567247583939954e-04, -- 0x3F1C5088987DFB07
          R.f64 4.48640949618915160150e-05  -- 0x3F07858E90A45837
         ]

let RC = R.f64 1.0

let R' = [R.f64 1.39200533467621045958e+00, -- 0x3FF645A762C4AB74
          R.f64 7.21935547567138069525e-01, -- 0x3FE71A1893D3DCDC
          R.f64 1.71933865632803078993e-01, -- 0x3FC601EDCCFBDF27
          R.f64 1.86459191715652901344e-02, -- 0x3F9317EA742ED475
          R.f64 7.77942496381893596434e-04, -- 0x3F497DDACA41A95B
          R.f64 7.32668430744625636189e-06  -- 0x3EDEBAF7A5B38140
         ]

let SC = R.negate(R.f64 7.72156649015328655494e-02) -- 0xBFB3C467E37DB0C8

let S = [R.f64 2.14982415960608852501e-01,  -- 0x3FCB848B36E20878
         R.f64 3.25778796408930981787e-01,  -- 0x3FD4D98F4F139F59
         R.f64 1.46350472652464452805e-01,  -- 0x3FC2BB9CBEE5F2F7
         R.f64 2.66422703033638609560e-02,  -- 0x3F9B481C7E939961
         R.f64 1.84028451407337715652e-03,  -- 0x3F5E26B67368F239
         R.f64 3.19475326584100867617e-05   -- 0x3F00BFECDD17E945
        ]
let T1C = R.f64 4.83836122723810047042e-01 -- 0x3FDEF72BC8EE38A2
let T1 = [R.negate(R.f64 3.27885410759859649565e-02), -- 0xBFA0C9A8DF35B713
          R.f64 6.10053870246291332635e-03,  -- 0x3F78FCE0E370E344
          R.negate(R.f64 1.40346469989232843813e-03), -- 0xBF56FE8EBF2D1AF1
          R.f64 3.15632070903625950361e-04   -- 0x3F34AF6D6C0EBBF7
         ]
let T2C = R.negate(R.f64 1.47587722994593911752e-01) -- 0xBFC2E4278DC6C509
let T2 = [R.f64 1.79706750811820387126e-02,  -- 0x3F9266E7970AF9EC
          R.negate(R.f64 3.68452016781138256760e-03), -- 0xBF6E2EFFB3E914D7
          R.f64 8.81081882437654011382e-04,  -- 0x3F4CDF0CEF61A8E9
          R.negate(R.f64 3.12754168375120860518e-04)  -- 0xBF347F24ECC38C38
         ]
let T3C = R.f64 6.46249402391333854778e-02 -- 0x3FB08B4294D5419B
let T3 = [R.negate(R.f64 1.03142241298341437450e-02), -- 0xBF851F9FBA91EC6A
          R.f64 2.25964780900612472250e-03,  -- 0x3F6282D32E15C915
          R.negate(R.f64 5.38595305356740546715e-04), -- 0xBF41A6109C73E0EC
          R.f64 3.35529192635519073543e-04   -- 0x3F35FD3EE8C2D3F4
         ]
let UC = R.negate(R.f64 7.72156649015328655494e-02) -- 0xBFB3C467E37DB0C8
let U = [R.f64 6.32827064025093366517e-01,  -- 0x3FE4401E8B005DFF
         R.f64 1.45492250137234768737e+00,  -- 0x3FF7475CD119BD6F
         R.f64 9.77717527963372745603e-01,  -- 0x3FEF497644EA8450
         R.f64 2.28963728064692451092e-01,  -- 0x3FCD4EAEF6010924
         R.f64 1.33810918536787660377e-02   -- 0x3F8B678BBF2BAB09
        ]
let VC = R.f64 1.0
let V = [R.f64 2.45597793713041134822e+00, -- 0x4003A5D7C2BD619C
         R.f64 2.12848976379893395361e+00, -- 0x40010725A42B18F5
         R.f64 7.69285150456672783825e-01, -- 0x3FE89DFBE45050AF
         R.f64 1.04222645593369134254e-01, -- 0x3FBAAE55D6537C88
         R.f64 3.21709242282423911810e-03  -- 0x3F6A5ABB57D0CF61
        ]
let WC = R.f64 4.18938533204672725052e-01 -- 0x3FDACFE390C97D69
let W = [R.f64 8.33333333333329678849e-02,  -- 0x3FB555555555553B
         R.negate(R.f64 2.77777777728775536470e-03), -- 0xBF66C16C16B02E5C
         R.f64 7.93650558643019558500e-04,  -- 0x3F4A019F98CF38B6
         R.negate(R.f64 5.95187557450339963135e-04), -- 0xBF4380CB8C0FE741
         R.f64 8.36339918996282139126e-04,  -- 0x3F4B67BA4CDAD5D1
         R.negate(R.f64 1.63092934096575273989e-03)  -- 0xBF5AB89D0B9E43E4
        ]
let YMIN = R.f64 1.461632144968362245
let TWO52 = R.f64 4503599627370496 -- 2**52
let TWO58 = R.f64 288230376151711744 -- 2**58
let TINY = R.f64 8.470329472543003e-22
let TC = R.f64 1.46163214496836224576e+00 -- 0x3FF762D86356BE3F
let TF = R.negate(R.f64 1.21486290535849611461e-01) -- 0xBFBF19B9BCC38A42
let TT = R.negate(R.f64 3.63867699703950536541e-18) -- 0xBC50C7CAA48A971F => TT = -(tail of TF)

let evalpoly [n] (coffs:[n]t) (x:t) : t =
  R.(reduce (+) (i32 0) (map2 (\i c -> c * (x ** i32 i)) (iota n) coffs))

let polyvalA1 = evalpoly A1
let polyvalA2 = evalpoly A2
let polyvalR = evalpoly R'
let polyvalS = evalpoly S
let polyvalT1 = evalpoly T1
let polyvalT2 = evalpoly T2
let polyvalT3 = evalpoly T3
let polyvalU = evalpoly U
let polyvalV = evalpoly V
let polyvalW = evalpoly W

let sinpi x = R.(sin (pi * x))

let case0 r y =
  R.(let z = y * y
     let p1 = A1C + z*polyvalA1 z
     let p2 = z * (A2C + z*polyvalA2 z)
     let p = y*p1 + p2
     in r + ( p - f64 0.5 * y ))

let case1 r y =
  R.(let z = y * y
     let w = z * y
     let p1 = T1C + w*polyvalT1 w
     let p2 = T2C + w*polyvalT2 w
     let p3 = T3C + w*polyvalT3 w
     let p = z*p1 - (TT - w*(p2+y*p3))
     in r + TF + p)

let case2 r y =
  R.(let p1 = y * (UC + y*polyvalU y)
     let p2 = VC + y*polyvalV y
     in r + ( negate(f64 0.5) * y + p1/p2 ))

let gammaln (x:t) : t =
  R.(if isnan x || isinf x then x
     else if x == i32 0 then inf
     else let (isNegative,x) =
            if x < i32 0 then (true,negate x)
            else (false,x)
          let return r =
            if isNegative then
               let nadj = log( pi / abs( sinpi(x) * x ) )
               in nadj - r
            else r
          in if x < TINY then negate(log x)
             else if isNegative && x >= TWO52 then inf
             else if isNegative && sinpi x == i32 0 then inf
             else if x == i32 1 || x == i32 2 then i32 0
             else
               let r =
                 if x < i32 2 then
                   if x <= f64 0.9 then
                      let r = negate(log x)
                      in if x >= ( YMIN - i32 1 + f64 0.27 ) then
                            case0 r (i32 1 - x)
                         else if x >= (YMIN - i32 1 - f64 0.27) then
                            case1 r (x - (TC - i32 1))
                         else
                            case2 r x
                   else
                      let r = i32 0
                      in if x >= (YMIN + f64 0.27) then
                            case0 r (i32 2 - x)
                         else if x >= (YMIN - f64 0.27) then
                            case1 r (x - TC)
                         else
                            case2 r (x - i32 1)
                 else -- x >= 2.0
                    if x < i32 8 then
                      let flg = trunc x
                      let y = x - flg
                      let p = y * (SC + y*polyvalS y)
                      let q = RC + y*polyvalR y
                      let r = f64 0.5 * y + p/q  -- gammaln(1+s) = ln(s) + gammaln(s)
                      let cases v z = if flg >= i32 v then z*(y+(i32 v - i32 1)) else z
                      let z = i32 1
                      let z = cases 7 z
                      let z = cases 6 z
                      let z = cases 5 z
                      let z = cases 4 z
                      let z = cases 3 z
                      in if flg > i32 2 then r + log z
                         else r
                    else if x < TWO58 then -- 8 <= x < 2**58
                      let t = log x
                      let z = i32 1 / x
                      let y = z * z
                      let w = WC + z * polyvalW y
                      in (x - f64 0.5) * (t - i32 1) + w
                    else -- 2**58 <= x <= Inf
                       x * ( log x - i32 1 )
               in return r)
}
