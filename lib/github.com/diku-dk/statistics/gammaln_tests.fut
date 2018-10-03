-- | ignore

import "gammaln"

module g = mk_gammaln f64

-- ==
-- entry: gammaln
-- input { 4.0 } output { 1.791759469228055 }
-- input { 0.5 } output { 0.5723649429247001 }
-- input { -0.5 } output { 1.2655121234846454 }

entry gammaln (x:f64) : f64 =
  g.gammaln x