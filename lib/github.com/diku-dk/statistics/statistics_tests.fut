-- | ignore

import "statistics"

module s = statistics f64

-- ==
-- entry: mean
-- input { [3.0f64,4.5f64,6.0f64,6.5f64] }
-- output { 5.0f64 }

entry mean (vs:[]f64) : f64 =
  s.mean vs
