-- | ignore

import "statistics"

module s = mk_statistics f64

-- ==
-- entry: mean
-- input { [3.0f64,4.5f64,6.0f64,6.5f64] }
-- output { 5.0f64 }

entry mean (vs:[]f64) : f64 =
  s.mean vs

-- ==
-- entry: gmean
-- input { [3.0,4.5,6.0,6.5] }
-- output { 4.790155301362109 }

entry gmean = s.gmean

-- ==
-- entry: hmean
-- input { [3.0,4.5,6.0,6.5] }
-- output { 4.565853658536585 }

entry hmean = s.hmean

-- ==
-- entry: qmean
-- input { [3.0,4.5,6.0,6.5] }
-- output { 5.18411033833193 }

entry qmean = s.qmean

-- ==
-- entry: median
-- input { [4.5,3.0,6.5,6.0] }
-- output { 5.25 }

entry median = s.median

-- ==
-- entry: median_sorted
-- input { [3.0,4.5,6.0,6.5] }
-- output { 5.25 }

entry median_sorted = s.median_sorted

-- ==
-- entry: quantile_sorted
-- input { [3.0,4.5,6.0,6.5] 0.3 }
-- output { 3.9900000035762786f64 }
-- input { [3.0,4.5,6.0,6.5] 0.5 }
-- output { 5.25 }

entry quantile_sorted = s.quantile_sorted

-- ==
-- entry: quantile
-- input { [4.5,3.0,6.5,6.0] 0.3 }
-- output { 3.9900000035762786f64 }
-- input { [4.5,3.0,6.5,6.0] 0.5 }
-- output { 5.25 }

entry quantile = s.quantile
