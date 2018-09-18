-- | ignore

import "statistics"

module s = mk_statistics f64

-- ==
-- entry: mean
-- input { [3.0f64,4.5f64,6.0f64,6.5f64] } output { 5.0f64 }
-- input { [600.0,470.0,170.0,430.0,300.0] } output { 394.0 }

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
-- entry: erf
-- input { 0.0 } output { 0.0 }
-- input { 0.02 } output { 0.022564575 }
-- input { 0.08 } output { 0.090078126 }
-- input { 0.3 } output { 0.328626759 }
-- input { 0.6 } output { 0.603856091 }
-- input { 1.0 } output { 0.842700793 }
-- input { 1.4 } output { 0.95228512 }
-- input { 1.8 } output { 0.989090502 }
-- input { 2.2 } output { 0.998137154 }
-- input { 3.5 } output { 0.999999257 }

entry erf = s.erf

-- ==
-- entry: gamma
-- input { 2.0 } output { 1.0 }
-- input { 3.0 } output { 2.0 }
-- input { 4.0 } output { 6.0 }
-- input { 5.0 } output { 24.0 }
-- input { 0.5 } output { 1.7724538509055159 }

entry gamma = s.gamma

-- ==
-- entry: gammaln
-- input { 2.0 } output { 0.0 }
-- input { 3.0 } output { 0.6931471805599453 }
-- input { 4.0 } output { 1.791759469228055 }
-- input { 5.0 } output { 3.1780538303479458 }
-- input { 0.5 } output { 0.5723649429247 }

entry gammaln = s.gammaln

-- ==
-- entry: variance
-- input { [600.0,470.0,170.0,430.0,300.0] } output { 27130.0 }

entry variance = s.variance

-- ==
-- entry: stddev
-- input { [600.0,470.0,170.0,430.0,300.0] } output { 164.7118696390761 }

entry stddev = s.stddev

-- ==
-- entry: variance_pop
-- input { [600.0,470.0,170.0,430.0,300.0] } output { 21704.0 }

entry variance_pop = s.variance_pop

-- ==
-- entry: stddev_pop
-- input { [600.0,470.0,170.0,430.0,300.0] } output { 147.32277488562318 }

entry stddev_pop = s.stddev_pop

-- ==
-- entry: skewness
-- input { [68.0,82.0,63.0,86.0,34.0,96.0,41.0,89.0,29.0,51.0,75.0,77.0,56.0,59.0,42.0] } output { -0.09869395 }

entry skewness = s.skewness

-- ==
-- entry: skewness_adj
-- input { [68.0,82.0,63.0,86.0,34.0,96.0,41.0,89.0,29.0,51.0,75.0,77.0,56.0,59.0,42.0] } output { -0.11001624579542711 }

entry skewness_adj = s.skewness_adj
