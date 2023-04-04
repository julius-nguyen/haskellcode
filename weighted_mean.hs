weightedMean :: (Fractional a, Integral b) => [b] -> [b] -> a
weightedMean xs ws = (fromIntegral (sum [x*w |w <- ws, x <- xs])) / (fromIntegral(sum ws))
