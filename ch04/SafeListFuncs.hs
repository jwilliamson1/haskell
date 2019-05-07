safeListFunc func [] = Nothing
safeListFunc func xs = Just (func xs)

safeHead = safeListFunc head
safeTail = safeListFunc tail
safeLast = safeListFunc last
safeInit = safeListFunc init

mySafeHead :: [a] -> Maybe a
mySafeHead [] = Nothing
mySafeHead x = Just (head x)