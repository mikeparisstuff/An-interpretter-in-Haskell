main :: IO ()
main = io (map processIt)

io f = interact (unlines . f . lines)

processIt s = show (length s)
