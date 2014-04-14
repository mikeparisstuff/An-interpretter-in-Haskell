import System.IO
import Data.List
import System.Environment

prepare f = 
    let --zipM :: ([(a, a)], [a]) -> ([(a, a)], [a])
        zipM (tupd, x:b:xs) = zipM ((x,b):tupd, xs)
        zipM (tupd, [])     = (tupd, [])
        -- using it
        (c, b) = zipM ([], f)
        -- find dependencies
        allDeps :: (Eq a) => [(a, a)] -> a -> [a]
        allDeps c x = [ a | (a, e) <- c, e == x]
        graph = map (\(x, _) -> (x, allDeps c x)) c
        
        --findStart :: (Ord a) => [(a, [a])] => Either String a
        findStart graph  -- lets us pull first element out
            | length empts > 0 = Right $ head $ reverse empts
            | otherwise        = Left "cycle"
            where empts = (sort . filter (\(x, y) -> length y == 0)) graph

        -- Modifies the graph by removing every instance of edge elem
        -- And also the vertex
        --removeElems :: (Eq a) => [(a, [a])] -> a -> [(a, [a])]
        removeElems graph elem = 
                             -- keep x if it does not match the elem
            let f elem (x, lst) = (x, filter (\e -> e /= elem) lst)
                tmp_g = map (f elem) graph in
            filter (\(x, lst) -> x /= elem) tmp_g
                
        --topoSort :: [(a, [a])] -> [a] -> [a]
        topoSort graph order s = 
            case s of 
                (Left _)  ->  order
                (Right (s, _)) ->
                    -- remove all occurences of s from g
                    let mod_g = removeElems graph s in
                    topoSort mod_g (s:order) $ findStart mod_g
    in 
    topoSort graph [] $ findStart graph
    




main = do
    [s] <- getArgs
    fi   <- readFile s
    let f = lines fi
        ordered = prepare f
    if length ordered /= (quot (length f) 2) then
        mapM_ putStrLn  ordered
    else
        putStrLn "cycle"
    return ()
     

