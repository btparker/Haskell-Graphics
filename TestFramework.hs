module TestFramework where
assertTrue :: Bool -> String -> IO ()
assertTrue x claim = if x
                     then putStrLn claim
                     else putStrLn $ "*** Untrue: " ++ claim   
                                                                                
assertFalse :: Bool -> String -> IO ()
assertFalse x claim = assertTrue (not x) claim  

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual x y = if x == y
                  then putStrLn $ (show x) ++ " equals " ++ (show y)
                  else putStrLn $ "*** " ++ (show x) ++ " does not equal " ++ (show y)