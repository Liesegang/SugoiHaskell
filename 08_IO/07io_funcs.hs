main = do
   putStr "Hey, "
   putStr "I'm "
   putStrLn "Andy!"

   putChar 't'
   putChar 'h'
   putChar 'e'

   myPutStr " apple is green!"

   print True
   print 2
   print "hahah"
   print 3.2
   print [3, 4, 5]

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
    putChar x
    myPutStr xs
