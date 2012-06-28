{-# LANGUAGE DoRec #-}
data Event = GetLine | GotLine [Char] | PutStrLn [Char] | Succ | Fail

handleIO :: Event -> IO Event
handleIO GetLine = do
    c <- getLine
    return $ GotLine c

handleIO (PutStrLn c) = do
    putStrLn c
    return Succ

eventLoop handler = do
    rec
        let events' = handler events
        events <- mapM handleIO events'
        return ()
    return ()

echo :: [Event] -> [Event]
echo ~(Succ: ~((GotLine s) : rest)) =
                             [
                                 PutStrLn "Echoing...",
                                 GetLine,
                                 PutStrLn s
                             ]

main = eventLoop echo
