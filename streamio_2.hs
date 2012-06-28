{-# LANGUAGE DoRec #-}
data Event = GetLine | GotLine [Char] | PutStrLn [Char] | Succ | Fail

handleIO :: Event -> IO Event
handleIO GetLine = do
    c <- getLine
    return $ GotLine c

handleIO (PutStrLn c) = do
    putStrLn c
    return Succ

handleIO other = return other

eventLoop handler = do 
    rec
        let events' = handler events
        events <- mapM handleIO events'
        return events
    return()

echo :: [Event] -> [Event]
echo ~(Succ: ~((GotLine s) : rest)) =
                             [
                                 PutStrLn "Echoing...",
                                 GetLine,
                                 PutStrLn s
                             ] ++ (echo rest)
echo (_:rest) = echo rest

main = eventLoop echo
