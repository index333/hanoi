import  Control.Monad.State 
ds n = [1..n]
getc = getContents >>= (return . lines)
disp (a,b,c) = mapM_ (\x -> liftIO $ print $ reverse x) [a,b,c]
move "AB" = get >>= \(a:aa,b,c) -> put (aa,a:b,c)
move "AC" = get >>= \(a:aa,b,c) -> put (aa,b,a:c)
move "BA" = get >>= \(a,b:bb,c) -> put (b:a,bb,c)
move "BC" = get >>= \(a,b:bb,c) -> put (a,bb,b:c)
move "CA" = get >>= \(a,b,c:cc) -> put (c:a,b,cc)
move "CB" = get >>= \(a,b,c:cc) -> put (a,c:b,cc)
move _ = return ()
doHanoi [] = return () 
doHanoi (x:xs) = do 
    move x
    (a,b,c) <- get
    liftIO $ print x
    disp (a,b,c)
    doHanoi xs
main = do
    let a = ds 4
    let b = []
    let c = []
    code <- getc
    print code
    mapM_ putStrLn code
    disp (a,b,c)
    runStateT (doHanoi code) (a,b,c)
