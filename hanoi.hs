doHanoi o d t [_] = putStrLn $ o:d:[]
doHanoi o d t ds = do
    doHanoi o t d $ init ds
    doHanoi o d t $ [last ds]
    doHanoi t d o $ init ds
main = do
    let a = 'A'
    let b = 'B'
    let c = 'C'
    doHanoi a b c [1..4]
