import Function

main = do
    let f = (["x", "y"], words "( x + y ) / 2")
    let f_op = _macroGen f
    print $ f_op ["3", "5"]

