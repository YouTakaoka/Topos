import Function

main = do
    let str1 = "2 * 4"
        str2 = "2*4"
        str3 = "2 *  4"
        str4 = "2  * 4"
        str5 = "2 * 4 * 6"
    print $ _toExp0 str1
    print $ _toExp0 str2
    print $ _toExp0 str3
    print $ _toExp0 str4
    print $ _toExp0 str5
