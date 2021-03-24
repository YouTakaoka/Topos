
import Parser

main = do
    print $ divListBy 0 [0, 1, 2]
    print $ divListBy 1 [0, 1, 2]
    print $ divListBy 2 [0, 1, 2]
    print $ divListBy 3 [0, 1, 2]
    print $ divListBy 1 []
    print $ divListInto ' ' "hoge fuga piyo"
    print $ divListInto ' ' " hoge fuga piyo"
    print $ divListInto ' ' "hoge fuga piyo "
    print $ divListInto ' ' "hoge  fuga piyo"
