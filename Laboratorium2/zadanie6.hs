-- zadanie 6

data List = Empty | Cons FSObject List deriving Show
data FSObject = File String | Folder String List deriving Show

fs :: FSObject

fs = Folder "root" (Cons (File "f1") (Cons (File "f2") (Cons (Folder "foler1" (Cons (File "f3") Empty)) Empty)))

-- szuka pliku o podanej nazwie w danym systemu plików (tj. dla obiektu będącego korzeniem tego systemu)

searchInnerList :: String -> List -> String -> Maybe String
searchInnerList name Empty path             = Nothing
searchInnerList name (Cons fsobj next) path = let result = searchInner name fsobj path in
                                                        case result of Nothing -> searchInnerList name next path
                                                                       Just _  -> result
searchInner :: String -> FSObject -> String -> Maybe String
searchInner name (File fileName) path | name == fileName = Just (path ++ "/" ++ name)
                                      | otherwise        = Nothing

searchInner name (Folder folderName fsObjList) path = searchInnerList name fsObjList (path ++ "/" ++ folderName)

search :: String -> FSObject -> Maybe String
search name fsobj = searchInner name fsobj ""