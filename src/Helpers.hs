
module Helpers where

times a 0 = []
times a n = a ++ (times a (n-1))

next [] = []
next (x:xs) = x : next xs
getNext a = head (next a)

left  = "1"
right = "2"
cards = ["I", "zero", "succ", "dbl", "get",
         "put", "S", "K", "inc", "dec", "attack",
         "help", "copy", "revive", "zombie"
        ]
