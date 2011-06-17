
module Helpers where

times _ 0 = []
times a n = a : (times a (n-1))