module TeTetris.Utils

module Seq = 
  let repeat items = 
    seq { while true do yield! items }

let (|SeqEmpty|SeqCons|) (xs: 'a seq) = //'
  if Seq.isEmpty xs then SeqEmpty
  else SeqCons(Seq.head xs, Seq.skip 1 xs)

let deattachHead = function
    | SeqCons (x, xs) -> x, xs
