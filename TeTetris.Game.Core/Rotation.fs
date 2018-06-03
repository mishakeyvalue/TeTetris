namespace TeTetris.Game.Core

module Matricies = 
    let rec transpose = function
        | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
        | _ -> []
    
    let rec inner u v =
        match u, v with 
            | [x], [y] -> x*y     
            | u'::u, v'::v -> u'*v' + inner u v
    
    let multiply xs ys =
        [for row in xs ->
          [for col in transpose ys -> inner row col]]

    let private negateInt x = -x
    let negate xs = List.map (List.map negateInt) xs

    let rec add xs ys = 
        match xs, ys with
            | [], [] -> []
            | u'::u, v'::v -> [List.map2 (+) u' v'] @ add u v
        
    let sub xs ys = add xs (negate ys)

    let rotationMatrix = [[0;-1];[1;0]]


module Rotation = 
    open TeTetris.Game.Types
    let private toMatrix (p: Point) = [[p.x];[p.y]]
    let private fromMatrix ([[x'];[y']]) = { x=x';y=y'}

    let rotateMatrix pivot vector =
        let vr = Matricies.sub vector pivot
        let vt = Matricies.multiply Matricies.rotationMatrix vr
        Matricies.add pivot vt
    let rotate(c: TetraminoCoords) = 
        let pivot = toMatrix c.c
        let rotatePoint = toMatrix >> rotateMatrix pivot >> fromMatrix
        
        { c with a = rotatePoint c.a; b = rotatePoint c.b; d = rotatePoint c.d }





