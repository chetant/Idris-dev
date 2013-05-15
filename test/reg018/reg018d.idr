module Main

total
pull : Fin (S n) -> Vect a (S n) -> (a, Vect a n)
pull {n=O}   _      (x :: xs) = (x, xs)
-- pull {n=S q} fO     (Vect.(::) {n=S _} x xs) = (x, xs)
pull {n=S _} (fS n) (x :: xs) =
  let (v, vs) = pull n xs in
        (v, x::vs)

main : IO ()
main = print (pull fO [0, 1, 2])
