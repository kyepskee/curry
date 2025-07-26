# implicits, intersections, subtyping, symbols, effects some day?, types as values

(:: square (-> int int))
(def square (lambda (x)
    (* x x))

# more precisely
(:: square (forall (numt <: num) (-> numt numt)))
