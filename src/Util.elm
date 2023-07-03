module Util exposing (..)


cond : List ( a -> Bool, a -> b ) -> a -> Maybe b
cond candidates arg =
    case candidates of
        [] ->
            Nothing

        ( predicate, transform ) :: rest ->
            if predicate arg then
                Just (transform arg)

            else
                cond rest arg


isZero : number -> Bool
isZero x =
    x == 0


isNotZero : number -> Bool
isNotZero x =
    x /= 0


isPos : number -> Bool
isPos x =
    x > 0


isNeg : number -> Bool
isNeg x =
    x < 0


isNatInt : Int -> Bool
isNatInt x =
    x >= 0


sign : number -> Int
sign x =
    if x > 0 then
        1

    else if x < 0 then
        -1

    else
        0


update : (a -> Bool) -> (a -> a) -> a -> a
update needUpdate f value =
    if needUpdate value then
        f value

    else
        value


dec : number -> number
dec x =
    x - 1


inc : number -> number
inc x =
    x + 1
