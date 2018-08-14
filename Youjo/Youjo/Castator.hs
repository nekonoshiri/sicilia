module Youjo.Castator
( (+.)
, (.+)
, (-.)
, (.-)
, (*.)
, (.*)
, (**.)
, (.**)
) where

(+.) :: (Integral a, Fractional b) => a -> b -> b
x +. y = fromIntegral x + y

(.+) :: (Integral a, Fractional b) => b -> a -> b
x .+ y = x + fromIntegral y

(-.) :: (Integral a, Fractional b) => a -> b -> b
x -. y = fromIntegral x - y

(.-) :: (Integral a, Fractional b) => b -> a -> b
x .- y = x - fromIntegral y

(*.) :: (Integral a, Fractional b) => a -> b -> b
x *. y = fromIntegral x * y

(.*) :: (Integral a, Fractional b) => b -> a -> b
x .* y = x * fromIntegral y

(**.) :: (Integral a, Floating b) => a -> b -> b
x **. y = fromIntegral x ** y

(.**) :: (Integral a, Floating b) => b -> a -> b
x .** y = x ** fromIntegral y

