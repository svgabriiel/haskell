module Funcoeshaskell(
    dobrar,
    calcAreaCirc,
    msg
)where

dobrar :: Double -> Double
dobrar x = x*2

piValue::Double
piValue = 3.14159


calcAreaCirc:: Double -> Double
calcAreaCirc raio = piValue * raio^2

msg::String
msg = "Voce e Vencerdor!!!"