import Data.List
import Data.Char
import Funcoeshaskell
main::IO()
main = do 
        print(reajuste[("f", 10), ("b",7), ("U", 5.6)] )
        print(listaDeProdutos[("K",10,5),("L",5,9),("P",4,15)])
        print("===================================")
        print(pessoaMaior  p1 p2)
        print(calMedia p2)
        print(somi [1,3,2])
        print(convertP["gabriel", "lindo"])
        print("============================")
        print(dobrar (calcAreaCirc(5.86)))
        print(msg)
        print( elevaEinverte [2,3,4] )
        print(nomesAprovados [("GB",10.0),("Gabriel",10.0),("piolho",6.0)])


pars::[Int] ->ç

elevaEinverte :: [Int] -> [Int]
elevaEinverte lista = map(*(-1))(map(^2)lista)

nomesAprovados::[(String,Float)] -> [String]
nomesAprovados lista = map fst (filter (\(nome,nota) -> length nome >5 && nota >= 7.0)lista)

data Pessoa = Pessoa{nome::String, idade::Int, altura::Float, ano::Int}
eu = Pessoa{nome = "Gabriel", idade = 23, altura = 1.75, ano = 2003}

data Ptest = Ptest{name::String, height::Float, nota::(Float, Float, Float)}
p1 = Ptest{name = "cla", height = 1.55, nota = (10,5,3)}
p2 = Ptest{name = "Joseval", height = 1.71, nota = (7,4,8)} 
dobraPositivo :: [Int] -> [Int]
dobraPositivo [] = []
dobraPositivo (x:xs)
    |x >0 = 2* x:dobraPositivo xs
    |otherwise = x:dobraPositivo xs

intercalar::[Int] -> [Int] -> [Int] 
intercalar [][] = []
intercalar x [] =  x
intercalar [] y = y
intercalar (x:xs) (y:ys) = x:y: intercalar xs ys

trocador:: Int -> Int -> [Int] -> [Int]
trocador old new [] = []
trocador old new (x:xs)
    | x == old = new : trocador old new xs
    | otherwise = x : trocador old new xs

reajuste::[(String, Float )] -> [(String, Float )]
reajuste [] = []
reajuste ((nome,preco):xs) = (nome, preco * 1.1) : reajuste xs

listaDeProdutos:: [(String, Float, Float)] -> Float
listaDeProdutos [] = 0
listaDeProdutos((nome, preco, qtd):xs) = preco * qtd +  listaDeProdutos xs

pessoaMaior:: Ptest -> Ptest -> String
pessoaMaior p1 p2 
    |height p1 > height p2 = name p1
    |otherwise = name p2
    
soma::(Float,Float,Float) -> Float
soma (x,y,z)= x+z+y

calMedia::Ptest -> String
calMedia a
    |soma( nota a)/3 >= 5 = "Aprovado!"
    |otherwise = "reprovado!"

somi:: [Int] -> Int
somi p  = sum(map(^2)(filter (even) p))

convertP :: [String] -> [String]
convertP lista = map(map(toUpper))lista