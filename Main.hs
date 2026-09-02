main::IO()
main = do 
        print(reajuste[("f", 10), ("b",7), ("U", 5.6)] )
        print(listaDeProdutos[("K",10,5),("L",5,9),("P",4,15)])
        print("===================================")
        print(pessoaMaior  p1 p2)
        print(calMedia p2)

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


bonusSalario::Ptest -> Int
bonusSalario 