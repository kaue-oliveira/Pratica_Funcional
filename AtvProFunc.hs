--Avaliação de Programação Funcional
--Universidade Federal de Lavras
--Curso: Ciência da Computação
--Disciplina: Paradigmas de Linguagens de Programação
--Membro 1: Kauê de Oliveira Silva    nº de matrícula: ********* 
--Membro 2: Thiago Ferreira Azevedo   nº de matrícula: *********      
--Designação: Grupo 1 de questões
----------------------------------------------------------------------------------------------------------

import Data.Char (toUpper, toLower)

--Questão 1 Unica ocorrencia------------------------------------------------------------------------------

unicaOcorrencia::(Integral t)=> t-> [t]-> Bool
unicaOcorrencia n lista
    | countOcorrencia n lista == 1 = True
    | otherwise = False
    where
        countOcorrencia n [] = 0
        countOcorrencia n (c:r)
            | n == c = 1 + countOcorrencia n r 
            | otherwise = countOcorrencia n r

-- Questão 4 remove ---------------------------------------------------------------------------------------

remove :: (Eq t) => t -> [t] -> [t]
remove c [] = []
remove c (t:r)
    | c == t    = r
    | otherwise = t : remove c r

--Questão 7------------------------------------------------------------------------------------------------
-- Função para encontrar o menor elemento da lista
encontrarMenor :: (Ord t) => [t] -> t
encontrarMenor [c] = c
encontrarMenor (c:r) 
  | c < menorResto = c
  | otherwise = menorResto
  where menorResto = encontrarMenor r
-- Função para remover a primeira ocorrência do menor elemento
removerMenor :: (Ord t) => [t] -> [t]
removerMenor [] = []
removerMenor r = remove (encontrarMenor r) r
-- Função para reduzir a lista até o tamanho n
maiores :: (Ord t) => Int -> [t] -> [t]
maiores n r
  | contaComprimento r <= n = r
  | otherwise = maiores n (removerMenor r)

--Questão 10 divide------------------------------------------------------------------------------------------

divide :: (Integral t) =>[t] -> t -> ([t], [t])
divide r 0 = ([], r)  
divide (c:r) n = (c:cabeca, resto)
  where
    (cabeca, resto) = divide r (n-1)  


--Questão 13  Uniao------------------------------------------------------------------------------------------

uniao :: (Eq t) => [t] -> [t] -> [t]
uniao list1 list2 = removeDuplicates (list1 ++ list2)
    where
        removeDuplicates [] = []
        removeDuplicates (x:xs) = x : removeDuplicates (remove x xs) --chama remove que ja foi implementado

--Questão 16 sequencia---------------------------------------------------------------------------------------

sequencia :: (Integral t)=> t -> t -> [t]
sequencia n m
  | n <= 0    = []
  | otherwise = m : sequencia (n - 1) (m + 1)

--19  Ordenação----------------------------------------------------------------------------------------------

ordena::(Ord t)=> [t] -> [t]
ordena [c] = [c]
ordena (c:r) = insereOrd c (ordena r)
    where 
        insereOrd e [] = [e]
        insereOrd e (c:r)
            | e < c = e : c : r  
            | otherwise = c : insereOrd e r



--Questão 22 rodar_esquerda----------------------------------------------------------------------------------

rodar_esquerda::(Integral t) =>  t -> [t] -> [t]
rodar_esquerda n r = rotacionar n r
  where
    rotacionar 0 ys = ys
    rotacionar k (c:r) = rotacionar (k - 1) (r ++ [c])



contaComprimento :: [a] -> Int
contaComprimento lista = conta lista 0
  where
    conta [] acumulador = acumulador
    conta (c:r) acumulador = conta r (acumulador + 1)

--Questão 25 primeiras maiusculas-----------------------------------------------------------------------------

-- Função principal que capitaliza a primeira letra de cada palavra
primeiras_maiusculas :: String -> String
primeiras_maiusculas [] = []
primeiras_maiusculas s = percorre s True
  where
    percorre :: String -> Bool -> String
    percorre s True
        | s == [] = []
        | otherwise = let (c:r) = s in
            if c == ' ' then
                c : percorre r True
            else
                toUpper c : percorre r False
    percorre s False
        | s == [] = []
        | otherwise = let (c:r) = s in
            if c == ' ' then
                c : percorre r True
            else
                toLower c : percorre r False

--Questão 28 mediana de numeros racionais----------------------------------------------------------------------- 

mediana :: (Ord t, Fractional t) => [t] -> t
mediana lista
  | even comprimento = (elemento listaOrdenada (meio - 1) + elemento listaOrdenada meio) / 2
  | otherwise = elemento listaOrdenada meio
  where
    listaOrdenada = bolha lista
    comprimento = contaComprimento lista
    meio = dividirPorDois comprimento

-- dividir por 2 de forma recursiva
dividirPorDois :: (Integral t)=> t -> t
dividirPorDois 0 = 0
dividirPorDois 1 = 0
dividirPorDois n = 1 + dividirPorDois (n - 2)

-- Função para obter n-ésimo elemento de uma lista
elemento :: (Ord t)=> [t] -> Int -> t
elemento (c:r) 0 = c
elemento (y:r) n = elemento r (n - 1)


--Questão 31 palindromo? ---------------------------------------------------------------------------------------
ehPalindromo :: (Eq t)=> [t] -> Bool
ehPalindromo r = r == inverter r

inverter ::[t] -> [t]
inverter [] = []
inverter (c:r) = inverter r ++ [c]


--Questão 34 bubble sort----------------------------------------------------------------------------------------

bolha :: (Ord t) => [t] -> [t]
bolha lista = bolhaRec lista (contaComprimento lista)
  where
    -- Função recursiva
    bolhaRec :: (Ord t) => [t] -> Int -> [t]
    bolhaRec lista 0 = lista
    bolhaRec lista n = bolhaRec (passo lista) (n - 1)

    -- Função para realizar um passo 
    passo :: (Ord t) => [t] -> [t]
    passo [] = []
    passo [c] = [c]
    passo (c:y:r)
      | c > y     = y : passo (c:r)
      | otherwise = c : passo (y:r)
    
    -- Função para calcular o comprimento da lista
    contaComprimento :: [t] -> Int
    contaComprimento lista = conta lista 0
      where
        conta [] acumulador = acumulador
        conta (c:r) acumulador = conta r (acumulador + 1)
