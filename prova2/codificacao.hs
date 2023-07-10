-- Função de codificar uma String
codificar :: String -> String
codificar [] = []
codificar (x : xs) = codificarAux (getStringOfEqualsChars (x : xs) ' ')

-- Codifica uma lista de Strings de caracteres iguais em uma String (Função auxiliar)
codificarAux :: [String] -> [Char]
codificarAux [] = []
codificarAux (x : xs) = int2str (countEqualsChars x) ++ firstItemOfList x ++ codificarAux xs

-- Retorna a lista separada de Strings de caracteres iguais
getStringOfEqualsChars :: [Char] -> Char -> [String]
getStringOfEqualsChars [] ' ' = []
getStringOfEqualsChars (x : xs) returnItem
  | isLastItem = [] -- Verifica se é o último item da lista
  | returnItem == x && checkListaVazia = getStringOfEqualsChars xs x -- Verifica se o item atual é igual ao item anterior e se a lista não está vazia
  | [x] == firstItemOfList xs = linearizar [[x : auxGetStringOfEqualsChars x xs]] ++ getStringOfEqualsChars xs x 
  | otherwise = [x] : getStringOfEqualsChars xs ' '
  where
    isLastItem
      | length xs == 0 && x == returnItem = True -- Verifica se é o último item da lista
      | otherwise = False
    checkListaVazia
      | xs == [] = False -- Verifica se a lista está vazia
      | otherwise = True

-- Retorna a lista de Strings de caracteres iguais agrupando os caracteres iguais
auxGetStringOfEqualsChars :: Char -> [Char] -> [Char]
auxGetStringOfEqualsChars c [] = []
auxGetStringOfEqualsChars c (x : xs)
  | c == x = x : auxGetStringOfEqualsChars c xs
  | otherwise = []

-- Copia o primeiro item de uma Lista e adiciona na próxima
linearizar :: [[String]] -> [String]
linearizar [] = []
linearizar (x : xs) = x ++ linearizar xs

-- Retorna o primeiro item de uma lista
firstItemOfList :: String -> String
firstItemOfList [] = ""
firstItemOfList (x : xs) = [x]

-- Retorna o primeiro char de uma lista
firstCharOfList :: String -> Char
firstCharOfList [] = ' '
firstCharOfList (x : xs) = x

-- Retorna a quantidade de caracteres iguais em uma String que está em uma lista
countEqualsChars :: String -> Int
countEqualsChars [] = 0
countEqualsChars (x : xs) = length (x : xs)

-------------------------------------------------------------------

-- Função de decodificar uma String
decodificar :: String -> String
decodificar [] = []
decodificar (x : xs)
  | isDigit x && nextDigitIsNotDigit = decodificarAux (str2int digit) (firstItemOfList xs) ++ decodificar xs -- Caso só um dígito
  | isDigit x && nextDigitIsDigit = decodificarAux (str2int digit) (firstItemOfList (nextItemOfList xs)) ++ decodificar (nextItemOfList xs) -- Caso mais de um dígito
  | otherwise = decodificar xs
  where
    nextDigitIsDigit = isDigit (firstCharOfList xs) -- Verifica se o próximo char depois do primeiro dígito é um número
    nextDigitIsNotDigit | nextDigitIsDigit = False | otherwise = True -- Verifica se o próximo char depois do primeiro dígito não é um número
    digit
      | nextDigitIsDigit = [x] ++ firstItemOfList xs -- Se o próximo char depois do primeiro dígito for um número, concatena o primeiro dígito com o próximo char
      | otherwise = [x] -- Se o próximo char depois do primeiro dígito não for um número, retorna apenas o primeiro dígito

-- Retorna o próximo item de uma lista
nextItemOfList :: String -> String
nextItemOfList [] = []
nextItemOfList (x : xs) = xs

-- Função auxiliar de decodificar uma String
decodificarAux :: Int -> String -> String
decodificarAux 0 cs = []
decodificarAux i c = c ++ decodificarAux (i - 1) c

-------------------------------------------------------------------

-- Funções auxiliares dadas no enunciado
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

int2str :: Int -> String
int2str i = show i

str2int :: String -> Int
str2int s = read s

-------------------------------------------------------------------
