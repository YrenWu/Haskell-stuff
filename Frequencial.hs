-- calcule le nombre de lettres dans une chaine pour une lettre
countLetter' :: Int -> Char -> String -> Int
countLetter' tot _ [] = tot
countLetter' tot char (x:xs)
    | x /= char = countLetter' tot char xs
    | otherwise = countLetter' (tot+1) char xs

countLetter :: Char -> String -> Int
countLetter char list = countLetter' 0 char list

-- calcule le nombre de lettres présentes dans la chaine (minuscules et majuscules séparées)
alphaMin :: String -> [(Char, Int)]
alphaMin str = [(a, countLetter a str) | a <- ['a'..'z']]

alphaMaj :: String -> [(Char, Int)]
alphaMaj str = [(a, countLetter a str) | a <- ['A'..'Z']]

-- total majuscule et minuscules comprises
total :: String -> [Int]
total str = zipWith (+) [countLetter a str | a <- ['a'..'z']] 
    [countLetter a str | a <- ['A'..'Z']]

-- affichage du total avec la lettre associée
total' :: String -> [(Char, Int)]
total' str = zip ['A' .. 'Z'] (total str)

-- pourcentage du nombre de lettres dans une chaine
percent :: String -> [Double]
percent str = [(fromIntegral(n*100))
    /(fromIntegral(length str)) | n <- total str]

-- affichage du pourcentage avec la lettre associée
percent' :: String -> [(Char, Double)]
percent' str = zip ['A' .. 'Z'] (percent str)

-- calcul de l'indice de coincidence d'une chaine
friedman :: String -> Double
friedman "" = 0.0
friedman str = fromIntegral(sum [n^2 | n <- total str])
    /fromIntegral((sum [a | a <- total  str])^2)

showDetails :: String -> [String]    
showDetails str = [(\l -> ("-> " ++ show (fst l) ++ " : " ++ show (snd l))) 
    t | t <- total' str]
    -- l est une lambda équivalente à showDetails'


showDetails' :: (Char, Int) -> String
showDetails' tuple = "-> " ++ show (fst tuple) 
    ++ " : " ++ show (snd tuple) 

country :: String -> String
country str = country' (friedman str)

country' :: Double -> String
country' ic 
    | ic > 0.025 && ic < 0.04   = "Polyalphabetic cipher or random letters ?"
    | ic > 0.04 && ic < 0.06    = "Russian maybe Serbian ?"
    | ic > 0.06 && ic < 0.065   = "Serbian, Swedish, maybe English ?"
    | ic > 0.065 && ic < 0.07   = "Esperanto, Greek, Norwegian maybe Danish ?"
    | ic > 0.07 && ic < 0.075   = "Danish, Finnish, Italian, Portuguese maybe Arab ?"
    | ic > 0.075 && ic < 0.08   = "Arab, German, Hebrew, Spanish, Japanese, French, maybe Dutch ?"
    | ic > 0.08 && ic < 0.09    = "Dutch maybe Malaysian ?"
    | otherwise = "Can't recognize language or cipher"

-- todo déchiffrement vigenère
-- déchiffrement césar
-- déchiffrement substitution monoalphabétique
-- test de kasiski ?
-- longueur clé probable vigenère

-- Lister les lettres manquantes

-- Compter les bigrammmes (couples de 2 lettres)
-- Compter les trigrammmes (3 lettres)
-- Compter les N-grammes, N

-- Compter les lettres doublées (répétées 2 fois de suite)

-- Compter les longueur des mots
-- Proposer un déchiffrement statistique

-- stats détection chiffrement
-- analyse des digrammes

-- todo aller chercher le cyphertext dans un fichier?