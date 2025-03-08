{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Tree as T
import Data.Tree (drawTree)
import Data.Char (toUpper)
import Data.List (isPrefixOf, dropWhileEnd)

-- Definição da árvore binária
data BinTree a = Leaf a | Node a (BinTree a) (BinTree a)

-- Árvore de decisão
decisionTree :: BinTree String
decisionTree = 
    Node "Fez Poscomp?" 
        (Node "Nota do IRA >= 7?" 
            (Node "Nota da Entrevista >= 7?"
                (Node "Nota da Prova de Títulos >= 5?"
                    (Leaf "Classificado")
                    (Leaf "Eliminado"))
                (Node "Nota da Prova de Títulos >= 7?"
                    (Leaf "Classificado")
                    (Leaf "Eliminado")))
            (Node "Nota da Entrevista >= 9?" 
                (Node "Nota da Prova de Títulos >= 7?" 
                    (Leaf "Classificado")
                    (Leaf "Eliminado"))
                (Node "Nota da Prova de Títulos >= 8?"
                    (Leaf "Classificado")
                    (Leaf "Eliminado"))))
        (Node "Nota do IRA >= 8?"
            (Node "Nota da Entrevista >= 8?" 
                (Node "Nota da Prova de Títulos >= 8?" 
                    (Leaf "Classificado")
                    (Leaf "Eliminado"))
                (Leaf "Eliminado"))
            (Leaf "Eliminado"))


-- Converte nossa árvore para uma árvore do módulo Data.Tree, 
-- incluindo os parenteses para representar cada nó
toDataTree :: BinTree String -> T.Tree String
toDataTree (Leaf s)      = T.Node ("(" ++ s ++ ")") []
toDataTree (Node s l r)  = T.Node ("(" ++ s ++ ")") [toDataTree l, toDataTree r]

-- Funções de normalização de entrada
trim :: String -> String
trim = dropWhileEnd (==' ') . dropWhile (==' ')

normalize :: String -> String
normalize = map toUpper . trim

isYes :: String -> Bool
isYes ans = any (`isPrefixOf` normalize ans) ["SIM", "S", "Y", "YES"]

isNo :: String -> Bool
isNo ans = any (`isPrefixOf` normalize ans) ["NAO", "N", "NO"]

-- Função que navega pela árvore interativamente e mostra o desenho atual
askQuestion :: BinTree String -> IO String
askQuestion tree = do
    putStrLn "\n--- Arvore atual ---"
    -- Exibe o desenho da árvore usando Data.Tree.drawTree
    putStrLn $ drawTree (toDataTree tree)
    case tree of
        Leaf result -> return result
        Node question left right -> do
            putStrLn question
            answer <- getLine
            if isYes answer then askQuestion left
            else if isNo answer then askQuestion right
            else do
                putStrLn "Entrada invalida! Responda com uma variacao de SIM ou NAO."
                askQuestion tree

-- Função principal para executar o programa interativo
main :: IO ()
main = do
    result <- askQuestion decisionTree
    putStrLn $ "Resultado: " ++ result
    putStrLn "Digite 'sair' para terminar ou pressione Enter para reiniciar."
    continue <- getLine
    if normalize continue /= "SAIR"
        then main
        else putStrLn "Programa encerrado."
