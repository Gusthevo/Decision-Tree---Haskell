-- Aqui só definimos o tipo de dado da árvore
data BinTree a = Leaf a | Node a (BinTree a) (BinTree a)

-- Árvore de decisão para definir o que é nó de cada qual
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

-- Aqui vamos navegar pela árvore baseado na decisão
askQuestion :: BinTree String -> IO String
askQuestion (Leaf result) = return result
askQuestion (Node question left right) = do
    putStrLn question
    answer <- getLine
    if answer == "SIM" then askQuestion left
    else if answer == "NÃO" then askQuestion right
    else do
        putStrLn "Entrada inválida! Responda com SIM ou NÃO."
        askQuestion (Node question left right)

-- Função principal para executar o programa interativo
main :: IO ()
main = do
    result <- askQuestion decisionTree
    putStrLn $ "Resultado: " ++ result
    -- Aqui vai continuar pra sempre enquanto não digitar sair
    putStrLn "Digite 'sair' para terminar ou pressione Enter para continuar."
    continue <- getLine
    if continue /= "sair"
        then main
        else putStrLn "Programa encerrado."
