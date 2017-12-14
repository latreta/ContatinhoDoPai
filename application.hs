import System.IO
import System.Directory
import Data.List

main = do
    putStrLn "Seja Bem vindo ao Menu da Agenda Telefonica: \n 1.Inserir número. \n 2.Remover número. \n 3.Listar números. \n 4.Procurar contato. \n"
    c <- getLine
    menuAgenda c

menuAgenda c | c == "1" = do
                    putStrLn "Digite o nome e numero a ser adicionado: (nome - numero) "
                    insere
                    main
             | c == "2" = do
                    remover
                    listar_Arquivo
                    main
             | c == "3" = do
                    listar_Arquivo
                    main
             | c == "4" = do
                    procurar
                    main
             | otherwise = main

insere = do
    todoItem <- getLine
    appendFile "agenda.txt" (todoItem ++ "\n")
    putStrLn "Número adicionado"

listar_Arquivo = do
    arquivoLido <- openFile "agenda.txt" ReadMode
    conteudo <- hGetContents arquivoLido
    let contatos = lines conteudo
        contatosEnumerados = zipWith (\n line -> show n ++ " - " ++ line) [0..] contatos
    putStrLn "Esses são os contatos da agenda:"
    putStr $ unlines contatosEnumerados

remover = do
    arquivo <- openFile "agenda.txt" ReadMode
    dirtempo <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile dirtempo "diretorioTemporario"
    conteudo <- hGetContents arquivo
    let contatos = lines conteudo
        contatosEnumerados = zipWith (\n line -> show n ++ " - " ++ line) [0..] contatos
    putStr $ unlines contatosEnumerados
    putStrLn "Qual número você deseja excluir?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (contatos !! number) contatos
    hPutStr tempHandle $ unlines newTodoItems
    hClose arquivo
    hClose tempHandle
    removeFile "agenda.txt"
    renameFile tempName "agenda.txt"

procurar = do
    -- Lê o conteudo do arquivo
    arquivo <- openFile "agenda.txt" ReadMode
    conteudo <- hGetContents arquivo
    -- Lines separa as linhas em strings numa lista
    let contatos = lines conteudo
    -- Recebe o nome pesquisado
    putStrLn "Digite o nome a consultar:"
    name <- getLine
    let consultado = comparar name contatos
    putStr "O contato "
    putStr consultado
    putStr "\n"

comparar nome [] = "Não foi encontrado."
comparar nome (h1:t)
          | any(==nome) (inits h1) = h1
          | any(/=nome) (inits h1) = comparar nome t
          | otherwise = error "Nome nao encontrado"
--comparar nome (h1:t) = if any (==nome) (inits h1) then h1 else comparar nome t
