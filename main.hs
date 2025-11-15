-- Main.hs
module Main where

import Types -- Nossos tipos (Item, Inventario, LogEntry, ResultadoOperacao)
import qualified Data.Map as M
import System.IO (readFile, writeFile, appendFile, hFlush, stdout)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe) -- Ótimo para validar entrada e ler arquivo vazio
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import IOnodes
import Data.Maybe (mapMaybe)
import Data.Time.Clock (getCurrentTime)
import Puras
import InventarioInicial
import Analise

criarArquivo :: FilePath -> IO ()
criarArquivo nome = do
    putStrLn ("Gerando o arquivo: " ++ nome)
    threadDelay 500000
    writeFile nome ""
    putStrLn "Arquivo gerado com sucesso!"
    

carregarInventario :: IO Inventario
carregarInventario = do 
    putStrLn ("\nCarregando inventario -> (Inventario.dat)")
    conteudo <- readFile "Inventario.dat" `catch` tratarErroLeitura
    threadDelay 500000
    case readMaybe conteudo of 
        Just inv -> return inv
        
        Nothing -> do 
            putStrLn "Inventario vazio. Um novo será gerado!"
            return M.empty -- Retorna um map vazio, mais facil para trabalhar
    where
      tratarErroLeitura :: IOException -> IO String
      tratarErroLeitura e
        | isDoesNotExistError e = do 
            putStrLn "Arquivo: Inventario.dat, não encontrado."
            threadDelay 500000
            criarArquivo "Inventario.dat"
            return ""
        | otherwise = ioError e


carregarLogs :: IO [LogEntry]
carregarLogs = do 
    putStrLn ("\nCarregando logs -> (Auditoria.log)")
    conteudo <- readFile "Auditoria.log" `catch` tratarErroLeitura
    case mapM readMaybe (lines conteudo) of 
        Just logs -> return logs 
        
        Nothing -> do
            putStrLn "Auditoria.log corrompido. Uma nova lista será gerada!."
            threadDelay 500000
            return []
  where
    tratarErroLeitura :: IOException -> IO String
    tratarErroLeitura e
      | isDoesNotExistError e = do
          putStrLn "Arquivo: Auditoria.log, não encontrado."
          threadDelay 500000
          criarArquivo "Auditoria.log"
          return ""
      | otherwise = ioError e
      
proximoItemID :: Inventario -> String
proximoItemID inv
    | M.null inv = "1"
    | otherwise  =
        let chaves = M.keys inv                -- [String]
            nums   = mapMaybe readMaybe chaves :: [Int]
            maior  = if null nums then 0 else maximum nums
        in show (maior + 1)
        
main :: IO ()
main = do
    putStrLn "<---Bem vindo ao sistema de inventário em Haskell!--->"
    threadDelay 500000

    invCarregado <- carregarInventario
    logsIniciais <- carregarLogs

    case M.null invCarregado of
        True -> do
            -- inventário vazio, passa itemID como "1" e popula 10 itens
            -- Se quiser mais itens, adicione manualmente no InventarioInicial.hs
            -- WARNING: 10 é o minímo para a autoria
            putStrLn "Criando e populando inventário inicial..."
            let inventarioVazio = M.empty
            (idFinal, invFinal, logsFinal) <-
                popularInventarioInicial "1" inventarioVazio logsIniciais

            putStrLn "Inventário inicial populado com 10 itens!"
            loopPrincipal idFinal invFinal logsFinal

        False -> do
            -- inventario existente
            putStrLn "Inventário carregado com sucesso!"
            let idInicial = proximoItemID invCarregado
            loopPrincipal idInicial invCarregado logsIniciais


-- loop para prompt de Usuario.
loop :: String -> Inventario -> [LogEntry] -> IO AcaoSistema
loop itemID invAtual logsAtuais = do
    putStrLn "\n<=-=-=Interface=-=-=>"
    putStrLn "[1]Add\n[2]Remove\n[3]View\n[4]Sair\n[report] Relatórios"
    putChar '$'
    hFlush stdout

    entrada <- map toLower <$> getLine
    let partes = words entrada

    case partes of  
        [cmd]
            | cmd `elem` ["4", "sair", "exit"] ->
                return Sair

            | cmd `elem` ["1", "add", "adicionar"] -> do
                novoItem <- iOconstruirItem itemID
                tempo <- getCurrentTime
                let resultado = manipularIncremento novoItem invAtual tempo
                case resultado of
                    Right res -> do
                        putStrLn "\nItem adicionado com sucesso!"
                        return (AcaoSucesso res)

                    Left logF -> do
                        putStrLn "\nFalha ao executar operação!"
                        putStrLn ("Motivo: " ++ show logF)
                        return (AcaoFalha logF)
                        


            | cmd `elem` ["2", "remove", "remover"] -> do
                mostrarInventario invAtual
                (itemIDrem, quantidade) <- iOremove
                tempo <- getCurrentTime
                let resultado = removerItem itemIDrem quantidade invAtual tempo
                case resultado of
                    Right res -> do
                        putStrLn "\nItem removido com sucesso!"
                        return (AcaoSucesso res)

                    Left logF -> do
                        putStrLn "\nFalha ao executar operação!"
                        putStrLn ("Motivo: " ++ show logF)
                        return (AcaoFalha logF)


            | cmd `elem` ["3", "view", "ver"] -> do
                mostrarInventario invAtual
                return AcaoVazia

            | cmd `elem` ["report", "relatorio"] -> do
                putStrLn "\n<=-= RELATORIOS =-=>"
                
                -- Logs de erro
                let erros = logsDeErro logsAtuais
                putStrLn $ "\nLogs de erro (" ++ show (length erros) ++ "):"
                mapM_ (putStrLn . show) erros

                -- Item mais movimentado
                case itemMaisMovimentado logsAtuais of
                    Just idItem -> putStrLn $ "\nItem mais movimentado - ID: " ++ idItem
                    Nothing     -> putStrLn "\n...Nenhum item movimentado..."

                -- Histórico de um item digitado pelo usuario
                putStrLn "\nDigite o ID de um item para ver histórico (Enter para cancelar):"
                putChar '$'
                hFlush stdout
                idHistorico <- getLine
                if null idHistorico
                    then return ()
                    else do
                        let hist = historicoPorItem idHistorico logsAtuais
                        putStrLn $ "\nHistorico do item|ID:" ++ idHistorico ++ " (" ++ show (length hist) ++ " registros):"
                        mapM_ (putStrLn . show) hist

                return AcaoVazia
                
            | otherwise ->
                return AcaoVazia
        _ ->
            return AcaoVazia




loopPrincipal :: String -> Inventario -> [LogEntry] -> IO ()
loopPrincipal itemID invAtual logsAtuais = do
    acao <- loop itemID invAtual logsAtuais

    case acao of
        Sair -> do
            putStr "\nSaindo do sistema"
            threadDelay 500000
            putStr "..."
            threadDelay 500000
            writeFile "Inventario.dat" (show invAtual)
            -- A linha de baixo está comentada, deveria ser write(Não sei se é permitido)
            --appendFile "Auditoria.log" (unlines (map show logsAtuais))
            putStrLn "Estado salvo.\n"

        AcaoVazia ->
            loopPrincipal itemID invAtual logsAtuais

        AcaoFalha logEntry -> do
            appendFile "Auditoria.log" (show logEntry ++ "\n")
            loopPrincipal itemID invAtual (logsAtuais ++ [logEntry])

        AcaoSucesso (novoInv, logEntry) -> do
            appendFile "Auditoria.log" (show logEntry ++ "\n")
            writeFile "Inventario.dat" (show novoInv)
            let novoID = incrementarID itemID
            loopPrincipal novoID novoInv (logsAtuais ++ [logEntry])
            
            
            
            