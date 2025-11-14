-- EntradasIO.hs
module IOnodes(
    iOconstruirItem,
    mostrarInventario,
    iOremove
) where
import Types 
import Text.Read (readMaybe)
import Data.Char (isDigit, isAlpha, isSpace)
import Puras -- (manipularIncremento, removerItem, etc.)
import Data.Char (toLower)
import qualified Data.Map as M
import System.IO (hFlush, stdout)

mostrarCategorias :: IO ()
mostrarCategorias = do
    let categoriasValidas = init ([minBound .. maxBound] :: [Categoria])
    mapM_ putStrLn $
        zipWith (\i c -> show i ++ " -> " ++ show c) [1..] categoriasValidas


lerCategoria :: IO Categoria
lerCategoria = do
    let categorias = init ([minBound .. maxBound] :: [Categoria])  -- remove Invalida
    putStrLn "Escolha a categoria:"
    mostrarCategorias
    putChar '$'
    hFlush stdout
    entrada <- getLine
    case reads entrada of
        [(n, "")] | n >= 1 && n <= length categorias ->
            return (categorias !! (n - 1))
        _ -> do
            return Invalida
    
lerInt :: IO Int
lerInt = do
    hFlush stdout
    entrada <- getLine
    case readMaybe entrada of
        Just n -> return n
        
        Nothing -> do
            putStrLn "Entrada invalida. Digite novamente:"
            lerInt



nomeValido :: String -> Bool
nomeValido string =
    not (null string)
    && not (all isSpace string)
    && any isAlpha string  -- tem pelo menos uma letra
    && not (length string < 3)

lerNome :: IO String
lerNome = do
    hFlush stdout
    entrada <- getLine
    validar entrada
  where
    validar s
        | nomeValido s = return s
        | otherwise = do
            putStrLn "Nome invalido. Tente novamente."
            lerNome


iOconstruirItem :: String -> IO Item
iOconstruirItem itemID = do
    putStrLn "<-----ADD----->"
    putStrLn "Digite o nome do item"
    putChar '$'
    hFlush stdout
    nome <- getLine
    let nomeMinusculo = map toLower nome
    putStrLn "Digite a quantidade que deseja inserir no inventario:"
    putChar '$'
    hFlush stdout
    quantidade <- lerInt 
    categoria <- lerCategoria
    putStrLn $ "Categoria escolhida: " ++ show categoria
    
    let itemConstruido = Item itemID nomeMinusculo quantidade categoria 
    return itemConstruido
    
iOremove :: IO (String, Int)
iOremove = do
    putStrLn "<----- REMOVER ----->"
    putStrLn "Digite o ID do item que deseja remover:"
    putStr "$ "
    hFlush stdout
    itemID <- getLine
    
    putStr "Digite a quantidade que deseja remover:"
    hFlush stdout
    quantidade <- lerInt
    return (itemID, quantidade)


mostrarInventario :: Inventario -> IO ()
mostrarInventario inventario = do
    putStrLn "\n<=-=-= INVENTÁRIO =-=-=>"
    if M.null inventario
        then putStrLn "Inventário vazio."
        else mapM_ mostrarItem (M.elems inventario)
    putStrLn "<=======================>\n"

mostrarItem :: Item -> IO ()
mostrarItem item = do
    putStrLn $ "ID:         " ++ itemID item
    putStrLn $ "Nome:       " ++ nome item
    putStrLn $ "Quantidade: " ++ show (quantidade item)
    putStrLn $ "Categoria:  " ++ show (categoria item)
    putStrLn "<----------------------->"

    
    
