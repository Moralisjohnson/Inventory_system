-- Puras.hs
module Puras (
    manipularIncremento,
    removerItem,
    incrementarID
) where

import qualified Data.Map as M
import Types
import Data.Time.Clock (UTCTime)
import Data.List (find)
import Data.Char (isDigit, isAlpha, isSpace)

buscarItemPorNome :: String -> Inventario -> Maybe Item
buscarItemPorNome nomeBuscado = 
    find (\item -> nome item == nomeBuscado) . M.elems

addItem :: Item -> Inventario -> Inventario
addItem itemNovo invVelho = M.insert (itemID itemNovo) itemNovo invVelho

updateQty :: String -> Int -> Inventario -> Inventario
updateQty itemID novaQuantidade inventarioAntigo =
    M.adjust (\item -> item {quantidade = novaQuantidade}) itemID inventarioAntigo

buscarItem :: String -> Inventario -> Maybe Item
buscarItem itemID inventario = M.lookup itemID inventario


incrementarID :: String -> String
incrementarID s =
    let n = read s :: Int
    in show (n + 1)
    
-- TODO: registrar quantidade negativa como falha no log 
manipularIncremento :: Item -> Inventario -> UTCTime -> Either LogEntry ResultadoOperacao
manipularIncremento item inventarioAntigo tempoAtual =
    case itemValidotor item of -- Chama a função para decidirmos left ou right
        Just erroMsg ->
            Left LogEntry
                { timestamp = tempoAtual
                , acao      = QueryFail
                , detalhe   = "Criterio Invalido: " ++ erroMsg
                , status    = Falha "Ao registrar."
                }

        Nothing ->
            case buscarItemPorNome (nome item) inventarioAntigo of
                Nothing ->
                    let novoInv = addItem item inventarioAntigo
                        logA = LogEntry
                            { timestamp = tempoAtual
                            , acao = Add
                            , detalhe = "Novo item|ID: " ++ itemID item
                            , status = Sucesso
                            }
                    in Right (novoInv, logA)

                Just itemExistente ->
                    let novaQuantidade = quantidade itemExistente + quantidade item
                        novoInv = updateQty (itemID itemExistente)
                                                      novaQuantidade
                                                      inventarioAntigo
                        logB = LogEntry
                            { timestamp = tempoAtual
                            , acao = Update
                            , detalhe = "Quantidade atualizada|ID:" ++ itemID item
                            , status = Sucesso
                            }
                    in Right (novoInv, logB)

-- Como escolha de design, um item nunca é apagado do sistema
-- A ideia é que, a quantidade minima dele seja 0, mas ele nunca será apagado de fato
-- (Implementar um delete com M.Delete seria simples), mas a documentação não pede exatamente.
removerItem :: String -> Int -> Inventario -> UTCTime -> Either LogEntry ResultadoOperacao
removerItem itemID qtdRemover inventarioAntigo tempoAtual =
    case buscarItem itemID inventarioAntigo of

        -- Iitem não existe
        Nothing ->
            let logA = LogEntry
                        { timestamp = tempoAtual
                        , acao      = QueryFail
                        , detalhe   = "Busca item ID: " ++ itemID
                        , status    = Falha "Item nao encontrado no sistema"
                        }
            in Left logA

        -- Item encontrado
        Just itemEncontrado ->
            case validarRemocao itemEncontrado qtdRemover of

                -- Remoção inválida (quantidade <=0 ou acima do estoque)
                Just erroMsg ->
                    let logErro = LogEntry
                            { timestamp = tempoAtual
                            , acao      = QueryFail
                            , detalhe   = "Remocao invalida do item ID: " ++ itemID
                            , status    = Falha erroMsg
                            }
                    in Left logErro

                -- Remoção válida
                Nothing ->
                    let novaQuantidade = quantidade itemEncontrado - qtdRemover

                        novoInv =
                            updateQty itemID novaQuantidade inventarioAntigo

                        logSucesso = LogEntry
                            { timestamp = tempoAtual
                            , acao      = Remove
                            , detalhe   = "Remocao do item ID: " ++ itemID
                            , status    = Sucesso
                            }

                    in Right (novoInv, logSucesso)


itemValidotor :: Item -> Maybe String
itemValidotor item
    | null (nome item) || all isSpace (nome item)
        = Just "Nome invalido."
    | length (nome item) < 3
        = Just "Nome menor que 3 caracteres."
    | quantidade item <= 0
        = Just "Quantidade menor igual a zero."
    | categoria item == Invalida
        = Just "Categoria Invalida."
    | otherwise = Nothing -- O item está correto.

validarRemocao :: Item -> Int -> Maybe String
validarRemocao item qtd
    | qtd <= 0
        = Just "A quantidade deve ser maior que zero."
    | qtd > quantidade item
        = Just "Quantidade solicitada excede estoque."
    | otherwise = Nothing




