module Analise (
    historicoPorItem,
    itemMaisMovimentado,
    logsDeErro
) where 
-- Todo código foi importado do VIM -> A identação deverá ser corrigida.
import qualified Data.Map as M
import Data.List (maximumBy)
import Data.Ord (comparing)
import Puras
import Types
import Data.Char (toLower)
import Data.Maybe
import Data.List


historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId logs =
    filter (\logElemento -> elem itemId (pegarDetalhesId (detalhe logElemento))) logs
  where -- Usa função lambda com filter, mais simples que a versão anterior
    pegarDetalhesId = words . map toLower
    
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro logs = filter isFalha logs
  where
    isFalha logElemento = case status logElemento of
                                    Falha _ -> True
                                    _       -> False

itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado logs =
    if M.null contagem then Nothing else Just (fst $ maximumBy (comparing snd) (M.toList contagem))
  where
    -- extrair ID de cada log que não seja QueryFail
    ids = mapMaybe (extrairID . detalhe) logs --primeiro pega detalhe do log usando o .

    -- contar ocorrências de cada ID
    contagem = M.fromListWith (+) [(i, 1) | i <- ids]

    -- extrair o id do detalhe
    extrairID :: String -> Maybe String
    extrairID detalhe = case words detalhe of
                    listaDePalavras -> find (\palavra -> all (`elem` "0123456789") palavra) listaDePalavras