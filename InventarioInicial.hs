module InventarioInicial (
    popularInventarioInicial
) where

import Types
import Data.Time.Clock (getCurrentTime)
import Puras

popularInventarioInicial :: String -> Inventario -> [LogEntry] -> IO (String, Inventario, [LogEntry])

popularInventarioInicial itemID invAtual logsAtuais = 
    inserirEmCascata itemID invAtual logsAtuais itensIniciais
  where
    inserirEmCascata idAtual invent logs [] =
        return (idAtual, invent, logs)

    inserirEmCascata idAtual invent logs (itemBase:resto) = do
        tempo <- getCurrentTime

        -- Substitui o ID do item pelo ID atual do sistema
        let item = itemBase { itemID = idAtual }

        case manipularIncremento item invent tempo of
            Right (novoInv, logEntry) -> do
                appendFile "Auditoria.log" (show logEntry ++ "\n")
                writeFile  "Inventario.dat" (show novoInv)
                let novoID = incrementarID idAtual
                inserirEmCascata novoID novoInv (logs ++ [logEntry]) resto

            Left logF -> do
                appendFile "Auditoria.log" (show logF ++ "\n")
                inserirEmCascata idAtual invent (logs ++ [logF]) resto

itensIniciais :: [Item]
itensIniciais =
    [ Item "" "martelo"        5 Ferramenta
    , Item "" "maca"          10 Comida
    , Item "" "teclado"        2 Eletronico
    , Item "" "chave de fenda" 6 Ferramenta
    , Item "" "pao"            7 Comida
    , Item "" "mouse"          4 Eletronico
    , Item "" "alicate"        3 Ferramenta
    , Item "" "agua"           8 Comida
    , Item "" "carregador"     3 Eletronico
    , Item "" "serrote"        4 Ferramenta
    ]

