-- Types.hs
module Types (
    Item(..),    -- Exporta o tipo E seus campos (itemID, nome...)
    Inventario,  -- É um alias, nao precisa de (..)
    AcaoLog(..), -- Exporta o tipo E seus construtores (Add, Remove...)
    StatusLog(..),
    LogEntry(..),
    ResultadoOperacao,
    Categoria(..),
    AcaoSistema(..)
) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Time.Clock (UTCTime)

-- Item possui muitos campos, logo será utilizado a sintaxe de registro
data Item = Item { -- <- O Construtor é chamado de Item por convenção
    itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: Categoria
} deriving(Read, Show)


type Inventario =  Map String Item -- Data.map -> Dicionario(String, Item) Type -> alias
data AcaoLog = Add | Remove | Update | QueryFail -- cada item é um construtor 
    deriving(Read, Show)
data StatusLog = Sucesso | Falha String -- O construtor falha possui argumento
    deriving(Read, Show)
data LogEntry = LogEntry {
    timestamp :: UTCTime,
    acao :: AcaoLog,
    detalhe :: String,
    status :: StatusLog
} deriving(Read, Show)

type ResultadoOperacao = (Inventario, LogEntry)

data Categoria = Ferramenta | Comida | Eletronico | Outro | Invalida
    deriving (Show, Read, Enum, Bounded, Eq)

data AcaoSistema
    = Sair
    | AcaoVazia
    | AcaoSucesso ResultadoOperacao   -- Right 
    | AcaoFalha LogEntry              -- Left

