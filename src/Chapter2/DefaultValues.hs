module Chapter2.DefaultValues where

type URL = String

data ConnType = TCP | UDP

data UseProxy = NoProxy | Proxy String

data TimeOut = NoTimeOut | TimeOut Integer

data Connection = Connection

connect ::
  URL ->
  ConnType ->
  Integer ->
  UseProxy ->
  Bool ->
  Bool ->
  TimeOut ->
  Connection
connect = undefined

connectUrl :: URL -> Connection
connectUrl u = connect u TCP 0 NoProxy False False NoTimeOut

data ConnOptions = ConnOptions
  { connType :: ConnType,
    connSpeed :: Integer,
    connProxy :: UseProxy,
    connCaching :: Bool,
    connKeepAlive :: Bool,
    connTimeOut :: TimeOut
  }

connect' :: URL -> ConnOptions -> Connection
connect' _ _ = undefined

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut
