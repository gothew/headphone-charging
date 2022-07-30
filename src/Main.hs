{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import DBus
import DBus.Socket
import Data.Int
import Data.List (intercalate)
import Data.Word
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Bus = Session | System
  deriving (Show)

data Option = BusOption Bus | AddressOption String
  deriving (Show)

optionInfo :: [OptDescr Option]
optionInfo =
  [ Option
      []
      ["session"]
      (NoArg (BusOption Session))
      "Monitor the session message bus. (default)",
    Option
      []
      ["system"]
      (NoArg (BusOption System))
      "Monitor the system message bus.",
    Option
      []
      ["address"]
      (ReqArg AddressOption "ADDRESS")
      "Connect to a particular bus address."
  ]

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...]"

findSocket :: [Option] -> IO Socket
findSocket opts = getAddress opts >>= open
  where
    session = do
      got <- getSessionAddress
      case got of
        Just addr -> return addr
        Nothing -> error "DBUS_SESSION_BUS_ADDRESS is not a valid address"

    system = do
      got <- getSystemAddress
      case got of
        Just addr -> return addr
        Nothing -> error "DBUS_SYSTEM_BUS_ADDRESS is not a valid address"

    getAddress [] = session
    getAddress ((BusOption Session) : _) = session
    getAddress ((BusOption System) : _) = system
    getAddress ((AddressOption addr) : _) = case parseAddress addr of
      Nothing -> error (show addr ++ " is not a valid address")
      Just parsed -> return parsed

addMatch :: Socket -> String -> IO ()
addMatch sock match =
  send
    sock
    (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "AddMatch")
      { methodCallDestination = Just "org.freedesktop.DBus",
        methodCallBody = [toVariant match]
      }
    (\_ -> return ())

defaultFilters :: [String]
defaultFilters =
  [ "type='signal',eavesdrop=true",
    "type='method_call,eavesdrop=true",
    "type='method_return,eavesdrop=true",
    "type='error',eavesdrop=true"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (options, userFilters, errors) = getOpt Permute optionInfo args
  unless (null errors) $ do
    name <- getProgName
    hPutStrLn stderr (concat errors)
    hPutStrLn stderr (usageInfo (usage name) optionInfo)
    exitFailure

  sock <- findSocket options

  send
    sock
    (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
      { methodCallDestination = Just "org.freedesktop.DBus"
      }
    (\_ -> return ())

  mapM_ (addMatch sock) (if null userFilters then defaultFilters else userFilters)

  forever $ do
    received <- receive sock
    putStrLn (formatMessage received ++ "\n")

formatMessage :: ReceivedMessage -> String
formatMessage (ReceivedMethodCall serial msg) =
  concat
    [ "method call",
      " sender=",
      maybe "(null)" formatBusName (methodCallSender msg),
      " -> dest=",
      maybe "(null)" formatBusName (methodCallDestination msg),
      " serial=",
      show (serialValue serial),
      " path=",
      formatObjectPath (methodCallPath msg),
      "; interface=",
      maybe "(null)" formatInterfaceName (methodCallInterface msg),
      "; member=",
      formatMemberName (methodCallMember msg),
      formatBody (methodCallBody msg)
    ]

formatBody :: [Variant] -> String
formatBody body = formatted
  where
    tree = Children (map formatVariant body)
    formatted = intercalate "\n" ("" : collapseTree 0 tree)

-- A string tree allows easy indentation of nested structures
data StringTree = Line String | MultiLine [StringTree] | Children [StringTree]
  deriving (Show)

collapseTree :: Int -> StringTree -> [String]
collapseTree d (Line x) = [replicate (d * 3) ' ' ++ x]
collapseTree d (MultiLine xs) = concatMap (collapseTree d) xs
collapseTree d (Children xs) = concatMap (collapseTree (d + 1)) xs

formatVariant :: Variant -> StringTree
formatVariant x = case variantType x of
  TypeBoolean ->
    Line $
      let Just x' = fromVariant x
       in "boolean " ++ if x' then "true" else "false"
  TypeWord8 ->
    Line $
      let Just x' = fromVariant x
       in "byte " ++ show (x' :: Word8)
  TypeWord16 ->
    Line $
      let Just x' = fromVariant x
       in "uint16 " ++ show (x' :: Word16)
  TypeWord32 ->
    Line $
      let Just x' = fromVariant x
       in "uint32 " ++ show (x' :: Word32)
  TypeWord64 ->
    Line $
      let Just x' = fromVariant x
       in "uint64 " ++ show (x' :: Word64)
  TypeInt16 ->
    Line $
      let Just x' = fromVariant x
       in "int16 " ++ show (x' :: Int16)
  TypeInt32 ->
    Line $
      let Just x' = fromVariant x
       in "int32 " ++ show (x' :: Int32)
  TypeInt64 ->
    Line $
      let Just x' = fromVariant x
       in "int64 " ++ show (x' :: Int64)
  TypeDouble ->
    Line $
      let Just x' = fromVariant x
       in "double " ++ show (x' :: Double)
  TypeString ->
    Line $
      let Just x' = fromVariant x
       in "string " ++ show (x' :: String)
  TypeObjectPath ->
    Line $
      let Just x' = fromVariant x
       in "object path " ++ show (formatObjectPath x')
  TypeSignature ->
    Line $
      let Just x' = fromVariant x
       in "signature " ++ show (formatSignature x')
  TypeArray _ ->
    MultiLine $
      let Just x' = fromVariant x
          items = arrayItems x'
          lines' =
            [ Line "array [",
              Children (map formatVariant items),
              Line "]"
            ]
       in lines'
  TypeDictionary _ _ ->
    MultiLine $
      let Just x' = fromVariant x
          items = dictionaryItems x'
          lines' =
            [ Line "dictionary {",
              Children (map formatItem items),
              Line "}"
            ]
          formatItem (k, v) = MultiLine (firstLine : vTail)
            where
              Line k' = formatVariant k
              v' = collapseTree 0 (formatVariant v)
              vHead = head v'
              vTail = map Line (tail v')
              firstLine = Line (k' ++ " -> " ++ vHead)
       in lines'
  TypeStructure _ ->
    MultiLine $
      let Just x' = fromVariant x
          items = structureItems x'
          lines' =
            [ Line "struct (",
              Children (map formatVariant items),
              Line ")"
            ]
       in lines'
  TypeVariant ->
    let Just x' = fromVariant x
     in MultiLine [Line "variant", Children [formatVariant x']]
