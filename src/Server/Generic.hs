{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

-- | This library auto-generates API services for data types using Haskell's
-- built-in support for generic programming.  The best way to understand how
-- this library works is to walk through a few examples.
--
-- For example, suppose that you define the following type:
-- 
-- > -- Example.hs
-- >
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Server.Generic
-- >
-- > data Example = Example { foo :: Int, bar :: Double }
-- >     deriving (Generic)
-- >
-- > instance ParseRecord Example
-- > instance ToJSON      Example
-- >
-- > handler :: Example -> IO Example
-- > handler = return
-- >
-- > main :: IO ()
-- > main = serveJSON 8080 handler
--
-- Named fields translate to query parameters which you can provide in any
-- order:
--
-- > $ stack build server-generic
-- > $ stack runghc Example.hs
-- > ...
-- > {in another terminal}
-- > $ curl 'localhost:8080/example?bar=2.5&foo=1'
-- > {"foo":1,"bar":2.5}
--
-- `serveJSON` performs the following steps:
--
-- * automatically marshals the route into the `Example` data type
-- * supplies the `Example` data type to the @handler@
-- * converts the return value of the @handler@ to JSON served back to the
--   client
--
-- Let's write a more interesting handler that creates files:
--
-- > -- Create.hs
-- > 
-- > {-# LANGUAGE DeriveGeneric #-}
-- > 
-- > import Server.Generic
-- > 
-- > data Create = Create { filepath :: FilePath, contents :: String }
-- >     deriving (Generic)
-- > 
-- > instance ParseRecord Create
-- > 
-- > handler :: Create -> IO ()
-- > handler create = writeFile (filepath create) (contents create)
-- > 
-- > main :: IO ()
-- > main = serveJSON 8080 handler
--
-- If we run that then it will create a file any time we hit the @/create@
-- endpoint with the appropriate query parameters:
--
-- > $ curl 'localhost:8080/create?filepath=test.txt&contents=ABC'
-- > []
-- > $ cat test.txt
-- > ABC
--
-- The @[]@ in the response is just how the @aeson@ library encodes the empty
-- @()@ return value of the handler.
--
-- Unlabeled fields translate to path tokens in the route.  For example, this
-- type:
--
-- > -- Example.hs
-- > 
-- > {-# LANGUAGE DeriveGeneric #-}
-- > 
-- > import Server.Generic
-- > 
-- > data Example = Example Int Double Text
-- >     deriving (Generic)
-- > 
-- > instance ParseRecord Example
-- > instance ToJSON      Example
-- > 
-- > handler :: Example -> IO Example
-- > handler = return
-- > 
-- > main :: IO ()
-- > main = serveJSON 8080 handler
--
-- ... translates to a @\/example\/:int\/:double\/:text@ route that captures the
-- last three tokens as the fields of the @Example@ type:
--
-- > $ curl 'localhost:8080/example/1/2.5/foo'
-- > [1,2.5,"foo"]
--
-- Also, unlabeled data types get converted to an array when rendered as JSON.
--
-- Certain types of fields are given special treatment, such as in this example:
--
-- > data Example = Example
-- >     { list     :: [Int]
-- >     , optional :: Maybe   Int
-- >     , first    :: First   Int
-- >     , last     :: Last    Int
-- >     } deriving (Generic)
--
-- This gives the following behavior:
--
-- > $ curl 'localhost:8080/example?optional=1&list=1&list=2&first=1&first=2&last=1&last=2'
-- > {"list":[1,2],"first":1,"last":2,"optional":1}
-- > 
-- > $ curl 'localhost:8080/example'
-- > {"list":[],"first":null,"last":null,"optional":null}
--
-- If a datatype has multiple constructors:
--
-- > data Example
-- >     = Create { name :: Text, duration :: Maybe Int }
-- >     | Kill   { name :: Text }
-- >     deriving (Generic)
--
-- ... then they will translate into multiple API endpoints:
--
-- 
-- > $ curl 'localhost:8080/create?name=foo&duration=60'
-- > {"tag":"Create","name":"foo","duration":60}
-- > $ curl 'localhost:8080/kill?name=foo'
-- > {"tag":"Kill","name":"foo"}
--
-- This library also provides out-of-the-box support for many existing types,
-- like tuples and `Either`:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > 
-- > import Server.Generic
-- > 
-- > handler :: Either Int Double -> IO (Either Int Double)
-- > handler = return
-- > 
-- > main :: IO ()
-- > main = serveJSON 8080 handler
--
-- > $ curl 'localhost:8080/left/1'
-- > {"Left":1}
-- > $ curl 'localhost:8080/right/2.5'
-- > {"Right":2.5}
--
-- > handler :: (Int, Double) -> IO (Int, Double)
-- > handler = return
--
-- > $ curl 'localhost:8080/1/2.5'
-- > [1,2.5]
--
-- ... and you can also just parse a single value:
--
-- > handler :: Int -> IO Int
-- > handler = return
--
-- > $ curl 'localhost:8080/1'
-- > 1
--
-- However, there are some types that this library cannot generate sensible
-- routes for, such as:
--
-- * recursive types:
--
--     > data Example = Example { foo :: Example }
--
-- * records whose fields are other records
--
--     > data Outer = Outer { foo :: Inner } deriving (Show, Generic)
--     > data Inner = Inner { bar :: Int   } deriving (Show, Generic)
--
-- * record fields  with nested `Maybe`s or nested lists
--
--     > data Example = Example { foo :: Maybe (Maybe Int) }
--     > data Example = Example { foo :: [[Int]]           }
--
-- If you try to auto-generate a parser for these types you will get an error at
-- compile time that will look something like this:
--
-- >     No instance for (ParseFields TheTypeOfYourField)
-- >       arising from a use of ‘Server.Generic.$gdmparseRecord’
-- >     In the expression: Server.Generic.$gdmparseRecord
-- >     In an equation for ‘parseRecord’:
-- >         parseRecord = Server.Generic.$gdmparseRecord
-- >     In the instance declaration for ‘ParseRecord TheTypeOfYourRecord’

module Server.Generic (
    -- * Server
      serve
    , serveJSON
    , serveOK

    -- * Parser
    , Route(..)
    , Parser(..)
    , ParseRecord(..)
    , ParseFields(..)
    , ParseField(..)
    , Only(..)
    , getOnly

    -- * Re-exports
    , Generic
    , ToJSON
    , Data.Text.Text
    , All(..)
    , Any(..)
    , First(..)
    , Last(..)
    , Sum(..)
    , Product(..)
    ) where

import Control.Applicative
import Control.Monad (guard, MonadPlus)
import Control.Monad.State (MonadState(..), StateT)
import Data.Aeson (ToJSON)
import Data.Monoid
import Data.Void (Void)
import GHC.Generics
import Network.Wai (Response)
import Network.Wai.Handler.Warp (Port)

import qualified Control.Monad.State
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Network.HTTP.Types.Status
import qualified Network.HTTP.Types.URI
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai
import qualified Text.Read

-- | A list of path tokens which were originally separated by @'/'@s
data Route = Route
    { path  :: [Data.Text.Text]
    , query :: [(Data.ByteString.ByteString, Data.ByteString.ByteString)]
    } deriving (Show)

-- | A backtracking `Route` parser
newtype Parser a = Parser { unParser :: StateT Route [] a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , MonadState Route
    )

{-| A class for types that can be parsed from path tokens or query parameters

    This class has a default implementation for any type that implements
    `Generic` and you can derive `Generic` for many types by enabling the
    @DeriveGeneric@ language extension

    You can also use `getOnly` to create a `ParseRecord` instance from a
    `ParseFields` instance:

> instance ParseRecord MyType where
>     parseRecord = fmap getOnly parseRecord
-}
class ParseRecord a where
    parseRecord :: Parser a
    default parseRecord :: (Generic a, GenericParseRecord (Rep a)) => Parser a
    parseRecord = fmap to genericParseRecord

instance ParseFields a => ParseRecord (Only a) where
    parseRecord = fmap Only (parseFields Nothing)

instance ParseRecord Char where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Double where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Float where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Int where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Integer where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord () where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Any where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord All where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Data.ByteString.ByteString where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Data.ByteString.Lazy.ByteString where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Data.Text.Text where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Data.Text.Lazy.Text where
    parseRecord = fmap getOnly parseRecord
instance ParseField a => ParseRecord (Maybe a) where
    parseRecord = fmap getOnly parseRecord
instance ParseField a => ParseRecord (First a) where
    parseRecord = fmap getOnly parseRecord
instance ParseField a => ParseRecord (Last a) where
    parseRecord = fmap getOnly parseRecord
instance (Num a, ParseField a) => ParseRecord (Sum a) where
    parseRecord = fmap getOnly parseRecord
instance (Num a, ParseField a) => ParseRecord (Product a) where
    parseRecord = fmap getOnly parseRecord
instance ParseField a => ParseRecord [a] where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Bool where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Ordering where
    parseRecord = fmap getOnly parseRecord
instance ParseRecord Void where
    parseRecord = fmap getOnly parseRecord

instance (ParseFields a, ParseFields b) => ParseRecord (a, b) where
    parseRecord =
            (,)
        <$> parseFields Nothing
        <*> parseFields Nothing

instance (ParseFields a, ParseFields b, ParseFields c) => ParseRecord (a, b, c) where
    parseRecord =
            (,,)
        <$> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing

instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d) => ParseRecord (a, b, c, d) where
    parseRecord =
            (,,,)
        <$> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing

instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d, ParseFields e) => ParseRecord (a, b, c, d, e) where
    parseRecord =
            (,,,,)
        <$> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing

instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d, ParseFields e, ParseFields f) => ParseRecord (a, b, c, d, e, f) where
    parseRecord =
            (,,,,,)
        <$> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing

instance (ParseFields a, ParseFields b, ParseFields c, ParseFields d, ParseFields e, ParseFields f, ParseFields g) => ParseRecord (a, b, c, d, e, f, g) where
    parseRecord =
            (,,,,,,)
        <$> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing
        <*> parseFields Nothing

instance (ParseFields a, ParseFields b) => ParseRecord (Either a b)

{-| A 1-tuple, used solely to translate `ParseFields` instances into
    `ParseRecord` instances
-}
newtype Only a = Only a deriving (Generic, Show)

{-| This is a convenience function that you can use if you want to create a
    `ParseRecord` instance that just defers to the `ParseFields` instance for
    the same type:

> instance ParseRecord MyType where
>     parseRecord = fmap getOnly parseRecord
-}
getOnly :: Only a -> a
getOnly (Only x) = x

class GenericParseRecord f where
    genericParseRecord :: Parser (f a)

instance GenericParseRecord U1 where
    genericParseRecord = pure U1

instance GenericParseRecord V1 where
    genericParseRecord = empty

instance (GenericParseRecord f, GenericParseRecord g) => GenericParseRecord (f :+: g) where
    genericParseRecord =
        fmap L1 genericParseRecord <|> fmap R1 genericParseRecord

instance (GenericParseRecord f, GenericParseRecord g) => GenericParseRecord (f :*: g) where
    genericParseRecord = liftA2 (:*:) genericParseRecord genericParseRecord

instance (Constructor c, GenericParseRecord f) => GenericParseRecord (M1 C c f) where
    genericParseRecord = do
        let m :: M1 i c f a
            m = undefined

        text <- parseField Nothing
        guard (text == Data.Text.toLower (Data.Text.pack (conName m)))
        fmap M1 genericParseRecord

instance (GenericParseRecord f) => GenericParseRecord (M1 D c f) where
    genericParseRecord = fmap M1 genericParseRecord

instance (Selector s, ParseFields a) => GenericParseRecord (M1 S s (K1 i a)) where
    genericParseRecord = do
        let m :: M1 i s f a
            m = undefined

        let label = case selName m of
                ""   -> Nothing
                name -> Just (Data.Text.pack name)
        fmap (M1 . K1) (parseFields label)

{-| A class for all types that can be parsed from zero or more path tokens or
    query parameters

    `parseFields` has a default implementation for any type that implements
    `ParseField`
-}
class ParseRecord a => ParseFields a where
    parseFields
        :: Maybe Data.Text.Text
        -- ^ Field label (`Nothing` for path token, and `Just` for query param)
        -> Parser a
    default parseFields :: ParseField a => Maybe Data.Text.Text -> Parser a
    parseFields = parseField

instance ParseFields Char
instance ParseFields Double
instance ParseFields Float
instance ParseFields Int
instance ParseFields Integer
instance ParseFields Ordering
instance ParseFields Void
instance ParseFields Data.ByteString.ByteString
instance ParseFields Data.ByteString.Lazy.ByteString
instance ParseFields Data.Text.Text
instance ParseFields Data.Text.Lazy.Text
instance ParseFields Bool
instance ParseFields ()

instance ParseFields Any where
    parseFields m = (fmap mconcat . many . fmap Any) (parseField m)

instance ParseFields All where
    parseFields m = (fmap mconcat . many . fmap All) (parseField m)

instance ParseField a => ParseFields (Maybe a) where
    parseFields m = optional (parseField m)

instance ParseField a => ParseFields (First a) where
    parseFields m = (fmap mconcat . many . fmap (First . Just)) (parseField m)

instance ParseField a => ParseFields (Last a) where
    parseFields m = (fmap mconcat . many . fmap (Last . Just)) (parseField m)

instance (Num a, ParseField a) => ParseFields (Sum a) where
    parseFields m = (fmap mconcat . many . fmap Sum) (parseField m)

instance (Num a, ParseField a) => ParseFields (Product a) where
    parseFields m = (fmap mconcat . many . fmap Product) (parseField m)

instance ParseField a => ParseFields [a] where
    parseFields = parseListOfField

{-| A class for all record fields that can be parsed from exactly one
    path token or query parameter

    `parseField` has a default implementation for any type that implements
    `Read`
-}
class ParseField a where
    parseField
        :: Maybe Data.Text.Text
        -- ^ Field label (`Nothing` for path token, and `Just` for query param)
        -> Parser a
    default parseField :: Read a => Maybe Data.Text.Text -> Parser a
    parseField m = do
        text <- parseField m
        case Text.Read.readMaybe (Data.Text.unpack text) of
            Nothing -> empty
            Just a  -> return a

    parseListOfField
        :: Maybe Data.Text.Text
        -- ^ Field label
        -> Parser [a]
    parseListOfField m = many (parseField m)

instance ParseField Data.ByteString.ByteString where
    parseField  Nothing =
        fmap Data.Text.Encoding.encodeUtf8 (parseField Nothing)
    parseField (Just label) = do
        let labelBytes = Data.Text.Encoding.encodeUtf8 label

        let pop         []   = empty
            pop ((k, v):kvs)
                | k == labelBytes = return (v, kvs)
                | otherwise        = do
                    (v', kvs') <- pop kvs
                    return (v', (k, v):kvs')

        Route tokens params <- get
        case pop params of
            Just (v, params') -> do
                put (Route tokens params')
                return v
            Nothing           -> empty

instance ParseField Data.Text.Text where
    parseField Nothing = do
        Route tokens params <- get
        case tokens of
            []            -> empty
            token:tokens' -> do
                put (Route tokens' params)
                return token
    parseField (Just label) = do
        bytes <- parseField (Just label)
        case Data.Text.Encoding.decodeUtf8' bytes of
            Left _     -> empty
            Right text -> return text

instance ParseField Data.Text.Lazy.Text where
    parseField m = fmap Data.Text.Lazy.fromStrict (parseField m)

instance ParseField Data.ByteString.Lazy.ByteString where
    parseField m = fmap Data.ByteString.Lazy.fromStrict (parseField m)

instance ParseField Bool
instance ParseField Double
instance ParseField Float
instance ParseField Int
instance ParseField Integer
instance ParseField Ordering
instance ParseField Void

instance ParseField String where
    parseField m = fmap Data.Text.unpack (parseField m)

instance ParseField Char where
    parseField m = do
        string <- parseField m
        case string of
            [c] -> return c
            _   -> empty

    parseListOfField m = parseField m

instance ParseField () where
    parseField _ = pure ()

instance ParseField All where
    parseField m = fmap All (parseField m)

instance ParseField Any where
    parseField m = fmap Any (parseField m)

{-| Simple server that listens on the given `Port` and runs the handler for
    each incoming connection

    The value supplied to the handler is automatically parsed from the route

    The request method is ignored

    Failure to parse a value from the route results in a response with a 404
    status code
-}
serve
    :: ParseRecord a
    => Port
    -- ^ Port to listen on
    -> (a -> IO Response)
    -- ^ Handler for parsed value
    -> IO ()
    -- ^ Run the server
serve port handler = Network.Wai.Handler.Warp.run port application
  where
    application request respond = do
        let rawQuery = Network.Wai.rawQueryString request
        let route = Route
                { path  = Network.Wai.pathInfo request
                , query = Network.HTTP.Types.URI.parseSimpleQuery rawQuery
                }

        let parser = do
                x <- parseRecord
                Route tokens params <- get
                guard (null tokens && null params)
                return x

        case Control.Monad.State.evalStateT (unParser parser) route of
            []  -> do
                let status   = Network.HTTP.Types.Status.status404
                let response =
                        Network.Wai.responseLBS status [] "404 Not Found\n"
                respond response
            a:_ -> do
                response <- handler a
                respond response

{-| Like `serve` except the handler result is automatically encoded as JSON and
    served as the `Response`
-}
serveJSON :: (ParseRecord a, ToJSON b) => Port -> (a -> IO b) -> IO ()
serveJSON port handler = serve port handler'
  where
    handler' a = do
        b <- handler a
        let status = Network.HTTP.Types.Status.status200
        return (Network.Wai.responseLBS status [] (Data.Aeson.encode b <> "\n"))

-- | Like `serve` except the `Response` is always @\"200 OK\"@
serveOK :: ParseRecord a => Port -> (a -> IO ()) -> IO ()
serveOK port handler = serve port handler'
  where
    handler' a = do
        handler a
        let status = Network.HTTP.Types.Status.status200
        return (Network.Wai.responseLBS status [] "200 OK\n")
