module Wiki where

--------------------------------------------------------------------------------

-- List of imported modules and functions
import Control.Applicative
import Control.Exception

import Data.ByteString.Char8 as BS (unpack, pack, split, dropWhile)

import Data.Maybe (fromJust)
import Text.Html
import Network.Shed.Httpd

--------------------------------------------------------------------------------
-- | String formatting alternatives for inline text.

data Inline
  = Bold String
  | Unformatted String
  | Italic String
  | WikiLink String
  | URILink { uriLinkRef :: String, uriLinkName :: String }
  deriving (Eq, Show)

-- | A list of (un)formatted strings.

type Text
  = [Inline]

-- | Text formatting alternatives for blocks of text (between newlines).

data Block
  = Bullet Text
  | Paragraph Text
  | Header Text
  | Number Text
  deriving (Eq, Show)

-- | A list of (un)formatted blocks.

type Page
  = [Block]

--------------------------------------------------------------------------------

-- | Split a string into its lexical components to simplify manipulation.

inlinify :: String -> [String]
inlinify [] = []
inlinify str =
  case split [] str of
    (ln, wd, next) -> appCons reverse ln . appCons id wd $ inlinify next
  where
    split ln ('*':'*':cs) = (ln, "**", cs)
    split ln ('/':'/':cs) = (ln, "//", cs)
    split ln ('[':'[':cs) = (ln, "[[", cs)
    split ln (']':']':cs) = (ln, "]]", cs)
    split ln ('|':cs)     = (ln, "|", cs)
    split ln (c:cs)       = split (c : ln) cs
    split ln ""           = (ln, "", "")

-- | Apply a function to a list and cons the result to another list.

appCons :: ([a] -> b) -> [a] -> [b] -> [b]
appCons _ [] = id
appCons f xs = (f xs :)

-- | Split lines into their lexical components to simplify manipulation.

blockify :: [String] -> [String]
blockify [] = []
blockify lns =
  case split [] lns of
    (paras, ln, next) -> appCons (unwords . reverse) paras . appCons id ln $ blockify next
  where
    split paras (l@('*':' ':_):ls)     = (paras, l, ls)
    split paras (l@('#':' ':_):ls)     = (paras, l, ls)
    split paras (l@('=':'=':' ':_):ls) = (paras, l, ls)
    split paras ("":ls)                = (paras, [], ls)
    split paras (l:ls)                 = split (l : paras) ls
    split paras []                     = (paras, [], [])

-- | Merge two |Maybe| values with priority on the first one.

mplus :: Maybe a -> Maybe a -> Maybe a
mplus Nothing y = y
mplus x       _ = x

--------------------------------------------------------------------------------

-- Ex. 3
-- Ex. 4

readInline :: [String] -> Maybe (Inline, [String])
readInline inp = readFormattedInline inp `mplus` readUnformattedInline inp

readUnformattedInline :: [String] -> Maybe (Inline, [String])
readUnformattedInline []      = Nothing
readUnformattedInline (c:cs)  = Just (Unformatted c, cs)

type Token = String

readFormattedInline :: [String] -> Maybe (Inline, [String])
readFormattedInline inp =
  case inp of
    "//":txt:"//":rest         -> Just (Italic txt, rest)
    "**":txt:"**":rest         -> Just (Bold txt, rest)
    "[[":uri:"|":linkname:"]]":rest -> Just (URILink uri linkname, rest)
    "[[":txt:"]]":rest         -> Just (WikiLink txt, rest)
    _                          -> Nothing
    
--------------------------------------------------------------------------------

-- Ex. 5

readText :: String -> Text
readText = parse . inlinify
  where
    parse :: [String] -> [Inline]
    parse [] = []
    parse xs = case readInline xs of
        Just (val, s) -> val : parse s
        Nothing -> []
--------------------------------------------------------------------------------

-- Ex. 6

readBlock :: String -> Block
readBlock inp = parseBlock inp
  where
    parseBlock :: String -> Block 
    parseBlock i = fromJust $ readFormattedBlock i `mplus` readUnformattedBlock i

readUnformattedBlock :: String -> Maybe Block
readUnformattedBlock = Just . Paragraph . readText

readFormattedBlock :: String -> Maybe Block
readFormattedBlock inp =
  case inp of
    '*':' ':txt  -> Just . Bullet $ readText txt
    '#':' ':txt  -> Just . Number $ readText txt
    '=':'=':' ':txt  -> Just . Header $ readText txt
    _            -> Nothing

--------------------------------------------------------------------------------

-- Ex. 7

readPage :: String -> Page
readPage inp = map readBlock (blockify $ lines $ inp)

--------------------------------------------------------------------------------

-- Ex. 9

-- | Example use of Text.HTML: make a paragraph with |"Hello, World!"| in bold.

exampleHtml :: Html
exampleHtml = thehtml << body << p << bold << "Hello, World!"

inlineToHtml :: Inline -> Html
inlineToHtml inl = case inl of
  Italic x      -> italics      << x
  Bold x        -> bold         << x
  WikiLink x    -> anchor       << x
  URILink x y   -> anchor       << x
  Unformatted x -> stringToHtml    x


textToHtml :: Text -> Html
textToHtml = concatHtml . map inlineToHtml
--------------------------------------------------------------------------------

-- Ex. 10

-- | Example unordered list with list items in HTML.

exampleList :: Html
exampleList = ulist << [li << "item 1", li << "item 2"]

blockToHtml :: Block -> Html
blockToHtml b = case b of
  Bullet x -> ulist << textToHtml x
  Paragraph x -> p << textToHtml x 
  Header x -> h1 << textToHtml x
  Number x -> olist << textToHtml x

pageToHtml :: Page -> Html
pageToHtml = concatHtml . map blockToHtml

-- | Convert a wiki-formatted string to a renderable HTML string.

wikify :: String -> String
wikify = renderHtml . (body <<) . pageToHtml . readPage

--------------------------------------------------------------------------------

simpleServer :: Request -> IO Response  
simpleServer = return . Response responseOK [contentText] . show

initMyServer :: (Request -> IO Response) -> IO Server
initMyServer = initServer myPort

myPort :: Int
myPort = 7734

contentText, contentHtml :: (String, String)
contentText = contentType "text/plain"
contentHtml = contentType "text/html"

responseOK, responseNotFound :: Int
responseOK       = 200
responseNotFound = 404

methodGET, methodPOST :: String
methodGET  = "GET"
methodPOST = "POST"

--------------------------------------------------------------------------------

-- Ex. 12

fileServer :: Request -> IO Response    
fileServer (Request _ uri _ _) = do
  case getPath uri of
    (f:fx:[]) -> buildResponse (getFileName f fx) (BS.unpack fx)
    _ -> return $ Response 400 [] "Bad request!"
  where
    getFileName file ext = (BS.unpack file) ++ "." ++ (BS.unpack ext)
    getPath url = split '.' $ BS.dropWhile (\x -> x == '/') (pack . show $ url)

buildResponse :: String -> String -> IO Response
buildResponse file ext = case ext of
    "txt"  -> success contentText file ext
    "html" -> success contentHtml file ext
    "wiki" -> success contentHtml file ext
    _      -> return $ Response 404 [] "Unknown file extension"

success :: (String, String) -> String -> String -> IO Response  
success hdrs file ext = (Response 200 [hdrs] <$> needsWikify ext <$> readFile file)
                        `catch`
                        handleException
  where
    handleException = \e -> do
      print $ "Caught exception :(" ++ show (e :: IOException)
      return (Response 404 [] "File not found :(")

needsWikify :: String -> (String -> String)
needsWikify "wiki" = wikify
needsWikify _      = id
--------------------------------------------------------------------------------

-- Ex. 13

buildForm :: Html
buildForm =
  form ! [action $ "http://localhost:" ++ show myPort ++ "/wiki/show", method methodPOST]
    << [ input ! [thetype "text", name "thetext"]
       , input ! [thetype "submit", value "Submit"]
       ]

wikiFormServer :: Request -> IO Response        
wikiFormServer req@(Request _ uri _ bod) = case (show uri) of
    "/wiki/edit" -> return $ Response 200 [] (renderHtml buildForm)
    "/wiki/show" -> return $ Response 200 [contentHtml] (wikify formData)
    _ -> fileServer req
  where
    formData :: String
    formData = snd . head $ queryToArguments bod
