{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Text.KR4MB where

import Control.Monad.RWS
import Data.Char (toLower, toUpper, isNumber)
import Data.List (intercalate)

import qualified Text.KR4MB.CLI as CLI

data RuleState = RS { rsIndent :: Int, rsIdentities :: [String] } deriving Show
defaultRuleState = RS { rsIndent = 0, rsIdentities = [] }

type Rule = RWS () String RuleState ()
indentLevel = 4

run rule = (state, xml)
  where
    (state, xml) = execRWS rule () defaultRuleState

-- keyrepeat settings
-- =========================

type KeyRepeatSettings = [(String, Int)]
setParams :: KeyRepeatSettings -> IO ()
setParams = mapM_ CLI.set

-- reload settings
-- =========================

reload :: FilePath -> Rule -> IO ()
reload private_xml_path rule = do
    let (state, xml) = run rule
    writeFile private_xml_path xml
    CLI.reload

    forM_ (rsIdentities state) $ \ident -> do
        CLI.enable ident

    return ()

-- change state
-- =========================

doIndent :: Rule -> Rule
doIndent rule = indent >> rule >> dedent
  where
    indent = do
        state <- get
        let newIndent = (rsIndent state) + indentLevel
        put state{ rsIndent = newIndent }

    dedent = do
        state <- get
        let newIndent = (rsIndent state) - indentLevel
        put state{ rsIndent = newIndent }

identifier :: String -> Rule
identifier name = do
    let ident = toIdentifier name
    state <- get
    let newIdentities = ident : rsIdentities state
    put state{ rsIdentities = newIdentities }
    tell' "identifier" ident

-- utility
-- =========================

tell' tag s = do
    tellIndent
    tell $ "<" ++ tag ++ ">" ++ s ++ "</" ++ tag ++ ">\n"

tellIndent = do
    state <- get
    let indent = rsIndent state
    tell $ replicate indent ' '

wrap :: String -> Rule -> Rule
wrap tagname rule = do
    tellIndent
    tell $ "<" ++ tagname ++ ">\n"
    doIndent rule
    tellIndent
    tell $ "</" ++ tagname ++ ">\n"

toIdentifier :: String -> String
toIdentifier s = "private." ++ [toLower c | c <- s, c /= ' ']

-- main functions
-- =========================

root :: Rule -> Rule
root rule = do
    tell $ "<?xml version=\"1.0\"?>\n"
    wrap "root" rule

group :: String -> Rule -> Rule
group name config = wrap "item" $ do
    tell' "name" name
    config

item :: String -> Rule -> Rule
item name config = wrap "item" $ do
    tell' "name" name
    identifier name
    config

appendix :: String -> Rule
appendix message = tell' "appendix" message

autogen :: String -> String ->  Rule
autogen key contents = tell' "autogen" $ key ++ " " ++ contents

only :: String -> Rule
only app_name = tell' "only" app_name

-- keyremap
-- =========================

--(<.>) :: a -> a -> a
a <.> b = a ++ ", " ++ b

showKey :: forall a. (KeyBehavior a) => a -> String
showKey = show . toKey

keyOverlaidModifier :: (KeyBehavior a) => a -> a -> [a] -> Rule
keyOverlaidModifier base normal single = do
    let base' = showKey base
    let normal' = showKey normal
    let single' = intercalate ", " $ map showKey single
    autogen "__KeyOverlaidModifier__" (base' <.> normal' <.> single')

keyOverlaidModifierWithRepeat :: (KeyBehavior a) => a -> a -> Rule
keyOverlaidModifierWithRepeat base normal = do
    let base' = showKey base
    let normal' = showKey normal
    autogen "__KeyOverlaidModifierWithRepeat__" (base' <.> normal' <.> base')

keyToKey :: (KeyBehavior a, KeyBehavior b) => a -> b -> Rule
keyToKey old new = do
    let old' = showKey old
    let new' = showKey new
    autogen "__KeyToKey__" (old' <.> new')

keyToKey' :: (KeyBehavior a, KeyBehavior b) => a -> [b] -> Rule
keyToKey' old seqs = do
    let old' = showKey old
    let seqs' = intercalate ", " $ map showKey seqs
    autogen "__KeyToKey__" (old' <.> seqs')

--keyToConsumer :: a
keyToConsumer old new = do
    let old' = showKey old
    let new' = showKey new
    autogen "__KeyToConsumer__" (old' <.> new')

app_only app_name rule = do
    item app_name $ do
        only app_name
        rule

-- contributes
-- =========================

swapKey :: (KeyBehavior a, KeyBehavior b) => a -> b -> Rule
swapKey k1 k2 = do
    k1 `keyToKey` k2
    k2 `keyToKey` k1

keySequence :: String -> [Key]
keySequence = map toKey

setJSLayout = do
    autogen "__SetKeyboardType__" "KeyboardType::MACBOOK"
    JIS_YEN `keyToKey` '`'
    JIS_UNDERSCORE `keyToKey` '`'

-- modkeys
-- =========================
shift, control, command, option, extra1 :: (KeyBehavior a) => a -> Key
shift k = hoge M_SHIFT_L (toKey k)
control k = hoge M_CONTROL_L (toKey k)
command k = hoge M_COMMAND_L (toKey k)
option k = hoge M_OPTION_L (toKey k)
extra1 k = hoge M_EXTRA1 (toKey k)

cmd, opt, ctrl :: (KeyBehavior a) => a -> Key
cmd = command
opt = option
ctrl = control

hoge mod (Key code mods) = Key code (mod : mods)

-- data definitions
-- =========================

data Key = Key KeyCode [ModKey]

data KeyCode
    = C Char
    | ConsumerKey ConsumerKey
    | CONTROL_L | SHIFT_L | OPTION_L | COMMAND_L
    | CONTROL_R | SHIFT_R | OPTION_R | COMMAND_R
    | VK_MODIFIER_EXTRA1
    | VK_NONE
    | JIS_EISUU | JIS_KANA | JIS_YEN | JIS_UNDERSCORE | ESCAPE
    | RETURN | SPACE
    | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12
    deriving (Show)

data ConsumerKey
    = MUSIC_PREV | MUSIC_PLAY | MUSIC_NEXT
    | VOLUME_MUTE | VOLUME_DOWN | VOLUME_UP
    deriving (Show)

data ModKey = M_SHIFT_L | M_CONTROL_L | M_COMMAND_L | M_OPTION_L | M_EXTRA1 deriving (Show)

class (Show a) => KeyBehavior a where
    toKey :: a -> Key

instance KeyBehavior Key where
    toKey = id

instance KeyBehavior KeyCode where
    toKey code = Key code []

instance KeyBehavior ConsumerKey where
    toKey ckey = Key (ConsumerKey ckey) []

instance KeyBehavior Char where
    toKey c | Just c' <- lookup c shiftKeyMap = Key (C c') [M_SHIFT_L]
    toKey c = Key (C c) []

-- english key layout
shiftKeyMap = zip "!@#$%^&*()_+~QWERTYUIOP{}ASDFGHJKL:\"|ZXCVBNM<>?~" "1234567890-=`qwertyuiop[]asdfghjkl;'\\zxcvbnm,./`"

-- convert private.xml
-- =========================

instance Show Key where
    show (Key code []) = showKeyCode code
    show (Key code mods) = showKeyCode code ++ ", " ++ intercalate " | " (map showModKey mods)

keyCodePrefix :: String -> String
keyCodePrefix s = "KeyCode::" ++ s

showKeyCode :: KeyCode -> String
showKeyCode (C ';') = keyCodePrefix "SEMICOLON"
showKeyCode (C '-') = keyCodePrefix "MINUS"
showKeyCode (C '[') = keyCodePrefix "BRACKET_LEFT"
showKeyCode (C ']') = keyCodePrefix "BRACKET_RIGHT"
showKeyCode (C '.') = keyCodePrefix "DOT"
showKeyCode (C ',') = keyCodePrefix "COMMA"
showKeyCode (C ' ') = keyCodePrefix "SPACE"
showKeyCode (C '`') = keyCodePrefix "BACKQUOTE"
showKeyCode (C '\n') = keyCodePrefix "ENTER"
showKeyCode (C c)
  | isNumber c = keyCodePrefix $ "KEY_" ++ [c]
  | otherwise = keyCodePrefix [toUpper c]
showKeyCode (ConsumerKey ckey) = "ConsumerKeyCode::" ++ show ckey
showKeyCode code = keyCodePrefix $ show code

showModKey :: ModKey -> String
showModKey modkey = "ModifierFlag::" ++ drop 2 (show modkey)

