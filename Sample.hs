
import Text.KR4MB
import Control.Monad (forM_)
import System.Environment

dest_path = "/Users/keqh/Library/Application Support/KeyRemap4MacBook/private.xml"

settings = [("repeat.wait", 10)
    ,("repeat.initial_wait", 250)
    ,("pointing.relative2scroll_rate", 250)
    ]

main = do
  (arg:rest) <- getArgs
  if arg == "dump"
    then do
      dump rule
    else do
      reload dest_path rule
      setParams settings

rule :: Rule
rule = root $ do
  item "disable minimize" $ do
    cmd 'm' `keyToKey` VK_NONE

  item "personal settings" $ do
    keyOverlaidModifier CONTROL_L CONTROL_L [JIS_EISUU, ESCAPE]

    ':' `swapKey` ':'
    ';' `swapKey` '_'

    appendix "Google IME"
    ctrl 'j' `keyToKey` JIS_KANA

  group "standard settings" $ do
    item "JIS to US" $ do
        setJSLayout

    item "basic settings" $ do
      COMMAND_L `keyToKey` OPTION_L
      JIS_KANA `keyToKey` RETURN
      keyOverlaidModifier JIS_EISUU COMMAND_L [JIS_EISUU]
      SPACE `keyOverlaidModifierWithRepeat` SHIFT_L

      F7 `keyToConsumer` MUSIC_PREV
      F8 `keyToConsumer` MUSIC_PLAY
      F9 `keyToConsumer` MUSIC_NEXT

      F10 `keyToConsumer` VOLUME_MUTE
      F11 `keyToConsumer` VOLUME_DOWN
      F12 `keyToConsumer` VOLUME_UP

  group "use extra1" $ do
    item "for symbol keycode" $ do
      appendix "Modと併用時は普通にshiftとして動作する"
      forM_ [cmd, opt, ctrl] $ \modkey -> do
          modkey SHIFT_L `keyToKey` modkey SHIFT_L
      SHIFT_L `keyToKey` VK_MODIFIER_EXTRA1

      -- EXTRA1に依存してるので整理すること
    item "for tmux" $ do
      let focusiTerm = opt $ ctrl $ shift 'z'
      let tmuxPrefix = ctrl 't'
      forM_ "jklpnc" $ \key -> do
        extra1 key `keyToKey'` [toKey JIS_EISUU, focusiTerm, tmuxPrefix, toKey key]

    app_only "TERMINAL" $ do
      let tmuxPrefix = ctrl 't'
      let copyModePrefix = ctrl '['
      forM_ "du" $ \key -> do
        extra1 key `keyToKey'` [tmuxPrefix, copyModePrefix, ctrl key]

  item "for coding" $ do
    extra1 '.' `keyToKey'` " -> "
    extra1 ',' `keyToKey'` " # "
    extra1 'w' `keyToKey'` "\n  where\n"

