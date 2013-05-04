
import Text.KR4MB
import Control.Monad (forM_)

dest_path = "/Users/keqh/Library/Application Support/KeyRemap4MacBook/private.xml"

main = reload dest_path rule

rule :: Rule
rule = root $ do
  item "Control_L to Control_L" $ do
    appendix "(+when you type Control_L only, send Escape + EISUU)"
    keyOverlaidModifier CONTROL_L CONTROL_L [JIS_EISUU, ESCAPE]

  group "use extra1" $ do
      item "for symbol keycode" $ do
        appendix "Modと併用時は普通にshiftとして動作する"
        forM_ [cmd, opt, ctrl] $ \modkey -> do
            modkey SHIFT_L `keyToKey` modkey SHIFT_L

      --  appendix "左shiftを別extra1に変換"
      --  SHIFT_L `keyToKey` VK_MODIFIER_EXTRA1

      --  appendix "extra1と同時押しでa-lを最上段に変換"
      --  forM_ (zip "asdfghjkl;" "!@#$%^&*()") $ \(c,sym) -> do
      --    extra1 c `keyToKey` sym

      -- EXTRA1に依存してるので整理すること
      item "for tmux" $ do
        appendix "左shiftを別extra1に変換"
        SHIFT_L `keyToKey` VK_MODIFIER_EXTRA1

        let focusiTerm = opt $ ctrl $ shift 'z'
        let tmuxPrefix = ctrl 't'
        forM_ "[uiopnc" $ \key -> do
          extra1 key `keyToKey'` [toKey JIS_EISUU, focusiTerm, tmuxPrefix, toKey key]

      item "chrome" $ do
        appendix "左shiftを別extra1に変換"
        SHIFT_L `keyToKey` VK_MODIFIER_EXTRA1

        -- alfredでF3でchromeにfocusが行くよう設定
        let focusChrome = toKey F3
        forM_ "jk" $ \key -> do
          extra1 key `keyToKey'` [toKey JIS_EISUU, focusChrome, toKey key]

  item "Swap Semicolon and underscore" $ do
    ':' `swapKey` ':'
    ';' `swapKey` '_'

  item "Google IME" $ do
    ctrl 'j' `keyToKey` ctrl (shift 'j')

  item "for coding" $ do
    extra1 '.' `keyToKey'` " -> "

    appendix "insert where clause for haskell"
    extra1 'w' `keyToKey'` "\n  where\n"

  group "standard settings" $ do
    item "JIS to US" $ do
        setJSLayout

    -- item "basic settings" $ do
    item "Command_L to Option_L" $ do
      COMMAND_L `keyToKey` OPTION_L

    item "EISUU to Command_L" $ do
      appendix "(+ When you type EISUU only, send EISUU)"
      keyOverlaidModifier JIS_EISUU COMMAND_L [JIS_EISUU]

    item "KANA to Return" $ do
      JIS_KANA `keyToKey` RETURN

    item "Space to Shift_L" $ do
      appendix "(+ When you type Space only, send Space) + [KeyRepeat]"
      SPACE `keyOverlaidModifierWithRepeat` SHIFT_L

    item "F[7-12] to ConsumerKeys" $ do
      F7 `keyToConsumer` MUSIC_PREV
      F8 `keyToConsumer` MUSIC_PLAY
      F9 `keyToConsumer` MUSIC_NEXT

      F10 `keyToConsumer` VOLUME_MUTE
      F11 `keyToConsumer` VOLUME_DOWN
      F12 `keyToConsumer` VOLUME_UP

