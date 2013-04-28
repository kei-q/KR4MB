
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

        appendix "左shiftを別extra1に変換"
        SHIFT_L `keyToKey` VK_MODIFIER_EXTRA1

        appendix "extra1と同時押しでa-lを最上段に変換"
        forM_ (zip "asdfghjkl;" "!@#$%^&*()") $ \(c,sym) -> do
          extra1 c `keyToKey` sym

      -- EXTRA1に依存してるので整理すること
      item "for tmux" $ do
        appendix "左shiftを別extra1に変換"
        SHIFT_L `keyToKey` VK_MODIFIER_EXTRA1

        let focusiTerm = opt $ ctrl $ shift 'z'
        let tmuxPrefix = ctrl 't'
        forM_ "[uiopnc" $ \key -> do
          extra1 key `keyToKey'` [toKey JIS_EISUU, focusiTerm, tmuxPrefix, toKey key]

  item "Swap Semicolon and underscore" $ do
    ':' `swapKey` ':'
    ';' `swapKey` '_'

  item "Google IME" $ do
    ctrl 'j' `keyToKey` ctrl (shift 'j')

  item "for coding" $ do
    extra1 '.' `keyToKey'` " -> "
    appendix "insert where clause for haskell"
    extra1 'w' `keyToKey'` "\n  where\n"

