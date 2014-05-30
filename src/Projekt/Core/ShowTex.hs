--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.ShowTex
-- Note        :
--
-- Benötigt die Programme:
--      * latex     zum rendern des Latex Codes
--      * dvipng    um aus dem erstelltem DVI ein PNG zu erstellen
--      * sxiv      für das schnelle öffnen durch viewRendered
--
-- Dies sollte so hauptsächlich unter Linux laffähig sein
--
--------------------------------------------------------------------------------
module Projekt.Core.ShowTex
  ( ShowTex(..)
  , renderTex, renderRawTex
  , viewRendered
  ) where
import System.Process

class ShowTex a where
  showTex :: a -> String

instance ShowTex Integer where
  showTex = show

--------------------------------------------------------------------------------
--  Nutze latex und dvipng um Latex schnipsel in ein PNG zu rendern

-- |wie renderRawTex, nur dass zunächst ShowTex aufgerufen wird.
renderTex :: (ShowTex a) => a -> IO ()
renderTex = renderRawTex . showTex

-- |Nimmt einen Latex-String und packt diesen in ein minimales Latex Dokument,
-- rendert dieses und wandelt es danach in ein Bild um, wobei unnötiger Rand
-- entfernt wird
renderRawTex :: String -> IO ()
renderRawTex x = do createProcess (shell cmd)
                    return ()
  where cmd = "latex -halt-on-error -output-directory /tmp "
              ++ "'\\documentclass[12pt]{article}"
              ++ "\\pagestyle{empty}"
              ++ "\\usepackage{amsmath}"
              ++ "\\begin{document}"
              ++ "\\begin{multline*}"
              ++ x
              ++ "\\end{multline*}"
              ++ "\\end{document}'"
              ++ " > /dev/null"
              ++ " ; "
              ++ "dvipng "
              ++ "-gamma 2 -z 9 -T tight "
              ++ "-bg White " -- ++ "-bg Transparent "
              ++ "-o /tmp/snipet.png /tmp/article.dvi"
              ++ " > /dev/null"

--------------------------------------------------------------------------------
-- |Nutze sxiv um das erzeugte Bild anzuzeigen
viewRendered = do createProcess (shell "sxiv /tmp/snipet.png")
                  return ()
