--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.ShowLatex
-- Note        :
--
-- Benötigt die Programme:
--      * latex
--      * dvipng
--      * sxiv
--
-- Dies sollte so hauptsächlich unter Linux laffähig sein
--
--------------------------------------------------------------------------------
module Projekt.Core.ShowLatex
  ( ShowLatex(..)
  , renderLatex, renderRawLatex
  , viewRendered
  ) where
import System.Process

class ShowLatex a where
  showLatex :: a -> String

instance ShowLatex Integer where
  showLatex = show

--------------------------------------------------------------------------------
--  Nutze latex und dvipng um Latex schnipsel in ein PNG zu rendern

renderLatex :: (ShowLatex a) => a -> IO ()
renderLatex = renderRawLatex . showLatex

renderRawLatex :: String -> IO ()
renderRawLatex x = do createProcess (shell cmd)
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
--  Nutze sxiv um das erzeugte Bild anzuzeigen

viewRendered = do createProcess (shell "sxiv /tmp/snipet.png")
                  return ()
