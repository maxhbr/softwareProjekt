--------------------------------------------------------------------------------
-- | 
-- Module      : Projekt.Core.ShowLatex
-- Note        : 
-- 
-- 
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

--------------------------------------------------------------------------------
--  Use latex and divpng to render Latex snippets

renderLatex :: (ShowLatex a) => a -> IO ()
renderLatex = renderRawLatex . showLatex

renderRawLatex :: String -> IO ()
renderRawLatex x = do createProcess (shell cmd)
                      return ()
  where cmd = "latex -halt-on-error -output-directory /tmp "
               ++ "\"\\documentclass[12pt]{article}"
               ++ "\\pagestyle{empty}"
               ++ "\\usepackage{amsmath}"
               ++ "\\begin{document}"
               ++ "\\begin{multline*}"
               ++ x
               ++ "\\end{multline*}"
               ++ "\\end{document}\""
               ++ " > /dev/null"
               ++ " ; "
               ++ "dvipng " 
               ++ "-gamma 2 -z 9 -T tight "
               -- ++ "-bg Transparent "
               ++ "-bg White "
               ++ "-o /tmp/snipet.png /tmp/article.dvi"
               ++ " > /dev/null"

viewRendered = do createProcess (shell "sxiv /tmp/snipet.png")
                  return ()
