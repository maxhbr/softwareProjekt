--------------------------------------------------------------------------------
-- | 
-- Module      : Projekt.Core.LatexShow
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------
module Projekt.Core.LatexShow
  ( LatexShow(..)
  , latexRender, latexRenderRaw
  ) where
import System.Process

class LatexShow a where
  latexShow :: a -> String

--------------------------------------------------------------------------------
--  Use latex and divpng to render Latex snippets

latexRender :: (LatexShow a) => a -> IO ()
latexRender = latexRenderRaw . latexShow

latexRenderRaw :: String -> IO ()
latexRenderRaw x = do createProcess (shell cmd)
                      return ()
  where cmd = "latex -halt-on-error -output-directory /tmp "
               ++ "\"\\documentclass[12pt]{article}"
               ++ "\\pagestyle{empty}"
               ++ "\\begin{document}"
               ++ "\\["
               ++ x
               ++ "\\]"
               ++ "\\end{document}\""
               ++ " > /dev/null"
               ++ " ; "
               ++ "dvipng " 
               ++ "-gamma 2 -z 9 -T tight "
               ++ "-bg Transparent "
               ++ "-o /tmp/snipet.png /tmp/article.dvi"
               ++ " > /dev/null"
