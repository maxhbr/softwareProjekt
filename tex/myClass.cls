%\xdef\myDevelopVariable{0}
\def\ifundefined#1{\expandafter\ifx\csname#1\endcsname\relax}
\ifundefined{myDevelopVariable}\xdef\myDevelopVariable{1}\fi

\LoadClassWithOptions{scrbook}


\usepackage[utf8]{luainputenc}
\usepackage[T1]{fontenc}

%%%% General
\clubpenalty = 10000
\widowpenalty = 10000
\displaywidowpenalty = 10000

%%%% Sprache
\usepackage{polyglossia}
\setdefaultlanguage{german}
\setotherlanguages{english}

\usepackage{hyperref}
\usepackage{amsmath,amssymb}
\usepackage{latexsym}
\usepackage{enumerate}
\usepackage{graphicx}
\usepackage[table]{xcolor}

\usepackage{url} % URLS in bibtex

\usepackage[german]{varioref}


%%%% Listings in footnotes
\usepackage{cprotect}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  Design / Style  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Headline styles %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage[automark]{scrpage2}
\pagestyle{scrheadings}

%%%%  Layout  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%\usepackage[scale=0.75]{geometry}
%\setlength{\unitlength}{1mm}
%\emergencystretch 2em
%\usepackage{multicol}
%\usepackage{lscape} % alt: pdflscape

\allowdisplaybreaks

\iftrue %Section Numbers moved left out
  \usepackage{sectsty}
  \makeatletter\def\@seccntformat#1{%
    \protect\makebox[0pt][r]{\csname the#1\endcsname\hspace{12pt}}}\makeatother
\fi

%\renewcommand*{\thefootnote}{[\arabic{footnote}]}


\iftrue %Horizontale Linien
  %\definecolor{myheadLineColor}{RGB}{0,51,128}
  \definecolor{myheadLineColor}{RGB}{0,0,0}
  \usepackage{sectsty}
  \chapterfont{
    \color{myheadLineColor}
    \sectionrule{0pt}{0pt}{-5pt}{2pt}
    }
  \sectionfont{
    \color{myheadLineColor}
    \sectionrule{0pt}{0pt}{-5pt}{0.8pt}
    }
  \subsectionfont{
    \color{myheadLineColor}
    \sectionrule{0pt}{0pt}{-5pt}{0.6pt}
    }
  \subsubsectionfont{
    \color{myheadLineColor}
    \sectionrule{0pt}{0pt}{-5pt}{0.4pt}
    }
\fi

%%%%  Typography  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{lmodern} %Type1-Schriftart für nicht-englische Texte
\setkomafont{sectioning}{\normalcolor\bfseries\boldmath}

\iffalse %schrift enger:
  \usepackage{mathptmx}
  \usepackage[scaled=.92]{helvet}
  \usepackage{courier}
\fi
\makeatletter
%\linespread{1.5}

\usepackage[babel,german=quotes]{csquotes}%
\defineshorthand{"`}{\openautoquote}
\defineshorthand{"'}{\closeautoquote}


%%%%  Theoreme Styles  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage[thref, hyperref, amsmath, thmmarks]{ntheorem}


\theoremstyle{plain}% default
\newtheorem{thm}{Satz}[chapter]
%%\newtheorem{thm}[satz]{Satz}
\newtheorem{lemma}[thm]{Lemma}
%\newtheorem{lem}[thm]{Lemma}
%\newtheorem{lemdef}[thm]{Lemma/Definition}
%\newtheorem{lemcor}[thm]{Lemma/Korollar}
%\newtheorem{defcor}[thm]{Definition/Korollar}
%\newtheorem{cor}[thm]{Korollar}
%\newtheorem{kor}[thm]{Korollar}
\newtheorem{prop}[thm]{Proposition}

%\newtheorem{ex}{Aufgabe}[chapter]
%\newtheorem*{beh}{Behauptung}
%\newtheorem*{dazu}{Dazu}

\theoremstyle{definition}
\theorembodyfont{\normalfont}
\newtheorem{definition}[thm]{Definition}
%\newtheorem{defn}[thm]{Definition}
%\newtheorem{conj}[thm]{Conjecture}
%\newtheorem{exmp}[thm]{Beispiel}
%\newtheorem{bsp}[thm]{Beispiel}
\newtheorem{invariante}[thm]{Invariante}


\theoremstyle{remark}
\newtheorem{bemerkung}[thm]{Bemerkung}
%\newtheorem{bem}[thm]{Bemerkung}
%\newtheorem{beo}[thm]{Beobachtung}
%\newtheorem{note}[thm]{Notiz}
\newtheorem{beispiel}[thm]{Beispiel}

%%\usepackage{thmbox}
%\newtheorem{try}{Versuch}
%\newtheorem{case}{Fall}

%\numberwithin{equation}{chapter}
%\numberwithin{figure}{chapter}
%\numberwithin{figure}{thm}

\theoremstyle{nonumberplain}
\theoremindent0pt
\theoremheaderfont{\sffamily\itshape}
\theorembodyfont{\normalfont}
\theoremseparator{.}
\theoremsymbol{\ensuremath{_\square}}
\newtheorem{proof}{Beweis}
\qedsymbol{\ensuremath{_\square}}

\usepackage[framemethod=TikZ]{mdframed}
\surroundwithmdframed[
  linewidth=2pt,
  linecolor=gray,
  topline=false,
  rightline=false,
  bottomline=false,
  leftmargin=\parindent,
  skipabove=\medskipamount,
  skipbelow=\medskipamount
]{thm}

\surroundwithmdframed[
  linewidth=2pt,
  linecolor=gray,
  %topline=false,
  %rightline=false,
  %bottomline=false,
  leftmargin=\parindent,
  skipabove=\medskipamount,
  skipbelow=\medskipamount
]{ex}

%%%%  Ploting  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{tikz}
\usepackage{tikz-cd}
\usetikzlibrary{matrix, positioning, arrows, calc, decorations.pathmorphing,%
  decorations.pathreplacing }
\makeatletter
\global\let\tikz@ensure@dollar@catcode=\relax
\usepackage{tikzpagenodes}
\makeatother

%%%%  figures  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{float}
%TODO: decorate floats
\newcommand{\myfigurestart}{
%\textcolor{lightgray}{\hrule height 2pt}
\hline
\vspace{3mm}
}
\newcommand{\myfigureend}{
\vspace{3mm}
\hline
%\textcolor{lightgray}{\hrule height 2pt}
}

%\usepackage{lipsum}% http://ctan.org/pkg/lipsum
%\usepackage{mdframed}% http://ctan.org/pkg/mdframed
%\usepackage{xcolor}% http://ctan.org/pkg/xcolor
%\newenvironment{inline}{
%\begin{figure}[h!]
  %\begin{mdframed}[backgroundcolor=back!5]
  %\vspace{10pt}%
%}{%
  %\end{mdframed}
%\end{figure}
%}

%%%%  Zeichen  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{nicefrac}
\usepackage{mathrsfs}
\usepackage{stmaryrd}
%simplewick

%%%%  myobracket/myubracket  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{mathtools}
\newcommand{\myobracket}[1]{%
  \textcolor{gray}{%
    \overbracket[0.7pt]{%
      \textcolor{black}{%
        #1%
      }%
    }%
  }%
}
\newcommand{\myubracket}[1]{%
  \textcolor{gray}{%
    \underbracket[0.7pt]{%
      \textcolor{black}{%
        #1%
      }%
    }%
  }%
}


%%%% Tikz Pfeil setup (Für Listings) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\trightarrow}{\hspace{-1pt}\tikz[baseline]{
  \draw[round cap->,line width=0.6pt] (0,0.3em) -- (1.5ex,0.3em);}}
\newcommand{\tleftarrow}{\tikz[baseline]{
  \draw[<-round cap,line width=0.6pt] (0,0.3em) -- (1.5ex,0.3em);}}
\newcommand{\tRightarrow}{\tikz[baseline]{
  \draw[-implies,double,line width=0.7pt] (0,0.3em) -- (2ex,0.3em);}}
\newcommand{\tequiv}{\tikz[baseline]{
  \draw[round cap-round cap, line width=0.6pt] (0,0.45em) -- (1ex,0.45em);
  \draw[round cap-round cap, line width=0.6pt] (0,0.3em) -- (1ex,0.3em);
  \draw[round cap-round cap, line width=0.6pt] (0,0.15em) -- (1ex,0.15em);
  }}
\newcommand{\tneq}{\tikz[baseline]{
  \draw[round cap-round cap, line width=0.6pt] (0,0.4em) -- (1ex,0.4em);
  \draw[round cap-round cap, line width=0.6pt] (0,0.25em) -- (1ex,0.25em);
  \draw[round cap-round cap, line width=0.6pt] (0.2ex,0.1em) -- (0.8ex,0.55em);
  }}
\newcommand{\tdplus}{\tikz[baseline]{
  \draw[round cap-round cap, line width=0.6pt] (0,0.3em) -- (1ex,0.3em);
  \draw[round cap-round cap, line width=0.6pt] (0.3ex,0.1em) -- (0.3ex,0.5em);
  \draw[round cap-round cap, line width=0.6pt] (0.7ex,0.1em) -- (0.7ex,0.5em);
  }}


%%%%  Listings  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{listings}
\lstset{
}


\lstnewenvironment{pseudocode}[2]{
\renewcommand{\lstlistingname}{Algorithmus}
\lstset{
	mathescape = true,
	basicstyle = \small\normalfont\sffamily,
	frame = tb,
	framexleftmargin = 15pt,
  numbers=none,
	breaklines = true,
	xleftmargin = 0.1\linewidth,
	xrightmargin = 0.1\linewidth,
	escapechar=!,
	tabsize=3,
	keywords={if, and, or, is, then, else, endif, while, endwhile, for, from,
	to, do, endfor, Input, Output, Algorithmus, return},
	morecomment=[l]{//},
  deletestring=[b]",
	columns=flexible,
  caption=#1,
  label=#2
}}{}
\renewcommand{\lstlistingname}{Algorithmus}

%%%% Bibliography %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage[style=numeric,backend=biber]{biblatex}
%\usepackage{filecontents}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%  MyDevelop  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{verbatim}
\newcommand{\comm}[1] {}
\newcommand{\ccite}[2][\empty] {}
\newcommand{\TODO}[1][\empty] {}
\ifnum\myDevelopVariable=1
  \usepackage{xcolor}
  \usepackage{color}
  \usepackage{framed}

  \definecolor{commentShadeColor}{rgb}{1.0,.9,.6}
  %\definecolor{commentShadeColor}{rgb}{.6,.6,.6}
  \definecolor{commentTextColor}{rgb}{.2,.2,.2}
  \definecolor{CommentFrameColor}{rgb}{.3,.3,.3}%

  \renewcommand{\comment}{\color{commentTextColor}%
    \def\FrameCommand{\fcolorbox{CommentFrameColor}{commentShadeColor}}%
    \MakeFramed {\FrameRestore}%
    %\textbf{\tiny Kommentar: } 
    }
  \renewcommand{\endcomment}{\endMakeFramed}

  \iftrue
    \usepackage{soul}
    \renewcommand{\comm}[1] {%
      \sethlcolor{commentShadeColor}%
      {\texthl{#1}}}
  \else
    \renewcommand{\comm}[1] {%
      \color{commentTextColor}{#1}}
  \fi

  \usepackage{ifthen}
  \makeatletter
  \renewcommand{\ccite}[2][\empty]{{
    \fcolorbox{CommentFrameColor}{commentShadeColor}{
      \ifthenelse{\equal{#1}{\empty}}
        {\cite{#2}}
        {\cite[#1]{#2}}
    }
  }}
  \makeatother

  \renewcommand{\TODO}[1][\empty]{{
    \fcolorbox{CommentFrameColor}{commentShadeColor}{
      \ifthenelse{\equal{#1}{\empty}}
        {TODO}
        {TODO: #1}
      
  }}}

  %Zeilenhöhe, für bessere lesbarkeit
  \linespread{1.3}

  %\cfoot{-\thepage-\\\today}

  % einseitig, überschreibt KOMA
  %\setboolean{@twoside}{false}

  \surroundwithmdframed[
    linewidth=2pt,
    linecolor=gray,
    topline=false,
    rightline=false,
    bottomline=false,
    leftmargin=\parindent,
    skipabove=\medskipamount,
    skipbelow=\medskipamount
  ]{cor}
  \surroundwithmdframed[
    linewidth=2pt,
    linecolor=gray,
    topline=false,
    rightline=false,
    bottomline=false,
    leftmargin=\parindent,
    skipabove=\medskipamount,
    skipbelow=\medskipamount
  ]{lem}
  \surroundwithmdframed[
    linewidth=2pt,
    linecolor=gray,
    topline=false,
    rightline=false,
    bottomline=false,
    leftmargin=\parindent,
    skipabove=\medskipamount,
    skipbelow=\medskipamount
  ]{prop}
  \surroundwithmdframed[
    topline=false,
    rightline=false,
    bottomline=false,
    leftmargin=\parindent,
    skipabove=\medskipamount,
    skipbelow=\medskipamount
  ]{defn}
  \surroundwithmdframed[
    topline=false,
    rightline=false,
    bottomline=false,
    leftmargin=\parindent,
    skipabove=\medskipamount,
    skipbelow=\medskipamount
  ]{lemdef}
\fi

\ifnum\myDevelopVariable=2
  \usepackage{showframe}
\fi

%\setcounter{chapter}{-1}

% vim:set ft=tex foldmethod=marker foldmarker={{{,}}}:
