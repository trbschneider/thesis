%%
%% This is file `ucl_document.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% ucl_document.dcl 
%% 
%% IMPORTANT NOTICE:
%% 
%% For the copyright see the source file.
%% 
%% You are *not* allowed to modify this file.
%% 
%% You are *not* allowed to distribute this file.
%% For distribution of the original source see the terms
%% for copying and modification in the file ucl_document.dcl.
%% 
%%  A LaTeX2e class defining a UCL document.
%%
%%  Copyright (c) 1996, Russel Winder and Sean Holden.
%%
\def \fileversion{v1.3}
\def \filedate{1999/08/09}
\def \docdate{\filedate}
\newcommand \doc@stringA {xx}
\newcommand \doc@stringB {xx}
\newcommand \doc@string {\doc@stringA\ \doc@stringB}
\newcommand \doc@tag {xx}
\def \doc@number {\doc@tag/XXXX}
\newcommand \documentnumber [1]{\gdef \doc@number{\doc@tag/#1}}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ucl_document}[\filedate\space\fileversion\space UCL Document class]
\DeclareOption{rn}{%
    \renewcommand{\doc@stringA}{Research}%
    \renewcommand{\doc@stringB}{Note}
    \renewcommand{\doc@tag}{RN}}
\DeclareOption{in}{%
    \renewcommand{\doc@stringA}{Internal}%
    \renewcommand{\doc@stringB}{Note}
    \renewcommand{\doc@tag}{IN}}
\DeclareOption{tr}{%
    \renewcommand{\doc@stringA}{Technical}%
    \renewcommand{\doc@stringB}{Report}
    \renewcommand{\doc@tag}{TR}}
\PassOptionsToClass{a4paper,titlepage,onecolumn}{article}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
\RequirePackage{ucl_a4}
\RequirePackage{ucl_logo}
\RequirePackage{times}
\RequirePackage{ucl_personalise}
\RequirePackage{ifthen}
\def \ps@uclpage{%
    \def \@oddhead{\normalfont \rmfamily \itshape \@titlehead \hfill \@authorhead}%
    \def \@evenhead{\@oddhead}%
    \def \@oddfoot{\normalfont \rmfamily \itshape \doc@number \hfill Page \thepage}%
    \def \@evenfoot{\@oddfoot}%
    \let \@mkboth \@gobbletwo%
    \let \sectionmark \@gobble%
    \let \subsectionmark \@gobble%
}
\pagestyle{uclpage}
\def \ps@firstpage{%
    \let \@oddhead \@empty%
    \let \@evenhead \@empty%
    \def \@oddfoot{\normalfont \rmfamily \itshape \doc@number \hfill Page \thepage}%
    \def \@evenfoot{\@oddfoot}%
    \let \@mkboth \@gobbletwo%
    \let \sectionmark \@gobble%
    \let \subsectionmark \@gobble%
}
\renewcommand \title {\@ifnextchar[{\@gettitletwo}{\@gettitleone}}
\def \@gettitleone#1{\@gettitletwo[#1]{#1}}
\def \@gettitletwo[#1]#2{{%
  \gdef \@title{#2}%
  \def \thanks##1{\relax}%
  \def \\{\unskip \space \ignorespaces}%
  \xdef \@titlehead{#1}%
}}
\renewcommand \author {\@ifnextchar[{\@getauthortwo}{\@getauthorone}}
\def \@getauthorone#1{\@getauthortwo[#1]{#1}}
\def \@getauthortwo[#1]#2{{%
  \gdef \and{\space and\space \ignorespaces}%
  \gdef \@author{#2}%
  \def \and{\unskip \space and\space \ignorespaces}%
  \def \thanks##1{\relax}%
  \def \\{\unskip \space \ignorespaces}%
  \xdef \@authorhead{#1}%
}}
\def \@author{\thepersonname}
\def \@authorhead{\thepersonname}
\def \@address{%
    Department of Computer Science\\%
    University College London\\%
    Gower Street, London~WC1E~6BT,~UK\\[\bigskipamount]%
    \@contactdetails%
}
\newcommand \address[1]{\gdef \@address{#1}}
\def \@contactdetails{%
    \trypersonfield{telephone}{\textit{Telephone\/}: }\\%
    \hbox{\textit{Fax\/}: +44 (0)20 7679 1397}\\%
    \trypersonfield{email}{\textit{Electronic Mail\/}: }\\%
    \trypersonfield{url}{\textit{URL\/}: }%
}
\newcommand \contactdetails[1]{\gdef \@contactdetails{#1}}
\def \@keywords{}
\newcommand \keywords[1]{\gdef \@keywords{#1}}
\renewcommand \maketitle {%
    \par%
    \setcounter{page}{0}%
    \begingroup%
        \renewcommand \thefootnote{\fnsymbol{footnote}}%
        \def \@makefnmark{\hb@xt@ \z@{$\m@th^{\@thefnmark}$\hss}}%
        \long \def \@makefntext##1{\parindent 1em\noindent%
            \hb@xt@ 1.8em{\hss$\m@th^{\@thefnmark}$}##1}%
        \setcounter{footnote}{1}%
        \if@titlepage%
            \@makedisplaytitle%
        \else%
            \if@twocolumn%
                \twocolumn[\@makenondisplaytitle]%
            \else%
                \newpage%
                \global \@topnum = \z@%
                \@makenondisplaytitle%
            \fi%
        \fi%
        \@thanks%
    \endgroup
    \setcounter{footnote}{0}%
    \let \thanks = \relax%
    \markboth{\@titlehead\hfill\@authorhead}{\@titlehead\hfill\@authorhead}%
    \gdef \@address{}%
    \gdef \@thanks{}%
    \gdef \@author{}%
    \gdef \@title{}%
    \let \@makenondisplaytitle = \relax%
    \let \@makedisplaytitle = \relax%
    \let \maketitle = \relax%
}
\newcommand \@makedisplaytitle{%
    \titlepage%
    \newlength{\tmpparindent}%
    \setlength{\tmpparindent}{\parindent}%
    \setlength{\parindent}{0em}%
    \leftline{\@masthead}
    \vfill%
    \setlength{\parindent}{\tmpparindent}%
    \begin{center}%
        {\Large \bfseries \@title}\\%
        \vspace{4em}%
        \begin{tabular}[t]{c}%
            {\large \itshape \@author}\\[3em]%
    \@contactdetails%
        \end{tabular}%
    \end{center}%
    \vfill%
}
\newcommand \@makenondisplaytitle{%
    \newpage%
    \null%
    \setcounter{page}{1}%
    \thispagestyle{firstpage}%
    \vspace{1em}%
    \leftline{\@masthead}%
    \vspace{1em}%
    \begin{center}%
        {\Large \bfseries \@title \par}%
        \vspace{2em}%
        {\large \itshape
         \begin{tabular}[t]{c}%
            \@author%
         \end{tabular}}%
        \par%
        \vspace{1em}%
        {\normalsize \itshape%
         \begin{tabular}[t]{c}%
            \@address%
         \end{tabular}}%
    \end{center}%
}
\newcommand \@masthead{\vbox{\hb@xt@ \hsize{\doc@number \hfill \doc@stringA}%
             \hb@xt@ \hsize{\@date \hfill \doc@stringB}}}
\DeclareFixedFont{\HelvXIIo}{\encodingdefault}{phv}{m}{sl}{12\p@}
\DeclareFixedFont{\HelvXIVo}{\encodingdefault}{phv}{m}{sl}{14\p@}
\renewenvironment{abstract}{%
    \if@twocolumn%
        \section*{Abstract}%
    \else%
        \begin{center}%
            {\large \bfseries Abstract}%
        \end{center}%
        \begin{quote}%
            \parskip 0.7em%
            \parindent \z@%
    \fi%
}{%
    \if@twocolumn%
        \setbox0 = \hbox{\@keywords}%
        \ifdim \wd0 > \z@%
            \par%
            \noindent%
            {\bfseries Keywords:} \@keywords%
        \fi%
    \else%
        \end{quote}%
        \setbox0 = \hbox{\@keywords}%
        \ifdim \wd0 > \z@%
            \vspace{1em}%
            \begin{center}%
                {\large \bfseries Keywords}\\[0.8em]%
                \@keywords%
            \end{center}%
        \fi%
    \fi%
    \if@titlepage%
        \vfill%
        \leftline{%
            \UCLlogo{0.15}%
            \hfill%
    \vbox{%
\baselineskip = 14\p@%
                \hbox{\HelvXIVo Department of Computer Science}%
                \hbox{\HelvXIIo University College London}%
                \hbox{\HelvXIIo Gower Street}%
                \hbox{\HelvXIIo London WC1E 6BT, UK}%
                \vspace{1em}%
             }%
        }%
        \endtitlepage%
        \if@twocolumn%
            \twocolumn%
        \fi%
    \fi%
}
\renewcommand \footnoterule {%
    \kern -3\p@%
    \hrule%
    \kern 2.6\p@%
}
\setlength \headsep  {16\p@}
\setlength \parindent{0\p@}
\ifcase \@ptsize
    \setlength \footskip{26\p@}
    \setlength \parskip {8\p@ \@plus 1\p@ \@minus 1\p@}
\or
    \setlength \footskip{27\p@}
    \setlength \parskip {9\p@ \@plus 1\p@ \@minus 1\p@}
\or
    \setlength \footskip{28\p@}
    \setlength \parskip {10\p@ \@plus 1\p@ \@minus 1\p@}
\fi
\setlength \partopsep \z@
\renewcommand \section{\@startsection {section}{1}{\z@}%
                                      {-1ex \@plus -.4ex \@minus -.2ex}%
                                      {.6ex \@plus .1ex}%
                                      {\reset@font \large \bfseries}}
\renewcommand \subsection{\@startsection{subsection}{2}{\z@}%
                                        {-.7ex\@plus -.3ex \@minus -.2ex}%
                                        {.4ex \@plus .1ex}%
                                        {\reset@font \large \bfseries \itshape}}
\renewcommand \subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                        {-.5ex\@plus -.2ex \@minus -.2ex}%
                                        {.3ex \@plus .1ex}%
                                        {\reset@font \large \itshape}}
\renewcommand \paragraph{\@startsection{paragraph}{4}{\z@}%
                                       {.5ex \@plus .1ex \@minus .2ex}%
                                       {-.2em}%
                                       {\reset@font \normalsize \bfseries}}
\renewcommand \subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                          {-.3ex \@plus .1ex \@minus .2ex}%
                                          {-.2em}%
                                          {\reset@font \normalsize \bfseries}}
\tolerance 1000
\hfuzz 2pt
\flushbottom
\endinput
%%
%% End of file `ucl_document.cls'.
