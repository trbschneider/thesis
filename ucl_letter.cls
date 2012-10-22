%%
%% This is file `ucl_letter.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% ucl_letter.dcl 
%% 
%% IMPORTANT NOTICE:
%% 
%% For the copyright see the source file.
%% 
%% You are *not* allowed to modify this file.
%% 
%% You are *not* allowed to distribute this file.
%% For distribution of the original source see the terms
%% for copying and modification in the file ucl_letter.dcl.
%% 
%%  A LaTeX2e class for an UCL letter.
%%
%%  Copyright (c) 1995, Russel Winder and Sean Holden.
%%
\def \fileversion{v1.3}
\def \filedate{1999/08/09}
\def \docdate{1999/08/09}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ucl_letter}[\filedate\space\fileversion\space UCL Letter class]
\newif \ifpersonalisehead
\personaliseheadfalse
\DeclareOption{personalisehead}{\personaliseheadtrue}
\PassOptionsToClass{a4paper}{letter}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{letter}}
\ProcessOptions
\LoadClass{letter}
\RequirePackage{ucl_a4}
\RequirePackage{ucl_logo}
\RequirePackage{times}
\RequirePackage{ucl_personalise}
\RequirePackage{ifthen}
\DeclareFixedFont{\HelvVIIIrm}{\encodingdefault}{phv}{m}{n}{8\p@}
\DeclareFixedFont{\HelvIXrm}{\encodingdefault}{phv}{m}{n}{9\p@}
\DeclareFixedFont{\HelvIXb}{\encodingdefault}{phv}{b}{n}{9\p@}
\DeclareFixedFont{\HelvXrm}{\encodingdefault}{phv}{m}{n}{10\p@}
\DeclareFixedFont{\HelvXb}{\encodingdefault}{phv}{b}{n}{10\p@}
\let \@texttop \relax
\renewcommand \opening[1]{%
    \thispagestyle{letterfirst}%
    \centerline{\fromaddress}%
    \vspace{1.5em}%
    {\raggedleft \@date \par}%
    \vspace{0.75em}%
    {\raggedright \toname \\ \toaddress \par}%
    \vspace{1.5em}%
    #1%
    \par%
    \nobreak%
}
\setlength \longindentation{\z@}
\setlength \indentedwidth{\textwidth}
\def \ps@letter{%
    \let \@mkboth \@gobbletwo%
    \def \@oddhead{\itshape\hfill-- \thepage\ --\hfill}%
    \def \@oddfoot{}%
    \let \@evenhead \@oddhead%
    \let \@evenfoot \@oddfoot%
}
\setlength \headsep {16\p@}
\pagestyle{letter}
\def \ps@letterfirst{%
    \let \@mkboth \@gobbletwo%
    \def \@oddhead{}%
    \def \@oddfoot{\ifthenelse{\boolean{seniorfooter}}{\@seniorfootline}{}}%
    \let \@evenhead \@oddhead%
    \let \@evenfoot \@oddfoot%
}
\newboolean{seniorfooter}
\setboolean{seniorfooter}{true}
\newcommand \@seniorfootline{%
    \centerline{%
\lower 3ex \vbox{%
    \baselineskip = 10\p@%
    \HelvVIIIrm%
            \hb@xt@ \@headwidth{\hrulefill}%
    \vskip 4\p@%
    \hb@xt@ \@headwidth{\hss Professors: Anthony Finkelstein (Head of Department),\hss}%
    \hb@xt@ \@headwidth{\hss Simon R Arridge, Bernard F Buxton, John A Campbell, David T Jones,\hss}%
    \hb@xt@ \@headwidth{\hss Peter T Kirstein, Mel Slater, Harold Thimbleby, Philip C Treleaven, John Washbrook, Steve R Wilbur.\hss}%
    \hb@xt@ \@headwidth{\hss Reader: M Angela Sasse\hss}%
}%
    }%
}
\ifpersonalisehead
\signature{%
    \dopersonfield{name}{}%
}
\else
\signature{%
    \hbox{%
          \dopersonfield{name}{}%
  \hskip 0.5em%
          \trypersonfield{postnominal}{\small}%
    }%
    \small%
    \trypersonfield{title}{\itshape}%
    \trypersonfield{role}{\itshape}%
    \vspace{4\p@}%
    \trypersonfield{telephone}{\textit{Telephone\/}: }%
    \trypersonfield{email}{\textit{Electronic Mail\/}: }%
    \trypersonfield{url}{\textit{URL\/}: }%
}
\fi
\name{\thepersonname}
\newlength{\@headwidth}
\setlength{\@headwidth}{180mm}
\newlength{\@spacer}
\setlength{\@spacer}{2mm}
\address{%
  \vbox to
\ifpersonalisehead%
  32mm%
\else%
  23mm%
\fi%
  {%
    \vss%
    \baselineskip=12\p@%
    \HelvXrm%
    \hb@xt@ \@headwidth{%
        \hskip \@spacer%
        \vbox{%
            \baselineskip=10\p@%
            \UCLlogo{0.115}%
            \vskip 8\p@%%
            \hbox{\HelvXrm DEPARTMENT OF COMPUTER SCIENCE}%
            \ifpersonalisehead%
                \vskip 8\p@%
                \hbox{%
                    \hskip \@spacer%
                    \dopersonfield{name}{\HelvIXb}%
    \hskip 0.5em%
    \trypersonfield{postnominal}{\HelvVIIIrm}%
                }%
                \trypersonfield{title}{\HelvVIIIrm \hskip \@spacer}%
                \trypersonfield{role}{\HelvVIIIrm \hskip \@spacer}%
            \fi%
        }%
        \hfill%
        \vbox{%
            \hbox{University College London}%
            \hbox{Gower Street}%
            \hbox{London {\HelvIXrm WC1E 6BT}, {\HelvIXrm UK}}%
            \hbox{}%
            \baselineskip=10\p@%
            \HelvIXrm%
            \ifpersonalisehead%
                \dopersonfield{telephone}{}%
            \else%
                \hbox{+44 (0)20 7679 7214}%
            \fi%
            \hbox{+44 (0)20 7679 1397 fax}%
            \ifpersonalisehead%
                \dopersonfield{email}{}%
                \trypersonfield{url}{\HelvVIIIrm}%
            \fi%
            \hbox{}%
        }%
    }%
    \vskip 0.5\@spacer%
    \hb@xt@ \@headwidth{\hrulefill}%
  }%
}
\endinput
%%
%% End of file `ucl_letter.cls'.
