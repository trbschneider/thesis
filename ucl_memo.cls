%%
%% This is file `ucl_memo.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% ucl_memo.dcl 
%% 
%% IMPORTANT NOTICE:
%% 
%% For the copyright see the source file.
%% 
%% You are *not* allowed to modify this file.
%% 
%% You are *not* allowed to distribute this file.
%% For distribution of the original source see the terms
%% for copying and modification in the file ucl_memo.dcl.
%% 
%%  A LaTeX2e class for an UCL memo.
%%
%%  Copyright (c) 1995, Russel Winder.
%%
\def \fileversion{v1.0}
\def \filedate{1995/03/16}
\def \docdate{1995/03/16}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ucl_memo}[\filedate\space\fileversion\space UCL Memo class]
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{ucl_letter}}
\ProcessOptions
\LoadClass{ucl_letter}
\DeclareFixedFont{\HelvXrm}{\encodingdefault}{phv}{m}{n}{10\p@}
\DeclareFixedFont{\HelvXIIrm}{\encodingdefault}{phv}{m}{n}{12\p@}
\address{%
  \vbox to 68pt{%
    \vss%
    \hb@xt@ \@headwidth{%
\hskip \@spacer%
\UCLlogo{0.115}%
\hfill
    }%
    \vskip \@spacer
    \hb@xt@ \@headwidth{%
\hskip \@spacer%
\HelvXrm%
DEPARTMENT OF COMPUTER SCIENCE%
\hss%
MEMORANDUM%
\hskip \@spacer%
    }%
    \vskip 0.5\@spacer%
    \hb@xt@ \@headwidth{\hrulefill}%
  }%
}
\renewcommand \opening[1]{%
    \thispagestyle{empty}%
    \centerline{\fromaddress}%
    \vspace{2\parskip}%
    \vskip 4mm%
    \leftline{\HelvXIIrm To: \toname}%
    \vskip 4mm%
    \leftline{\HelvXIIrm From: \fromname}%
    \vskip 4mm%
    \leftline{\HelvXIIrm Cc: \@cclist}%
    \vskip 4mm%
    \leftline{\HelvXIIrm Date:\@date}%
    \vskip 4mm%
    \leftline{\HelvXIIrm Subject: \@subject}%
    \vskip 8mm%
    #1%
    \par%
    \nobreak%
}
\def \@cclist{}
\renewcommand \cc[1]{\gdef \@cclist{#1}}
\def \@subject{}
\newcommand \subject[1]{\gdef \@subject{#1}}
\endinput
%%
%% End of file `ucl_memo.cls'.
