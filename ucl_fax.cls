%%
%% This is file `ucl_fax.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% ucl_fax.dcl 
%% 
%% IMPORTANT NOTICE:
%% 
%% For the copyright see the source file.
%% 
%% You are *not* allowed to modify this file.
%% 
%% You are *not* allowed to distribute this file.
%% For distribution of the original source see the terms
%% for copying and modification in the file ucl_fax.dcl.
%% 
%%  A LaTeX2e class for an UCL fax.
%%
%%  Copyright (c) 1995, Russel Winder and Sean Holden.
%%
\def \fileversion{v1.1}
\def \filedate{1999/08/09}
\def \docdate{1999/08/09}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ucl_fax}[\filedate\space\fileversion\space UCL Fax class]
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{ucl_letter}}
\ProcessOptions
\LoadClass{ucl_letter}
\DeclareFixedFont{\HelvXIIrm}{\encodingdefault}{phv}{m}{n}{12\p@}
\DeclareFixedFont{\HelvXVIIIrm}{\encodingdefault}{phv}{m}{n}{18\p@}
\renewcommand \opening[1]{%
    \thispagestyle{empty}%
    \centerline{\fromaddress}%
    \vspace{2\parskip}%
    \vskip 10mm%
    \centerline{\HelvXVIIIrm Facsimile Cover Sheet}%
    \vskip 10mm%
    \leftline{\HelvXIIrm To: \toname}%
    \vskip 4mm%
    \leftline{\HelvXIIrm At Fax Number: \@tofaxnumber}%
    \vskip 8mm%
    \leftline{\HelvXIIrm From: \fromname}%
    \vskip 4mm%
    \leftline{\HelvXIIrm Date: \@date}%
    \vskip 4mm%
    \leftline{\HelvXIIrm Number of Sheets Including This One:}%
    \vskip 8mm%
    \leftline{\HelvXIIrm If this fax is unreadable or incomplete please telephone \@faxproblemnumber}%
    \vskip 8mm%
    \leftline{\HelvXIIrm Remarks:}%
    \vskip 8mm%
    #1%
    \par%
    \nobreak%
}
\def \@tofaxnumber{}
\newcommand \tofax[1]{\gdef \@tofaxnumber{#1}}
\def \@faxproblemnumber{+44 (0)20 7679 7777 ext}
\newcommand \faxproblemnumber[1]{\gdef \@faxproblemnumber{#1}}
\endinput
%%
%% End of file `ucl_fax.cls'.
