#! /usr/bin/env perl
use strict;
use warnings;
use Term::ANSIColor;
use Text::Wrap;
use Encode;
use utf8;
#use locale;
#use POSIX qw(locale_h);
#setlocale(LC_CTYPE,"no_NO");


my $versionnumber="2.3.alpha.480";
my $versiondate="2010 Nov 22";

###### Set global settings and variables

# Options and states
my $strictness=0; # Flag to check for undefined groups 
my $verbose=0; # Level of verbosity
my $showcodes=1; # Flag to show overview of colour codes
my $showstates=0; # Flag to show internal state in verbose log
my $showsubcounts=0; # Write subcounts if #>this, or not (if 0)
my $showVersion=0; # Indicator that version info be included (1) or not (-1)
my $totalflag=0; # Flag to write only total summary
my $htmlstyle=0; # Flag to print HTML
my $includeTeX=0; # Flag to parse included files
my $briefsum=0; # Flag to set brief summary
my @sumweights; # Set count weights for computing sum
my $utf8flag=0; # Flag to parse in UTF8
my $outputtemplate; # Output template
my %substitutions; # Substitutions to make globally
my %WordFreq; # Hash for counting words
my $optionWordFreq=0; # Count words of this frequency, or don't count if 0 ;
my $optionFast=1; # Flag inticating fast method 

# Global variables (for internal use only)
my $blankline=0; # Number of blank lines printed
my $errorcount=0; # Number of errors in parsing
my %warnings=(); # Warnings

###### Set CMD specific settings and variables

## Preset command line options
# List of options (stings) separated by comma,
# e.g. ("-inc","-v") to parse included files and
# give verbose output by default.
my @StartupOptions=();

# CMD specific global variables
my @filelist; # List of files to parse
my $workdir; # Root directory (taken from filename)
my $globalworkdir=""; # Overrules workdir (default=present root)
my $fileFromSTDIN=0; # Flag to set input from STDIN
my $_STDIN_="<STDIN>"; # File name to represent STDIN

# CMD specific settings
$Text::Wrap::columns=76; # Page width for wrapped output

###### Set global definitions

### Count labels
# Labels used to describe the counts
my @countlabel=('Files','Words in text','Words in headers',
      'Words in float captions','Number of headers','Number of floats',
      'Number of math inlines','Number of math displayed');

### Break points
# Definition of macros that define break points that start a new subcount.
# The values given are used as labels.
my %BreakPointsOptions;
$BreakPointsOptions{'none'}={};
$BreakPointsOptions{'part'}={%{$BreakPointsOptions{'none'}},'\part'=>'Part'};
$BreakPointsOptions{'chapter'}={%{$BreakPointsOptions{'part'}},'\chapter'=>'Chapter'};
$BreakPointsOptions{'section'}={%{$BreakPointsOptions{'chapter'}},'\section'=>'Section'};
$BreakPointsOptions{'subsection'}={%{$BreakPointsOptions{'section'}},'\subsection'=>'Subsection'};
$BreakPointsOptions{'default'}=$BreakPointsOptions{'subsection'};
my %BreakPoints=%{$BreakPointsOptions{'none'}};

### Print styles
# Definition of different print styles: maps of class labels
# to ANSI codes. Class labels are as used by HTML styles.
my @STYLES=();
my %STYLE;
$STYLES[0]={'error'=>'bold red'};
$STYLES[1]={%{$STYLES[0]},''=>'normal',' '=>'normal',
            'word1'=>'blue','word2'=>'bold blue','word3'=>'blue',
            'grouping'=>'red','document'=>'red','mathgroup'=>'magenta',
            'state'=>'cyan underline','cumsum'=>'yellow'};
$STYLES[2]={%{$STYLES[1]},
            'command'=>'green','exclcommand'=>'yellow','exclgroup'=>'yellow','exclmath'=>'yellow',
            'ignore'=>'cyan'};
$STYLES[3]={%{$STYLES[2]},
            'tc'=>'bold yellow','comment'=>'yellow','option'=>'yellow',
            'fileinclude'=>'bold green'};
$STYLES[4]={%{$STYLES[3]}};

### Word regexp pattern list
# List of regexp patterns that should be analysed as words.
# Use @ to represent a letter, will be substituted with $LetterPattern.
my @WordPatterns=('(@+\.)+@+\.?','@+([\-\']@+)*');
my $WordPatternsJoined;
my $specialchars='\\\\(ae|AE|o|O|aa|AA)';
my $modifiedchars='\\\\[\'\"\`\~\^\=](\w|\{\w\})';
my $LetterPattern='\w';
my $LetterPatternRelaxed='([\w\-\']|'.$modifiedchars.'|'.$specialchars.'(\{\})?|\{'.$specialchars.'\}|\{\w\})';
my %NamedWordPattern;
$NamedWordPattern{'chinese'}='\p{script=Han}';
$NamedWordPattern{'japanese'}='(\p{script=Han}|\p{script=Hiragana}|\p{script=Katakana})';
$NamedWordPattern{'letters'}='@';

### Macro option regexp list
# List of regexp patterns to be gobbled as macro option in and after
# a macro.
my @MacroOptionPatterns=('\[(\w|[,\-\s\~\.\:\;\+\?\*\_\=])*\]');
my @MacroOptionPatternsRelaxed=('\[[^\[\]\n]*\]');

###### Define core rules

### Macros for headers
# Macros that identify headers: i.e. following token or
# {...} is counted as header. The =>[2] indicates transition to
# state 2 which is used within headers (although the value is
# actually never used). This is copied to %TeXmacro and the
# only role of defining it here is that the counter for the number
# of headers is incremented by one.
my %TeXheader=('\title'=>[2],'\part'=>[2],'\chapter'=>[2],
     '\section'=>[2],'\subsection'=>[2],'\subsubsection'=>[2],
     '\paragraph'=>[2],'\subparagraph'=>[2]);

### Macros indicating package inclusion
# Will always be assumed to take one parameter (plus options).
my %TeXpackageinc=('\usepackage'=>1);

### Macros that are counted within the preamble
# The preamble is the text between \documentclass and \begin{document}.
# Text and macros in the preamble is ignored unless specified here. The
# value is the status (1=text, 2=header, etc.) they should be interpreted as.
# Note that only the first unit (token or {...} block) is counted.
my %TeXpreamble=(
     '\title'=>[2],
     '\newcommand'=>[-3,-3],'\renewcommand'=>[-3,-3],
     '\newenvironment'=>[-3,-3,-3], 'renewenvironment'=>[-3,-3,-3],
     );

### In floats: include only specific macros
# Macros used to identify caption text within floats.
my %TeXfloatinc=('\caption'=>[3]);

### How many tokens to gobble after macro
# Each macro is assumed to gobble up a given number of
# tokens (or {...} groups), as well as options [...] before, within
# and after. The %TeXmacro hash gives a link from a macro
# (or beginNAME for begin-end groups without the backslash)
# to either an integer giving the number of tokens to ignore
# or to an array (specified as [num,num,...]) of length N where
# N is the number of tokens to be read with the macro and the
# array values tell how each is to be interpreted (see the status
# values: 0=ignore, 1=count, etc.). Thus specifying a number N is
# equivalent to specifying an array [0,...,0] of N zeros.
#
# For macros not specified here, the default value is 0: i.e.
# no tokens are excluded, but [...] options are. Header macros
# specified in %TeXheader are automatically included here.
my %TeXmacro=(%TeXheader,%TeXpreamble,%TeXfloatinc,
     '\documentclass'=>1,'\documentstyle'=>1,'\usepackage'=>1, '\hyphenation'=>1,
     '\pagestyle'=>1,'\thispagestyle'=>1, '\pagenumbering'=>1,'\markboth'=>1, '\markright'=>1,
     '\newcommand'=>[-3,-3],'\renewcommand'=>[-3,-3],
     '\newenvironment'=>[-3,-3,-3], 'renewenvironment'=>[-3,-3,-3],
     '\newfont'=>2,'\newtheorem'=>2,'\bibliographystyle'=>1, '\bibliography'=>1,
     '\parbox'=>1, '\marginpar'=>[3],'\makebox'=>0, '\raisebox'=>1, '\framebox'=>0,
     '\newsavebox'=>1, '\sbox'=>1, '\savebox'=>2, '\usebox'=>1,'\rule'=>2,
     '\footnote'=>[3],'\label'=>1, '\ref'=>1, '\pageref'=>1, '\bibitem'=>1,
     '\cite'=>1, '\citep'=>1, '\citet'=>1, '\citeauthor'=>1, '\citealt'=>1, '\nocite'=>1,
     '\eqlabel'=>1, '\eqref'=>1,'\hspace'=>1, '\vspace'=>1, '\addvspace'=>1,
     '\input'=>1, '\include'=>1, '\includeonly'=>1,'\includegraphics'=>1,
     '\newlength'=>1, '\setlength'=>2, '\addtolength'=>2,'\settodepth'=>2,
     '\settoheight'=>2, '\settowidth'=>2,'\newcounter'=>1, '\setcounter'=>2,
     '\addtocounter'=>2,'\stepcounter'=>1, '\refstepcounter'=>1, '\usecounter'=>1,
     '\alph'=>1, '\arabic'=>1, '\fnsymbol'=>1, '\roman'=>1, '\value'=>1,
     '\cline'=>1, '\multicolumn'=>3,'\typeout'=>1, '\typein'=>1,
     'beginlist'=>2, 'beginminipage'=>1, 'begintabular'=>1,
     'beginthebibliography'=>1,'beginlrbox'=>1,
     '\begin'=>1,'\end'=>1,'\title'=>[2],
					'\addtocontents'=>2,'\addcontentsline'=>3);

### Macros that should be counted as one or more words
# Macros that represent text may be declared here. The value gives
# the number of words the macro represents.
my %TeXmacroword=('\LaTeX'=>1,'\TeX'=>1);

### Begin-End groups
# Identified as begin-end groups, and define =>state. The
# states used corresponds to the elements of the count array, and
# are:
#    0: Not included
#    1: Text, words included in text count
#    2: Header, words included in header count
#    3: Float caption, words included in float caption count
#    6: Inline mathematics, words not counted
#    7: Displayed mathematics, words not counted
#   -1: Float, not included, but looks for captions
#
#    4 and 5 are used to count number of headers and floats
#    and are not used as states.
#
# Groups that are not defined will be counted as the surrounding text.
#
# Note that some environments may only exist within math-mode, and
# therefore need not be defined here: in fact, they should not as it
# is not clear if they will be in inlined or displayed math.
#
my %TeXgroup=('document'=>1,'letter'=>1,'titlepage'=>0,
     'center'=>1,'flushleft'=>1,'flushright'=>1,
     'abstract'=>1,'quote'=>1,'quotation'=>1,'verse'=>1,'minipage'=>1,'verbatim'=>1,
     'description'=>1,'enumerate'=>1,'itemize'=>1,'list'=>1,
     'theorem'=>1,'lemma'=>1,'definition'=>1,'corollary'=>1,'example'=>1,
     'math'=>6,'displaymath'=>7,'equation'=>7,'eqnarray'=>7,'align'=>7,
     'equation*'=>7,'eqnarray*'=>7,'align*'=>7,
     'figure'=>-1,'float'=>-1,'picture'=>-1,'table'=>-1,
     'tabbing'=>0,'tabular'=>0,'thebibliography'=>0,'lrbox'=>0);

### Macros for including tex files
# Allows \macro{file} or \macro file. If the value is 0, the filename will
# be used as is; if it is 1, the filetype .tex will be added if the
# filename is without filetype; if it is 2, the filetype .tex will be added.
my %TeXfileinclude=('\input'=>1,'\include'=>2);


###### Define package specific rules

### Package rule definitions

my %PackageTeXmacro=(); # TeXmacro definitions per package
my %PackageTeXheader=(); # TeXheader definitions per package
my %PackageTeXgroup=(); # TeXgroup definitions per package

# Rules for package psfig
$PackageTeXmacro{'psfig'}={('\psfig'=>1)};


###### Main script


###################################################

MAIN(@ARGV);
exit; # Just to make sure it ends here...

###################################################


#########
######### Main routines
#########

# MAIN ROUTINE: Handle arguments, then parse files
sub MAIN {
  my @args;
	push @args,@StartupOptions;
	push @args,@_;
  Initialise();
  Check_Arguments(@args);
  my @toplevelfiles=Parse_Arguments(@args);
  Apply_Options();
  if (scalar(@toplevelfiles)==0 && !$fileFromSTDIN) {
    conditional_print_help_style()
    || print_error("No files specified.","p","error");
  } else {
    if ($showVersion && !$htmlstyle && !($briefsum && $totalflag)) {
      print "\n=== LaTeX word count (TeXcount version ",$versionnumber,") ===\n\n";
    }
    conditional_print_help_style();
    my $totalcount=Parse_file_list(@toplevelfiles);
    conditional_print_total($totalcount);
    Report_Errors();
    if ($optionWordFreq) {print_word_freq();}
  }
  Close_Output();
}

# Initialise, overrule initial settings, etc.
sub Initialise {
  _option_subcount();
  # Windows settings
  if ($^O=~/^MSWin/) {
    option_ansi_colours(0);
  }
}

# Check arguments, exit on exit condition
sub Check_Arguments {
  my @args=@_;
  if (!@args) {
    print_version();
    print_syntax();
    print_reference();
    exit;
  } elsif ($args[0]=~/^(\-?\-(h|\?|help)|\/(\?|h))$/) {
    print_help();
    exit;
  } elsif ($args[0]=~/^(\-?\-(h|\?|help)|\/(\?|h))=/) {
    print_help_on("$'");
    exit;
  } elsif ($args[0]=~/^\-?\-(ver|version)$/) {
    print_version();
    exit;
  } elsif ($args[0]=~/^\-?\-(lic|license|licence)$/) {
    print_license();
    exit;
  }
  return 1;
}

# Parse arguments, set options (global) and return file list
sub Parse_Arguments {
  my @args=@_;
  my @files;
  foreach my $arg (@args) {
    if (parse_option($arg)) {next;}
    if ($arg=~/^\-/) {
      print 'Invalid opton '.$arg."\n";
      print_syntax();
      exit;
    }
    $arg=~s/\\/\//g;
    push @files,$arg;
  }
  return @files;
}

# Parse individual option parameters
sub parse_option {
  my $arg=shift @_;
  return parse_options_preset($arg) 
  || parse_options_parsing($arg)
  || parse_options_counts($arg)
  || parse_options_output($arg)
  || parse_options_format($arg)
  ;
}

# Parse presetting options
sub parse_options_preset {
  my $arg=shift @_;
  if ($arg=~/^-(opt|option|options|optionfile)=/) {
    _parse_optionfile($');
  }
  else {return 0;}
  return 1;
}

# Parse parsing options
sub parse_options_parsing {
  my $arg=shift @_;
  if ($arg eq '-') {$fileFromSTDIN=1;}
  elsif ($arg eq '-merge') {$includeTeX=2;}
  elsif ($arg eq '-inc') {$includeTeX=1;}
  elsif ($arg eq '-noinc') {$includeTeX=0;}
  elsif ($arg=~/^-(includepackage|incpackage|package|pack)=(.*)$/) {include_package($2);}
  elsif ($arg eq '-dir') {$globalworkdir=undef;}
  elsif ($arg=~/^-dir=(.*)$/) {
	  $globalworkdir=$1;
	  $globalworkdir=~s/([^\/\\])$/$1\//;
	  print 'WORKDIR: '.$globalworkdir."\n";
	} elsif ($arg=~/^-(utf8|unicode)$/) {$utf8flag=1;}
  elsif ($arg=~/^-(ch|chinese|zhongwen)$/) {
    $utf8flag=1;
    @WordPatterns=($NamedWordPattern{'chinese'},@WordPatterns);
  }
  elsif ($arg=~/^-(jp|japanese)$/) {
    $utf8flag=1;
    @WordPatterns=($NamedWordPattern{'japanese'},@WordPatterns);
  }
  elsif ($arg=~/^-(char|character|letter)s?$/) {
    @WordPatterns=($NamedWordPattern{'letters'});
    $countlabel[1]='Letters in text';
    $countlabel[2]='Letters in headers';
    $countlabel[3]='Letters in captions';
  }
  elsif ($arg eq '-relaxed') {
    @MacroOptionPatterns=@MacroOptionPatternsRelaxed;
    $LetterPattern=$LetterPatternRelaxed;
  }
  elsif ($arg eq '-freq') {$optionWordFreq=1;}
  elsif ($arg=~/^-freq=(\d+)$/) {$optionWordFreq=$1;}
  else {return 0;}
  return 1;
}

# Parse count and summation options
sub parse_options_counts {
  my $arg=shift @_;
  if ($arg=~/^-sum(=(.+))?$/) {_option_sum($2);}
  elsif ($arg=~/^-nosum/) {@sumweights=();}
  elsif ($arg=~/^-(sub|subcounts?)(=(.+))?$/) {_option_subcount($3);}
  elsif ($arg=~/^-(nosub|nosubcounts?)/) {$showsubcounts=0;}
  else {return 0;}
  return 1;
}

# Apply sum option
sub _option_sum {
  my $arg=shift @_;
  if (!defined $arg) {
    @sumweights=(1,1,1,0,0,1,1);
  } elsif ($arg=~/^(\d+(\.\d*)?(,\d+(\.\d*)?){0,6})$/) {
    @sumweights=split(',',$1);
  } else {
    print STDERR "Warning: Option value ".$arg." not valid, ignoring option.\n";
  }
}

# Apply subcount options
sub _option_subcount {
  my $arg=shift @_;
  $showsubcounts=2;
  if (!defined $arg) {
    %BreakPoints=%{$BreakPointsOptions{'default'}};
  } elsif (my $option=$BreakPointsOptions{$arg}) {
    %BreakPoints=%{$option};
  } else {
    print STDERR "Warning: Option value ".$arg." not valid, using default instead.\n";
    %BreakPoints=%{$BreakPointsOptions{'default'}};
  }
}

# Parse output and verbosity options
sub parse_options_output {
  my $arg=shift @_;
  if ($arg eq '-strict') {$strictness=1;}
  elsif ($arg eq "-v0") {$verbose=0;}
  elsif ($arg eq "-v1") {$verbose=1;}
  elsif ($arg eq '-v2') {$verbose=2;}
  elsif ($arg eq '-v3' || $arg eq '-v') {$verbose=3;}
  elsif ($arg eq '-v4') {$verbose=3; $showstates=1;}
  elsif ($arg =~ /^\-showstates?$/ ) {$showstates=1;}
  elsif ($arg =~ /^-(q|-?quiet)$/ ) {$verbose=-1;}
  elsif ($arg =~ /^-(template)=(.*)$/ ) {_set_output_template($2);}
  elsif ($arg eq '-split') {$optionFast=1;}
  elsif ($arg eq '-nosplit') {$optionFast=0;}
  elsif ($arg eq '-showver') {$showVersion=1;}
  elsif ($arg eq '-nover') {$showVersion=-1;}
  else {return 0;}
  return 1;
}

# Set output template
sub _set_output_template {
  my $template=shift @_;
  $outputtemplate=$template;
  if ($template=~/\{(S|SUM)[\?\}]/i && !@sumweights) {
    @sumweights=(1,1,1,0,0,1,1);
  }
  if ($template=~/\{SUB\?/i && !$showsubcounts) {
    _option_subcount();
  }
}

# Parse output formating options
sub parse_options_format {
  my $arg=shift @_;
  if ($arg eq '-brief') {$briefsum=1;}
  elsif ($arg eq '-total') {$totalflag=1;}
  elsif ($arg eq '-1') {$briefsum=1;$totalflag=1;$verbose=-1;}
  elsif ($arg eq "-html" ) {option_ansi_colours(0);$htmlstyle = 2;}
  elsif ($arg eq "-htmlcore" ) {option_ansi_colours(0);$htmlstyle = 1;}
  elsif ($arg=~/^\-(nocol|nc$)/) {option_ansi_colours(0);}
  elsif ($arg=~/^\-(col$)/) {option_ansi_colours(1);}
  elsif ($arg eq '-codes') {
    $showcodes=2;
    if ($verbose==0) {$verbose=3;}
  }
  elsif ($arg eq '-nocodes') {$showcodes=0;}
  else {return 0;}
  return 1;
}

# Include options from option file
sub _parse_optionfile {
  my $filename=shift @_;
  open(FH,"<",$filename)
    || die "Option file not found: ".$filename."\n";
  my @options=<FH>;
  close(FH);
  s/^\s*(#.*|)//s for @options;
  my $text=join('',@options);
  $text=~s/(\n|\r|\r\n)\s*\\//g;
  @options=split("\n",$text);
  foreach my $arg (@options) {
    __optionfile_tc($arg)
      || parse_option($arg)
      || die "Invalid option ".$arg." in ".$filename."\n";
  }
}

# Parse option file TC options
sub __optionfile_tc {
  my $arg=shift @_;
  $arg=~s/^\%\s*// || return 0;
  if ($arg=~/^subst\s+(\\\w+)\s+/i) {
    $substitutions{$1}=$';
  } elsif ($arg=~/^(\w+)\s+([\\]*\w+)\s+([^\s\n]+)(\s+([0-9]+))?/i) {
    tc_macro_param_option($1,$2,$3,$5)
      || die "Invalid TC option: ".$arg."\n";
  } else {
    print "Invalid TC option format: ".$arg."\n";
    return 0;
  }
  return 1;
}

# Parse file list and return total count
sub Parse_file_list {
  my @files=@_;
  my $listtotalcount=new_count("Total");
  foreach (@files) {
	  s/\\/\//g;
	  s/ /\\ /g;
	}
	if (@files) {
	  @files=<@files>; # For the sake of Windows: expand wildcards!
    for my $file (@files) {
      my $filetotalcount=parse_file($file);
      add_to_total($listtotalcount,$filetotalcount);
    }
  }
  if ($fileFromSTDIN) {
    my $filetotalcount=parse_file($_STDIN_);
    add_to_total($listtotalcount,$filetotalcount);
	}
  return $listtotalcount;
}

# Parse file and included files, and return total count
sub parse_file {
  my $file=shift @_;
  $workdir=$globalworkdir;
  if (!defined $workdir) {
    $workdir=$file;
    $workdir =~ s/^((.*[\\\/])?)[^\\\/]+$/$1/;
  }
  @filelist=($file);
  if ($htmlstyle && ($verbose || !$totalflag)) {print "\n<div class='filegroup'>\n";}
  my $filetotalcount=new_count("File(s): ".$file);
  foreach my $f (@filelist) {
    my $tex=TeXfile($f);
    my $fpath=$f;
    $fpath=~s/^((.*[\\\/])?)[^\\\/]+$/$1/;
    if (!defined $tex) {
      print STDERR "File not found or not readable: ".$f."\n";
      print_error("File not found or not readable: ".$f);
      #formatprint("File not found or not readable: ".$f."\n","p","error");
    } else {
      parse($tex);
      my $filecount=next_subcount($tex);
      if (!$totalflag) {
        print_count($filecount);
        print "\n";
      }
      add_to_total($filetotalcount,$filecount);
    }
  }
  if (!$totalflag && get_count($filetotalcount,0)>1) {
    if ($htmlstyle) {formatprint("Sum of files: ".$file."\n",'h2');}
    print_count($filetotalcount,'sumcount');
    print "\n";
  }
  if ($htmlstyle && ($verbose || !$totalflag)) {print "</div>\n\n";}
  return $filetotalcount;
}


######
###### Subroutines
######

###### CMD specific implementations


# Add file to list of files scheduled for parsing
sub include_file {
  my ($tex,$fname)=@_;
  my $fpath=$workdir.$fname;
  if ($includeTeX==2) {
    my $latexcode;
		$latexcode=read_file($fpath) || BLOCK {
      print_error("File ".$fpath." not found.");
      return;
    };
    flush_next($tex);
    line_return(0,$tex);
    prepend_code($tex,$latexcode."\n%--- End of included file ".$fname."\n");
  } else {
    push @filelist,$fpath;
  }
}

# Print count (total) if conditions are met
sub conditional_print_total {
  my $sumcount=shift @_;
  if ($totalflag || number_of_subcounts($sumcount)>1) {
    if ($totalflag && $briefsum && @sumweights) {
      print get_sum_count($sumcount),"\n";
    } else {
      if ($htmlstyle) {formatprint("Total word count",'h2');}
      print_count($sumcount,'sumcount');
    }
  }
}

# Set or unset use of ANSI colours
sub option_ansi_colours {
  my $flag=shift @_;
  $ENV{'ANSI_COLORS_DISABLED'} = $flag?undef:1;
}

# Print text using ANSI colours
sub ansiprint {
  my ($text,$colour)=@_;
	 print Term::ANSIColor::colored($text,$colour);
}

# Print with word wrapping
sub wprint {
  my $text=shift @_;
  my @lines=split(/\n/,$text);
  my $ind1=2;
  my $ind2=6;
  my $i;
  foreach my $line (@lines) {
    if ($line=~s/^@//) {
      $ind2=1+index($line,':');
      $ind1=1+index($line,'-');
      if ($ind1<1) {$ind1=$ind2;}
      #print "IND: ".$ind1.",".$ind2."\n";
		  next;
		}
    my $firstindent=0;
    if ($line=~s/^(\t|\s{2,})(\S)/$2/) {$firstindent=$ind1;}
    my $indent=$firstindent;
    if ($line=~/^(.*\S)(\t|\s{2,})/) {
      $indent=$ind2;
      if ($1 eq '\\') {$line=' ';}
			else {$line=$1."   ";}
      $i=$indent-$firstindent-length($line);
      if ($i>0) {$line.=' ' x $i;}
      $line.=$';
    }
    print wrap(' ' x $firstindent,' ' x $indent,$line)."\n";
  }
}


###### Option handling


# Apply options to set values
sub Apply_Options {
  %STYLE=%{$STYLES[$verbose]};
  if ($utf8flag) {
	  binmode STDIN,':utf8';
	  binmode STDOUT,':utf8';
	}
  if ($htmlstyle>1) {html_head();}
  @WordPatterns=map { s/\@/$LetterPattern/g ; qr/$_/ } @WordPatterns;
  $WordPatternsJoined=join '|',@WordPatterns;
}

# Process package inclusion
sub include_package {
  my $incpackage=shift @_;
  #print "INCLUDE PACKAGE: ".$incpackage."\n";
  my $sub;
  _add_to_hash_if_exists(\%TeXmacro,\%PackageTeXmacro,$incpackage);
  _add_to_hash_if_exists(\%TeXheader,\%PackageTeXheader,$incpackage);
  _add_to_hash_if_exists(\%TeXgroup,\%PackageTeXgroup,$incpackage);
}

# Add package rules if defined
sub _add_to_hash_if_exists {
  my ($target,$source,$name)=@_;
  my $sub;
  if ($sub=$source->{$name}) {
    while (my ($key,$val)=each(%$sub)) {
      $target->{$key}=$val;
    }
  }
}

# Process TC instruction
sub tc_macro_param_option {
  my ($instr,$macro,$param,$option)=@_;
  if ($param=~/^\[([0-9,]+)\]$/) {$param=[split(',',$1)];}
  if (!defined $option) {$option=1;}
  if (($instr eq 'macro') || ($instr eq 'exclude')) {$TeXmacro{$macro}=$param;}
  elsif ($instr eq 'header') {$TeXheader{$macro}=$param;$TeXmacro{$macro}=$param;}
  elsif ($instr eq 'macroword') {$TeXmacroword{$macro}=$param;}
  elsif ($instr eq 'preambleinclude') {$TeXpreamble{$macro}=$param;}
  elsif ($instr eq 'group') {
    $TeXmacro{'begin'.$macro}=$param;
    $TeXgroup{$macro}=$option;
  }
  elsif ($instr eq 'floatinclude') {$TeXfloatinc{$macro}=$param;}
  elsif ($instr eq 'fileinclude') {$TeXfileinclude{$macro}=$param;}
  elsif ($instr eq 'breakmacro') {$BreakPoints{$macro}=$param;}
  else {return 0;}
  return 1;
}


###### TeX code handle


# Read LaTeX file into TeX object
sub TeXfile {
  my $filename=shift @_;
  if ($filename eq $_STDIN_) {
    if ($verbose>0) {
      formatprint("File from STDIN\n",'h2');
      $blankline=0;
    }
	  return TeXcode(_read_stdin(),'STDIN');
	} else {
    my $file=read_file($filename) || return undef;
    if ($verbose>0) {
      formatprint("File: ".$filename."\n",'h2');
      $blankline=0;
    }
    return TeXcode($file,$filename);
  }
}

# Read file to string handling encoding
sub read_file {
  my $filename=shift @_;
  if ($utf8flag) {
    open(FH,"<:utf8",$filename) || return undef;
  } else {
    open(FH,"<".$filename) || return undef;
  }
  my @text=<FH>;
  close(FH);
  my $latexcode=join('',@text);
  if ($utf8flag) {$latexcode =~ s/^\x{feff}//;}
  return $latexcode;
}

# Read file from STDIN
sub _read_stdin {
  my @text=<STDIN>;
  my $latexcode=join('',@text);
  if ($utf8flag) {
    $latexcode =~ s/^\x{feff}//;
  }
  return $latexcode;
}

###### Parsing routines


# Parse LaTeX document
sub parse {
  my ($tex)=@_;
  if ($htmlstyle && $verbose) {print "<div class='parse'><p>\n";}
  while (!($tex->{'eof'})) {
    _parse_unit($tex,1);
  }
  if ($htmlstyle && $verbose) {print "</p></div>\n";}
}

# Parse one block or unit
sub _parse_unit {
  # Status:
  #    0 = exclude from count
  #    1 = text
  #    2 = header text
  #    3 = float text
  #   -1 = float (exclude)
  #   -2 = strong exclude, ignore begin-end groups
  #   -3 = stronger exclude, do not parse macro parameters
  #   -9 = preamble (between \documentclass and \begin{document})
  my ($tex,$status,$end)=@_;
  if (!defined $status) {
    print_error("CRITICAL ERROR: Undefined parser status!");
    exit;
  } elsif (ref($status) eq 'ARRAY') {
    print_error("CRITICAL ERROR: Invalid parser status!");
    exit;
  }
  my $substat;
  if ($showstates) {
    if (defined $end) {
      $tex->{'printstate'}=':'.$status.':'.$end.':';
    } else {
      $tex->{'printstate'}=':'.$status.':';
    }
    flush_next($tex);
  }
  while (defined (my $next=_next_token($tex))) {
    # parse next token; or tokens until match with $end
    set_style($tex,"ignore");
    if ((defined $end) && ($end eq $next)) {
      # end of unit
      return;
    } elsif (!defined $next) {
      print_error("ERROR: End of file while waiting for ".$end);
      return;
    }
    if ($status==-9 && $next eq '\begin' && $tex->{'line'}=~/^\{\s*document\s*\}/) {
      # \begin{document}
      $status=1;
    }
    if ($tex->{'type'}==-1) {
      # space or other code that should be passed through without styling
      flush_next($tex,' ');
    } elsif ($next eq '\documentclass') {
      # starts preamble
      set_style($tex,'document');
      __gobble_option($tex);
      __gobble_macro_parms($tex,1);
      while (!($tex->{'eof'})) {
       _parse_unit($tex,-9);
      }
    } elsif ($tex->{'type'}==666) {
      # parse TC instructions
      _parse_tc($tex);
    } elsif ($tex->{'type'}==1) {
      # word
      if ($status>0) {
        _process_word($next,$status);
        inc_count($tex,$status);
        set_style($tex,'word'.$status);
      }
    } elsif ($next eq '{') {
      # {...}
      _parse_unit($tex,$status,'}');
    } elsif ($tex->{'type'}==3 && $status==-3) {
      set_style($tex,'ignore');
    } elsif ($tex->{'type'}==3) {
      # macro call
      _parse_macro($tex,$next,$status,$substat);
    } elsif ($next eq '$') {
      # math inline
      _parse_math($tex,$status,6,'$');
    } elsif ($next eq '$$') {
      # math display (unless already in inlined math)
      if (!(defined $end && $end eq '$')) {
        _parse_math($tex,$status,7,'$$');
      }
    }
    if (!defined $end) {return;}
  }
}

# Process word with a given status (>0, i.e. counted)
sub _process_word {
  my ($word,$status)=@_;
  $WordFreq{lc $word}++; # ... but don't really want to use lc ...
}

# Parse unit when next token is a macro
sub _parse_macro {
  my ($tex,$next,$status,$substat)=@_;
  if (my $label=$BreakPoints{$next}) {
    if ($tex->{'line'}=~ /^[*]?(\s*\[.*?\])*\s*\{((.|\{.*\})*)\}/ ) {
      $label=$label.': '.$2;
    }
    next_subcount($tex,$label);
  }
  set_style($tex,$status>0?'command':'exclcommand');
  if ($next eq '\begin' && $status!=-2) {
 	  _parse_begin_end($tex,$status);
  } elsif (($status>0 || $status==-9) && defined ($substat=$TeXpackageinc{$next})) {
    _parse_include_package($tex,$substat);
  } elsif (($status==-1) && ($substat=$TeXfloatinc{$next})) {
    # text included from float
    set_style($tex,'command');
    __gobble_macro_parms($tex,$substat);
  } elsif ($status==-9 && defined ($substat=$TeXpreamble{$next})) {
  	# parse preamble include macros
    set_style($tex,'command');
  	if (defined $TeXheader{$next}) {inc_count($tex,4);}
    __gobble_macro_parms($tex,$substat,1);
  } elsif ($status<0) {
  	# ignore
    __gobble_option($tex);
  } elsif ($next eq '\(') {
    # math inline
    _parse_math($tex,$status,6,'\)');
  } elsif ($next eq '\[') {
    # math display
    _parse_math($tex,$status,7,'\]');
  } elsif ($next eq '\def') {
    # ignore \def...
    $tex->{'line'} =~ s/^([^\{]*)\{/\{/;
    flush_next($tex);
    print_style($1,'ignore');
    _parse_unit($tex,-2);
  } elsif (defined (my $addsuffix=$TeXfileinclude{$next})) {
  	# include file: queue up for parsing
    _parse_include_file($tex,$status,$addsuffix);
  } elsif (defined ($substat=$TeXmacro{$next})) {
    # macro: exclude options
  	if (defined $TeXheader{$next}) {inc_count($tex,4);}
    __gobble_macro_parms($tex,$substat,$status);
  } elsif (defined ($substat=$TeXmacroword{$next})) {
  	# count macro as word (or a given number of words)
    if ($status>0) {inc_count($tex,$status,$substat);}
    set_style($tex,'word'.$status);
  } elsif ($next =~ /^\\[^\w\_]/) {
  } else {
    __gobble_option($tex);
  }
}

# Parse TC instruction
sub _parse_tc {
  my ($tex)=@_;
  my $next=$tex->{'next'};
  set_style($tex,'tc');
  flush_next($tex);
  $next=~s/^\%+TC:\s*(\w+)\s*//i || BLOCK {
    print_error('Warning: TC command should have format %TC:instruction [macro] [parameters]');
    return;
  };
  my $instr=$1;
  $instr=~tr/[A-Z]/[a-z]/;
  if ($instr=~/^(break)$/) {
    if ($instr eq 'break') {next_subcount($tex,$next);}
  } elsif ($instr eq 'ignore') {
    __gobble_tc_ignore($tex);
  } elsif ($instr eq 'newtemplate') {$outputtemplate="";
  } elsif ($instr eq 'template') {$outputtemplate.=$next;
  } elsif ($instr eq 'subst') {
    if ($next=~/^(\\\S+)\s+/) {
      my $from=$1;
      my $to=$';
      $tex->{'line'}=~s/(\w)\Q$from\E\b\s*/$1 $to/g;
      $tex->{'line'}=~s/\Q$from\E\b\s*/$to/g;
    }
  } elsif ($next=~/^([\\]*\S+)\s+([^\s\n]+)(\s+(-?[0-9]+))?/) {
    # Format = TC:word macro
    my $macro=$1;
    my $param=$2;
    my $option=$4;
    if (tc_macro_param_option($instr,$macro,$param,$option)) {}
    else {print_error("Warning: Unknown TC command: ".$instr);}
  } else {
    print_error("Warning: Invalid TC command format: ".$instr);
  }
}

# Parse through ignored LaTeX code
sub __gobble_tc_ignore {
  my ($tex)=@_;
  set_style($tex,'ignore');
  _parse_unit($tex,-3,"%TC:endignore");
  set_style($tex,'tc');
  flush_next($tex);
}

# Parse math formulae
sub _parse_math {
  my ($tex,$status,$substat,$end)=@_;
  my $localstyle=$status>0 ? 'mathgroup' : 'exclmath';
  if ($status>0) {inc_count($tex,$substat);}
  set_style($tex,$localstyle);
  _parse_unit($tex,0,$end);
  set_style($tex,$localstyle);
}

# Parse begin-end group
sub _parse_begin_end {
  my ($tex,$status)=@_;
  my $localstyle=$status>0 ? 'grouping' : 'exclgroup';
  flush_next_gobble_space($tex,$localstyle,$status);
  #__gobble_option($tex); # no option before group name
  my $groupname;
  if ($tex->{'line'} =~ s/^\{([^\{\}\s]+)\}[ \t\r\f]*//) {
    # gobble group type
    $groupname=$1;
    print_style('{'.$1.'}',$localstyle);
    my $next='begin'.$groupname;
    if (defined (my $substat=$TeXmacro{$next})) {
      __gobble_macro_parms($tex,$substat);
    }
  } else {
    print_error("ERROR: BEGIN group without type.");
  }
  # find group status (or leave unchanged)
  my $substat;
  #defined ($substat=$TeXgroup{$1}) || ($substat=$status);
  $substat=$TeXgroup{$1};
  if (!defined $substat) {
    $substat=$status;
    if ($strictness>=1) {
      set_warning("Using default rule for group ".$groupname);
      #print_error("WARNING: Using default rule for group ".$1);
    }
  }
  if ($status<=0 && $status<$substat) {
    # Do not raise status
		  $substat=$status;
		} elsif ($status>0 && ($substat==-1)) {
    # Count float
    inc_count($tex,5);
  } elsif ($status>0 and $substat>3) {
    # Count item, exclude contents
    inc_count($tex,$substat);
    $substat=0;
  }
  _parse_unit($tex,$substat,'\end');
  flush_next_gobble_space($tex,$localstyle,$status);
  if ($tex->{'line'} =~ s/^\{([^\{\}\s]+)\}[ \t\r\f]*//) {
    # gobble group type
    print_style('{'.$1.'}',$localstyle);
    if ($groupname ne $1) {
      print_error("Warning: begin\{".$groupname."\} ended with end\{".$1."\}.");
    }
  } else {
    print_error("Warning: END group without type while waiting to end ".$groupname.".");
  }
}

# Parse and process file inclusion
sub _parse_include_file {
  my ($tex,$status,$addsuffix)=@_;
  $tex->{'line'} =~ s/^\{([^\{\}\s]+)\}//
	 || $tex->{'line'} =~ s/^\s*([^\{\}\%\\\s]+)//
	 || $tex->{'line'} =~ s/^\s*\{(.+?)\}//
	 || BLOCK {
	  print_error("WARNING: Failed to read or interpret file name for inclusion.");
	  return;
	};
  if ($status>0) {
    print_style($&,'fileinclude');
    my $fname=$1;
    if ($addsuffix==2) {$fname.='.tex';}
    elsif ($addsuffix==1 && ($fname=~/^[^\.]+$/)) {$fname.='.tex';}
    if ($includeTeX) {include_file($tex,$fname);}
  } else {
    print_style($&,'ignored');
  }
}

# Parse and process package inclusion
sub _parse_include_package {
  my ($tex)=@_;
  set_style($tex,'document');
  __gobble_option($tex);
  if ( $tex->{'line'}=~s/^\{(\w+)\}// ) {
    print_style('{'.$1.'}','document');
    include_package($1);
  } else {
    _parse_unit($tex,__new_status(0,1));
  }
  __gobble_options($tex);
}

# Gobble next option, return option or undef if none
sub __gobble_option {
  my $tex=shift @_;
  flush_next_gobble_space($tex);
  foreach my $pattern (@MacroOptionPatterns) {
    if ($tex->{'line'}=~s/^($pattern)//) {
      print_style($1,'option');
      return $1;
    }
  }
  return undef;
}

# Gobble all options
sub __gobble_options {
  while (__gobble_option(@_)) {}
}

# Gobble macro modifyer (e.g. following *)
sub __gobble_macro_modifier {
  my $tex=shift @_;
  flush_next($tex);
  if ($tex->{'line'} =~ s/^\*//) {
    print_style($1,'option');
    return $1;
  }
  return undef;
}

# Gobble macro parameters as specified in parm plus options
sub __gobble_macro_parms {
  my ($tex,$parm,$oldstat)=@_;
  my $i;
  if (ref($parm) eq 'ARRAY') {
    $i=scalar @{$parm};
  } else {
    $i=$parm;
    $parm=[0,0,0,0,0,0,0,0,0];
  }
  if ($i>0) {__gobble_macro_modifier($tex);}
  __gobble_options($tex);
  for (my $j=0;$j<$i;$j++) {
    _parse_unit($tex,__new_status($parm->[$j],$oldstat));
    __gobble_options($tex);
  }
}

# Return new parsing status given old and substatus
sub __new_status {
  my ($substat,$old)=@_;
  if (!defined $old) {return $substat;}
  if ($old==-3 || $substat==-3) {return -3;}
  if ($old==-2 || $substat==-2) {return -2;}
  if ($old==0 || $substat==0) {return 0;}
  if ($old==-9 || $substat==-9) {return -9;}
  if ($old>$substat) {return $old;}
  return $substat;
}

# Get next token skipping comments and flushing output buffer
sub _next_token {
  my $tex=shift @_;
  my ($next,$type);
  my $style=$tex->{'style'};
  if (defined $tex->{'next'}) {print_style($tex->{'next'},$tex->{'style'});}
  $tex->{'style'}=undef;
  while (defined ($next=__get_next_token($tex))) {
    $type=$tex->{'type'};
    if ($type==0) {
      print_style($next,'comment');
    } elsif ($type==9) {
      if ($verbose>0) {line_return(-1,$tex);}
    } else {
      return $next;
    }
  }
  return $next;
}

# Read, interpret and return next token
sub __get_next_token {
  # Token (or token group) category:
  #   -1: space
  #   0: comment
  #   1: word (or other forms of text or text components)
  #   2: symbol (not word, e.g. punctuation)
  #   3: macro
  #   4: curly braces {}
  #   5: brackets []
  #   6: maths
  #   9: line break in file
  #   999: end of line or blank line
  #   666: TeXcount instruction (%TC:instruction)
  my $tex=shift @_;
  my $next;
  my $ch;
  while (!$tex->{'eof'}) {
    $ch=substr($tex->{'line'},0,1);
    if ($ch eq '') {
      if (!more_texcode($tex)) {$tex->{'eof'}=1;}
      next;
    } elsif ($ch=~/^[ \t\f]/) {
      $tex->{'line'}=~s/^([ \t\f]+)//;
      return __set_token($tex,$1,-1);
    } elsif ($ch eq "\n" || $ch eq "\r") {
      $tex->{'line'}=~s/^(\r\n?|\n)//;
      return __set_token($tex,$1,9);
    } elsif ($ch eq '$') {
      $tex->{'line'}=~s/^(\$\$?)//;
      return __set_token($tex,$1,6);
    } elsif ($ch eq '{' || $ch eq '}') {
      return __get_chtoken($tex,$ch,4);
    } elsif ($ch eq '[' || $ch eq ']') {
      return __get_chtoken($tex,$ch,5);
    } elsif ($ch=~/^['"`:.,()[]!+-*=\/^_@<>~#&]$/) {
      return __get_chtoken($tex,$ch,2);
    } elsif ($ch eq '%') {
      if ($tex->{'line'}=~s/^(\%+TC:\s*endignore\b[^\r\n]*)//i) {
        __set_token($tex,$1,666);
        return "%TC:endignore";
      }
      if ($tex->{'line'}=~s/^(\%+[tT][cC]:[^\r\n]*)//) {return __set_token($tex,$1,666);}
      if ($tex->{'line'}=~s/^(\%+[^\r\n]*)//) {return __set_token($tex,$1,0);}
      return __get_chtoken($tex,$ch,0);
    } elsif ($tex->{'line'}=~s/^($WordPatternsJoined)//) {
      return __set_token($tex,$1,1);
    } elsif ($ch eq '\\') {
      if ($tex->{'line'}=~s/^(\\[{}%])//) {return __set_token($tex,$1,2);}
      if ($tex->{'line'}=~s/^(\\([a-zA-Z_]+|[^a-zA-Z_]))//) {return __set_token($tex,$1,3);}
      return __get_chtoken($tex,$ch,999);
    } else {
      return __get_chtoken($tex,$ch,999);
    }
  }
  return undef;
}

# Set next token and its type
sub __set_token {
  my ($tex,$next,$type,$delete)=@_;
  $tex->{'next'}=$next;
  $tex->{'type'}=$type;
  return $next;
}

# Set character token and remove from line
sub __get_chtoken {
  my ($tex,$ch,$type)=@_;
  $tex->{'line'}=substr($tex->{'line'},1);
  $tex->{'next'}=$ch;
  $tex->{'type'}=$type;
  return $ch;
}


###### Count handling routines


## Make TeX handle for LaTeX code: the main TeXcount object
# The "TeX object" is a data containser: a hash containing
#  - filename: name of LaTeX file being parsed
#  - filepath: path to LaTeX file
#  - countsum: count object with total count (incl. subcounts)
#  - subcount: count object for subcount (to be added to countsum)
#  - subcounts: list of subcounts
# plus following elements used for the processing the LaTeX code 
#  - line: the LaTeX paragraph being processed
#  - texcode: what remains of LaTeX code to process (after line)
#  - texlength: length of LaTeX code
#  - next: the next token, i.e. the one being processed
#  - type: the type of the next token
#  - style: the present output style (for verbose output)
#  - printstate: present parsing state (for verbose output only)
#  - eof: set once the end of the input is reached
# which are passed to methods by passing the TeX object. It is used when parsing
# the LaTeX code, and discarded once the parsing is done. During parsing, counts
# are added to the subcount element; whenever a break point is encountered, the
# subcount is added to the countsum (next_subcount) and a new subcount object
# prepared. Note that this requires that the last subcount object be added to
# the countsum once the end of the document is reached.
sub TeXcode {
  my ($texcode,$filename,$title)=@_;
  my %TeX=();
  $TeX{'filename'}=$filename;
  if (!defined $filename) {
    $TeX{'filepath'}='';
  } elsif ($filename=~/^(.*[\\\/])[^\\\/]+$/) {
    $TeX{'filepath'}=$1;
  } else {
    $TeX{'filepath'}='';
  }
  if (defined $title) {}
  elsif (defined $filename) {$title="File: ".$filename;}
  else {$title="Word count";}
  foreach my $key (keys %substitutions) {
    my $value=$substitutions{$key};
    $texcode=~s/(\w)\Q$key\E/$1 $value/g;
    $texcode=~s/\Q$key\E/$value/g;
  }
  $TeX{'line'}='';
  $TeX{'texcode'}=$texcode;
  $TeX{'texlength'}=length($texcode);
  if ($texcode =~ /([[:^ascii:]])/) {set_warning("Code contains non-ASCII characters.");}
  more_texcode(\%TeX);
  $TeX{'next'}=undef;
  $TeX{'type'}=undef;
  $TeX{'style'}=undef;
  $TeX{'printstate'}=undef;
  $TeX{'eof'}=0;
  my $countsum=new_count($title);
  $TeX{'countsum'}=$countsum;
  my $count=new_count("_top_");
  $TeX{'subcount'}=$count;
  inc_count(\%TeX,0);
  my @countlist=();
  $countsum->{'subcounts'}=\@countlist;
  return \%TeX;
}

## Get more TeX code from texcode buffer if possible, return 1 if done
sub more_texcode {
  my ($tex)=@_;
  if (!defined $tex->{'texcode'}) {return 0;}
  if ( $optionFast && $tex->{'texcode'} =~ s/^.*?(\r{2,}|\n{2,}|(\r\n){2,})//s ) {
    $tex->{'line'}.=$&;
    return 1;
  }
  $tex->{'line'}.=$tex->{'texcode'};
  $tex->{'texcode'}=undef;
  return 1;
}

## Prepend LaTeX code to TeXcode object
sub prepend_code {
  my ($tex,$latexcode)=@_;
  $tex->{'line'}=$latexcode.$tex->{'line'};
}

## Returns size of TeX code in bytes
sub get_texsize {
  my $tex=shift @_;
  return $tex->{'texlength'}
}

## Make new count object
# The "count object" is a hash containing
#  - title: the title of the count (name of file, section, ...)
#  - counts: a list of numbers (the counts: files, text words, ...)
# upon creation, but where the element
#  - subcounts: list of count objects (added by the TeX object)
# may exist if the count contains subcounts. The elements of the
# count are (by their index):
#  0 = #files: counts the number of files
#  1 = text words: counts the number of words in the text
#  2 = header words: number of words in headers
#  3 = caption words: number of words in float captions
#  4 = #headers: number of headers
#  5 = #floats: number of tables, figures, floats, etc.
#  6 = #inline formulae: number of math elements in text ($...$)
#  7 = #displayed formulae: number of displayed equations
sub new_count {
  my ($title)=@_;
  my @cnt=(0,0,0,0,0,0,0,0);
  my %count=('counts'=>\@cnt,'title'=>$title);
  # files, text words, header words, float words,
  # headers, floats, math-inline, math-display;
  return \%count;
}

# Increment TeX count for a given count type
sub inc_count {
  my ($tex,$type,$value)=@_;
  my $count=$tex->{'subcount'};
  if (!defined $value) {$value=1;}
  ${$count->{'counts'}}[$type]+=$value;
}

# Get count value for a given count type
sub get_count {
  my ($count,$type)=@_;
  return ${$count->{'counts'}}[$type];
}

# Compute sum count for a count object
sub get_sum_count {
  my $count=shift @_;
  my $sum=0;
  for (my $i=scalar(@sumweights);$i-->0;) {
    $sum+=get_count($count,$i+1)*$sumweights[$i];
  }
  return $sum;
}

# Returns the number of subcounts
sub number_of_subcounts {
  my $count=shift @_;
  if (my $subcounts=$count->{'subcounts'}) {
    return scalar(@{$subcounts});
  } else {
    return 0;
  }
}

# Is a null count? (counts 1-7 zero, title starts with _)
sub _count_is_null {
  my $count=shift @_;
  if (!$count->{'title'}=~/^_/) {return 0;}
  for (my $i=1;$i<8;$i++) {
    if (get_count($count,$i)>0) {return 0;}
  }
  return 1;
}

# Add one count to another
sub _add_to_count {
  my ($a,$b)=@_;
  for (my $i=0;$i<8;$i++) {
   ${$a->{'counts'}}[$i]+=${$b->{'counts'}}[$i];
  }
}

# Add subcount to sum count and prepare new subcount
sub next_subcount {
  my ($tex,$title)=@_;
  add_to_total($tex->{'countsum'},$tex->{'subcount'});
  $tex->{'subcount'}=new_count($title);
  return $tex->{'countsum'};
}

# Add count to total as subcount
sub add_to_total {
  my ($total,$count)=@_;
  _add_to_count($total,$count);
  if (!_count_is_null($count)) {
    push @{$total->{'subcounts'}},$count;
  }
}

###### Result output routines


# Close the output, e.g. adding HTML tail
sub Close_Output {
  if ($htmlstyle>1) {
    html_tail();
  }
}

# Add warning to list of registered warnings
sub set_warning {
  my $text=shift @_;
  $warnings{$text}++;
}

# Report if there were any errors occurring during parsing
sub Report_Errors {
  if (defined $outputtemplate) {return;}
  if ( !$briefsum && !$totalflag && $verbose>=0 ) {
    foreach (keys %warnings) {formatprint($_,"p","nb");print "\n";}
  }
  if ($errorcount==0) {return;}
  if ($briefsum && $totalflag) {print " ";}
  if ($htmlstyle) {
    print "<div class='error'><p>\n";
    print "There were ".$errorcount." error(s) reported!.\n";
    print "</p></div>\n";
  } elsif ($briefsum && $totalflag) {
    print "(errors:".$errorcount.")";
  } else {
    print "(errors:".$errorcount.")\n";
  }
}

# Print word frequencies (as text only)
sub print_word_freq {
  if ($htmlstyle) {print "<table class='wordfreq'>\n<tr><th>Word</th><th>Freq</th></tr>\n";}
  for my $word (sort {$WordFreq{$b} <=> $WordFreq{$a}} keys %WordFreq) {
    my $freq=$WordFreq{$word};
    if ($freq<$optionWordFreq) {
    } elsif ($htmlstyle) {
      print "<tr><td>",$word,"</td><td>",$freq,"</td></tr>\n";
    } else {
      print $word,": ",$freq,"\n";
    }
  }
  if ($htmlstyle) {print "</table>\n";}
}


###### Printing routines


# Print text using given style/colour
sub print_with_style {
  my ($text,$style,$colour)=@_;
  if ($style eq ' ') {
    if ($htmlstyle && $text=~/^[ \t]+$/ ) {$text=~s/[ \t]{2}/\&nbsp; /g;}
    print $text;
  } elsif ($style eq '') {
    print $text;
    print_error("Shouldn't have empty style in print_with_style!");
  } elsif ($htmlstyle) {
    print "<span class='".$style."'>".$text."</span>";
  } else {
    ansiprint($text,$colour);
  }
}

# Print text, using appropriate tags for HTML
sub formatprint {
  my ($text,$tag,$class)=@_;
  my $break=($text=~s/(\r\n?|\n)$//);
  if ($htmlstyle && defined $tag) {
    print '<'.$tag;
    if ($class) {print " class='".$class."'";}
    print '>'.$text.'</'.$tag.'>';
  } else {
    print $text;
  }
  if ($break) {print "\n";}
}

# Add a line break to output
sub linebreak {
  if ($htmlstyle) {print "<br>\n";} else {print "\n";}
}

###### Routines for printing count summary


# Print count summary for a count object
sub print_count {
  my ($count,$class)=@_;
  if ($htmlstyle) {print "<div class='".($class||'count')."'>\n";}  
  if ($outputtemplate) {
    _print_count_template($count,$outputtemplate);
  } elsif ($briefsum && @sumweights) {
    _print_sum_count($count);
  } elsif ($briefsum) {
    if ($htmlstyle) {print "<p class='count'>";}
    _print_count_brief($count);
    if ($htmlstyle) {print "</p>\n";}
  } else {
    _print_count_details($count);
  }
  if ($htmlstyle) {print "</div>\n";}  
}

# Return count,header,... list filling in header if missing
sub __count_and_header {
  my $count=shift @_;
  my $header=__count_header($count);
  return $count,$header,@_;
}

# Return count title or "" if missing
sub __count_header {
  my $count=shift @_;
  return $count->{'title'}||'';
}

# Print total count (sum) for a given count object
sub _print_sum_count {
  my ($count,$header)=__count_and_header(@_);
  if ($htmlstyle) {print "<p class='count'>".$header.": ";}
  print get_sum_count($count);
  if ($htmlstyle) {print "</p>\n";}
  else {print ": ".$header;}
  print "\n";
}

# Print brief summary of count object
sub _print_count_brief {
  my ($count,$header,$tag1,$tag2)=__count_and_header(@_);
  my @cnt=@{$count->{'counts'}};
  if ($htmlstyle && $tag1) {print "<".$tag1.">";}
  print $cnt[1]."+".$cnt[2]."+".$cnt[3].
      " (".$cnt[4]."/".$cnt[5]."/".$cnt[6]."/".$cnt[7].")";
  if ($htmlstyle && $tag2) {
    print "</".$tag1."><".$tag2.">";
    $tag1=$tag2;
  } else {print " ";}
  print $header;
  if ($htmlstyle && $tag1) {print "</".$tag1.">";}
  print "\n";
}

# Print detailed summary of count object
sub _print_count_details {
  my ($count,$header)=__count_and_header(@_);
  if ($htmlstyle) {print "<ul class='count'>\n";}
  if ($header) {formatprint($header."\n",'li','header');}
  if (@sumweights) {formatprint('Sum count: '.get_sum_count($count)."\n",'li');}
  for (my $i=1;$i<8;$i++) {
    formatprint($countlabel[$i].': '.get_count($count,$i)."\n",'li');
  }
  if (get_count($count,0)>1) {
    formatprint($countlabel[0].': '.get_count($count,0)."\n",'li');
  }
  my $subcounts=$count->{'subcounts'};
  if ($showsubcounts && defined $subcounts && scalar(@{$subcounts})>=$showsubcounts) {
    formatprint("Subcounts:\n",'li');
    if ($htmlstyle) {print "<span class='subcount'>\n";}
    formatprint("  text+headers+captions (#headers/#floats/#inlines/#displayed)\n",'li','fielddesc');
    foreach my $subcount (@{$subcounts}) {
      print '  ';
      _print_count_brief($subcount,'li');
    }
    if ($htmlstyle) {print "</span>\n";}
  }
  if ($htmlstyle) {print "</ul>\n";}
}

# Print summary of count object using template
sub _print_count_template {
  my ($count,$header,$template)=__count_and_header(@_);
  $template=~s/\\n/\n/g;
  if ($htmlstyle) {$template=~s/\n/<br>/g;}
  my ($subtemplate,$posttemplate);
  while ($template=~/\{SUB\?((.*?)\|)?(.*?)(\|(.*?))?\?SUB\}/is) {
    __print_count_using_template($count,$`);
    if (number_of_subcounts($count)>1) {
      if (defined $2) {print $2;}
      __print_subcounts_using_template($count,$3);
      if (defined $5) {print $5;}
    }
    $template=$';
  }
  __print_count_using_template($count,$template);
}

# Print counts using template
sub __print_count_using_template {
  my ($count,$template)=@_;
  for (my $i=0;$i<8;$i++) {
    $template=__process_template($template,$i,get_count($count,$i));
  }
  $template=~s/\{VER\}/$versionnumber/gi;
  $template=__process_template($template,"W|WARN|WARNING|WARNINGS",length(%warnings));
  $template=__process_template($template,"E|ERR|ERROR|ERRORS",$errorcount);
  $template=__process_template($template,"S|SUM",get_sum_count($count));
  $template=__process_template($template,"T|TITLE",$count->{'title'}||"");
  $template=__process_template($template,"SUB",number_of_subcounts($count));
  print $template;
}

# Print subcounts using template
sub __print_subcounts_using_template {
  my ($count,$template)=@_;
  my $subcounts=$count->{'subcounts'};
  if ($template && defined $subcounts && scalar(@{$subcounts})>=$showsubcounts) {
    foreach my $subcount (@{$subcounts}) {
      __print_count_using_template($subcount,$template);
    }
  }
}

# Process template for specific label
sub __process_template {
  my ($template,$label,$value)=@_;
  if ($value) {
    $template=~s/\{($label)\?(.*?)(\|(.*?))?\?(\1)\}/$2/gis;
  } else {
    $template=~s/\{($label)\?(.*?)\|(.*?)\?(\1)\}/$3/gis;
    $template=~s/\{($label)\?(.*?)\?(\1)\}//gis;
  }
  if (!defined $value) {$value="";}
  $template=~s/\{($label)\}/$value/gis;
  return $template;
}


###### Routines for printing parsing details


# Print next token
sub flush_next {
  my ($tex,$style)=@_;
  my $ret=undef;
  if (defined $style && $style ne '') {
	  set_style($tex,$style);
 	}
  if (defined $tex->{'next'}) {
    $ret=print_style($tex->{'next'},$tex->{'style'},$tex->{'printstate'});
  }
  $tex->{'printstate'}=undef;
  $tex->{'style'}='-';
  return $ret;
}

# Print next token and gobble following spaces
sub flush_next_gobble_space {
  my ($tex,$style,$status)=@_;
  my $ret=flush_next($tex,$style);
  if (!defined $ret) {$ret=0;}
  if (!defined $status) {$status=0;}
  my $prt=($verbose>0);
  if ($tex->{'line'}=~s/^([ \t\f]*)(\r\n?|\n)([ \t\f]*)//) {
    if (!$prt) {
    } elsif ($verbose>2 || $ret) {
      print $1;
      line_return(-1,$tex);
      my $space=$3;
      if ($htmlstyle) {$space=~s/  /\&nbsp;/g;}
      print $space;
    } else {
      line_return(0,$tex);
    }
  } elsif ($tex->{'line'}=~s/^([ \t\f]*)//) {
    if ($prt) {print $1;}
  }
}

# Set presentation style
sub set_style {
  my ($tex,$style)=@_;
  if (!(($tex->{'style'}) && ($tex->{'style'} eq '-')) && $style ne '') {$tex->{'style'}=$style;}
}

# Print text using the given style, and log state if given
sub print_style {
  my ($text,$style,$state)=@_;
  (($verbose>=0) && (defined $text) && (defined $style)) || return 0;
  my $colour;
  ($colour=$STYLE{$style}) || return;
  if (($colour) && !($colour eq '-')) {
    print_with_style($text,$style,$colour);
    if ($state) {print_style($state,'state');}
    if ($style ne "cumsum") {$blankline=-1;}
    return 1;
  } else {
    return 0;
  }
}

# Conditional line return
sub line_return {
  my ($blank,$tex)=@_;
  if ($blank<0 && $verbose<3) {$blank=1;}
  if ($blank<0 || $blank>$blankline) {
    if ((defined $tex) && @sumweights) {
      my $num=get_sum_count($tex->{'subcount'});
      print_style(" [".$num."]","cumsum");
    }
    linebreak();
    $blankline++;
  }
}

# Register error and print error message
sub print_error {
  my $text=shift @_;
  $errorcount++;
  if ($verbose>=0) {
    line_return(1);
    print_style("!!!  ".$text."  !!!",'error');
    line_return(1);
  }
}


###### Print help on style/colour codes


# Print output style codes if conditions are met
sub conditional_print_help_style {
  if ($showcodes) {_print_help_style();}
  return $showcodes;
}

# Print help on output styles
sub _print_help_style {
  if ($verbose<=0) {return;}
  if ($htmlstyle) {print "<div class='stylehelp'><p>";}
  formatprint("Format/colour codes of verbose output:","h2");
  print "\n\n";
  _help_style_line('Text which is counted',"word1","counted as text words");
  _help_style_line('Header and title text',"word2","counted as header words");
  _help_style_line('Caption text and footnotes',"word3","counted as caption words");
  _help_style_line("Ignored text or code","ignore","excluded or ignored");
  _help_style_line('\documentclass',"document","document start, beginning of preamble");
  _help_style_line('\macro',"command","macro not counted, but parameters may be");
  _help_style_line('\macro',"exclcommand","macro in excluded region");
  _help_style_line("[Macro options]","option","not counted");
  _help_style_line('\begin{group}  \end{group}',"grouping","begin/end group");
  _help_style_line('\begin{group}  \end{group}',"exclgroup","begin/end group in excluded region");
  _help_style_line('$  $',"mathgroup","counted as one equation");
  _help_style_line('$  $',"exclmath","equation in excluded region");
  _help_style_line('% Comments',"comment","not counted");
  _help_style_line('%TC:TeXcount instructions',"tc","not counted");
  _help_style_line("File to include","fileinclude","not counted but file may be counted later");
  if ($showstates) {
    _help_style_line('[state]',"state","internal TeXcount state");
  }
  if (@sumweights) {
    _help_style_line('[cumsum]',"cumsum","cumulative sum count");
  }
  _help_style_line("ERROR","error","TeXcount error message");
  if ($htmlstyle) {print "</p></div>";}
  print "\n\n";
}

# Print one line of help
sub _help_style_line {
  my ($text,$style,$comment)=@_;
  if ($htmlstyle) {
    $comment="&nbsp;&nbsp;....&nbsp;&nbsp;".$comment;
  } else {
    $comment=" .... ".$comment;
  }
  if (print_style($text,$style)) {
    print $comment;
    linebreak();
  }
}


###### HTML routines


# Print HTML header
sub html_head {
  print "<html>\n<head>";
  if ($utf8flag) {
    print "\n<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">";
  }
  _print_html_style();
  print "</head>\n\n<body>\n\n<h1>LaTeX word count";
  if ($showVersion>0) {
		  print " (version ",_html_version(),")"}
		print "</h1>\n";
}

# Print HTML tail
sub html_tail {
  print '</body></html>';
}

# Return version number using HTML
sub _html_version {
  my $htmlver=$versionnumber;
  $htmlver=~s/\b(alpha|beta)\b/&$1;/g;
  return $htmlver;
}

# Print HTML STYLE element
sub _print_html_style {
print '
<style>
<!--
body {width:auto;padding:5;margin:5;}
.error {font-weight:bold;color:#f00;font-style:italic;}
.word1,.word2,.word3 {color: #009; border-left: 1px solid #CDF; border-bottom: 1px solid #CDF;}
.word2 {font-weight: 700;}
.word3 {font-style: italic;}
.command {color: #c00;}
.exclcommand {color: #f99;}
.option {color: #cc0;}
.grouping, .document {color: #900; font-weight:bold;}
.mathgroup {color: #090;}
.exclmath {color: #6c6;}
.ignore {color: #999;}
.exclgroup {color:#c66;}
.tc {color: #999; font-weight:bold;}
.comment {color: #999; font-style: italic;}
.state {color: #990; font-size: 70%;}
.cumsum {color: #999; font-size: 80%;}
.fileinclude {color: #696; font-weight:bold;}
.warning {color: #c00; font-weight: 700;}

div.filegroup, div.parse, div.stylehelp, div.count, div.sumcount, div.error {
   border: solid 1px #999; margin: 4pt 0pt; padding: 4pt;
}
div.stylehelp {font-size: 80%; background: #fffff0; margin-bottom: 16pt;}
div.filegroup {background: #dfd; margin-bottom: 16pt;}
div.count {background: #ffe;}
div.sumcount {background: #cec;}
div.error {background: #fcc;}
.parse {font-size: 80%; background: #f8fff8; border-bottom:none;}

ul.count {list-style-type: none; margin: 4pt 0pt; padding: 0pt;}
.count li.header {font-weight: bold; font-style: italic;}
.subcount li.header {font-weight: normal; font-style: italic;}
.subcount li {margin-left: 16pt; font-size: 80%;}
.fielddesc {font-style: italic;}
.nb {color: #900;}
-->
</style>
';
}

###### Help routines


# Print TeXcount version
sub print_version {
  wprint "TeXcount version ".$versionnumber.", ".$versiondate.'.';
}

# Print TeXcount reference text
sub print_reference {
  wprint '
The TeXcount script is copyright of Einar Andreas Rødland (2008) and published under the LaTeX Project Public Licence.

Go to the TeXcount web page
    http://app.uio.no/ifi/texcount/
for more information about the script, e.g. news, updates, help, usage tips, known issues and short-comings, or to access the script as a web application. Feedback such as problems or errors can be reported to einarro@ifi.uio.no.
';
}

# Print TeXcount licence text
sub print_license {
  wprint 'TeXcount version '.$versionnumber.'
  
Copyright 2008-2010 Einar Andreas Rødland

The TeXcount script is published under the LaTeX Project Public License (LPPL)
    http://www.latex-project.org/lppl.txt
which grants you, the user, the right to use, modify and distribute the script. However, if the script is modified, you must change its name or use other technical means to avoid confusion.

The script has LPPL status "maintained" with Einar Andreas Rødland being the current maintainer.
';
}

# Print TeXcount parameter list
sub print_syntax {
  wprint '
Syntax: TeXcount.pl [options] files

Options:
@ -          :
  -relaxed      Uses relaxed rules for word and option handling: i.e. allows more general cases to be counted as either words or macros.
  -v            Verbose (same as -v3).
  -v0           Do not present parsing details.
  -v1           Verbose: print parsed words, mark formulae.
  -v2           More verbose: also print ignored text.
  -v3           Even more verbose: include comments and options.
  -v4           Same as -v3 -showstate.
  -showstate    Show internal states (with verbose).
  -brief        Only prints a brief, one line summary of counts.
  -q, -quiet    Quiet mode, no error messages. Use is discouraged!
  -strict       Strict mode, warns against begin-end groups for which rule are not defined.
  -sum, -sum=   Make sum of all word and equation counts. May also use -sum=#[,#] with up to 7 numbers to indicate how each of the counts (text words, header words, caption words, #headers, #floats, #inlined formulae, #displayed formulae) are summed. The default sum (if only -sum is used) is the same as -sum=1,1,1,0,0,1,1.
  -nosum        Do not compute sum.
  -sub, -sub=   Generate subcounts. Option values are none, part, chapter, section or subsection. Default (-sub) is set to subsection, whereas unset is none. (Alternative option name is -subcount.)
  -nosub        Do not generate subcounts.
  -col          Use ANSI colours in text output.
  -nc, -nocol   No colours (colours require ANSI).
  -html         Output in HTML format.
  -htmlcore     Only HTML body contents.
  -opt, -optionfile   Read options/parameters from file.
  -             Read LaTeX code from STDIN.
  -inc          Parse included TeX files (as separate file).
  -merge        Merge included TeX files into code (in place).
  -noinc        Do not include included tex files (default).
  -incpackage=    Include rules for the given package.
  -total        Do not give sums per file, only total sum.
  -1            Same as -brief and -total. Ensures there is only one line of output. If used in conjunction with -sum, the output will only be the total number. (NB: Character is the number one, not the letter L.)
  -template=    Speficy an output template. Use {1},...,{7}, {SUM} and {TITLE} to include values, {1?...?1} etc. to conditionally include sections, {1?....|...?1} etc. to specify an alternative text if zero. To include subcounts, use {SUB?...?SUB} where ... is replaced with the template to use per subcount. Line shift may be specified using \n.
  -dir, -dir=   Specify the working directory using -dir=path. Remember that the path must end with \ or /. If only -dir is used, the directory of the parent file is used.
  -utf8, -unicode    Turns on Unicode (UTF-8) for input and output. This is automatic with -chinese, and is required to handle e.g. Korean text. Note that the TeX file must be save in UTF-8 format (not e.g. GB2312 or Big5), or the result will be unpredictable.
  -ch, -chinese, -zhongwen    Turns on support for Chinese characters. TeXcount will then count each Chinese character as a word. Automatically turns on -utf8.
  -jp, -japanese    Turns on support for Japanese characters. TeXcount will count each Japanese character (kanji, hiragana, and katakana) as one word, i.e. not do any form of word segmentation. Automatically turns on -utf8.
  -char, -character, -letter    Counts letters instead of words. Note that spaces and punctuation is not counted.
  -freq         Produce individual word frequency table.
  -codes        Display output style code overview and explanation. This is on by default.
  -nocodes      Do not display output style code overview.
  -h, -?, -help, /?    Help text.
  -h=, -?=, -help=, /?=    Takes a macro or group name as option and returns a description of the rules for handling this if any are defined. If handling rule is package specific, use -incpackage=package name: -incpackage must come before -h= on the command line to take effect.
  -ver, -version    Print version number.
  -lic, -license, -licence    Licence information.
';
}

# Print TeXcount help text
sub print_help {
  wprint '
***************************************************************
*   TeXcount.pl '.$versionnumber.', '.$versiondate.'
*

Count words in TeX and LaTeX files, ignoring macros, tables, formulae, etc.
';
  print_syntax();
  wprint '
The script counts words as either words in the text, words in headers/titles or words in floats (figure/table captions). Macro options (i.e. \marco[...]) are ignored; macro parameters (i.e. \macro{...}) are counted or ignored depending on the macro, but by default counted. Begin-end groups are by default ignored and treated as \'floats\', though some (e.g. center) are counted.

Unless -nocol (or -nc) has been specified, the output will be colour coded. Counted text is coloured blue with headers are in bold and in HTML output caption text is italicised.

Mathematical formulae are not counted as words, but are instead counted separately with separate counts for inlined formulae and displayed formulae. Similarly, the number of headers and the number of \'floats\' are counted. Note that \'float\' is used here to describe anything defined in a begin-end group unless explicitly recognized as text or mathematics.

The verbose options (-v1, -v2, -v3, showstate) produces output indicating how the text has been interpreted. Check this to ensure that words in the text has been interpreted as such, whereas mathematical formulae and text/non-text in begin-end groups have been correctly interpreted.

Parsing instructions may be passed to TeXcount using comments in the LaTeX files on the format
@ -      :
  %TC:instruction arguments
and are used to control how TeXcount parses the document. The following instructions are used to set parsing rules which will apply to all subsequent parsing (including other files):
  %TC:macro [macro] [param.states]
    \    macro handling rule, no. of and rules for parameters
  %TC:macroword [macro] [number]
    \    macro counted as a given number of words
  %TC:header [macro] [param.states]
    \    header macro rule, as macro but counts as one header
  %TC:breakmacro [macro] [label]
    \    macro causing subcount break point
  %TC:group [name] [param.states] [content-state]
    \    begin-end-group handling rule
  %TC:floatinclude [macro] [param.states]
    \    as macro, but also counted inside floats
  %TC:preambleinclude [macro] [param.states]
    \    as macro, but also counted inside the preamble
  %TC:fileinclue [macro] [rule]
    \    file include, add .tex if rule=2, not if rule=0, if missing when rule=1
The [param.states] is used to indicate the number of parameters used by the macro and the rules of handling each of these: the format is [#,#,...,#] with # representing one number for each parameter giving the parsing state to use for that parameter, alternatively just a single number (#) indicating how many parameters to ignore (parsing state 0). The option [content-state] is used to give the parsing state to use for the contents of a begin-end group. The main parsing states are 0 to ignore and 1 to count as text.

Parsing instructions which may be used anywhere are:
@ -                    :
  %TC:ignore           start block to ignore
  %TC:endignore        end block to ignore
  %TC:break [title]    add subcount break point here
See the documentation for more details.

Command line options and most %TC commands (prefixed by % rather than %TC:) may be placed in an options file. This is particularly useful for defining your own output templates and macro handling rules.

Unix hint: Use \'less -r\' instead of just \'less\' to view output: the \'-r\' option makes less treat text formating codes properly.

Windows hint: Windows does not support ANSI colour codes. Instead, can use -html to produce HTML code, write this to file and view with your favourite browser.
';
  print_reference();
}

# Print help on specific macro or group
sub print_help_on {
  my $arg=shift @_;
  my $def;
  my %rules=(
    '\documentclass'=>'Initiates LaTeX document preamble.',
    '\begin'=>'Treatmend depends on group handling rules.',
    '\def'=>'Excluded from count.',
    '$'=>'Opens or closes inlined equation',
    '$$'=>'Opens or closes displayed equation.',
    '\('=>'Opens inlined equation.',
    '\)'=>'Closes inlined equation initiated by \(.',
    '\['=>'Opens displayed equation.',
    '\]'=>'Closes displayed equation initiated by \[.');
  if (!defined $arg || $arg=~/^\s*$/) {
    print "Specify macro or group name after the -h= option.\n";
    return;
  }
  if ($def=$rules{$arg}) {
    print "Special rule (hard coded) for $arg\n";
    print $def."\n";
  } elsif ($arg=~/^\\/) {
    if ($def=$TeXfileinclude{$arg}) {
      print "Rule for macro $arg\n";
      print "Takes file name as parameter which is included in document.\n";
    } elsif ($def=$TeXmacro{$arg}) {
      print "Rule for macro $arg\n";
      _print_rule_macro($arg,$def);
      if ($def=$TeXheader{$arg}) {
        print "This macro is counted as a header\n";
      }
      if ($def=$TeXfloatinc{$arg}) {
        print "This macro is also counted inside floats as captions.\n";
      }
    } elsif ($def=$TeXmacroword{$arg}) {
      print "Rule for macro $arg\n";
      print "Count as ".$def." word(s).\n";
    } else {
      print "No macro rule defined for $arg.\nParameters treated as surrounding text.\n";
    }
  } else {
    if ($def=$TeXgroup{$arg}) {
      print "Rule for group $arg\n";
      _print_rule_group($arg,$def);
    } else {
      print "No default group rule defined for $arg.\nContent handled as surrounding text.\n";
    }
  }
}

# Print macro handling rule
sub _print_rule_macro {
  my ($arg,$def)=@_;
  my %rules=(0=>"Exclude from count",1=>"Count as text",2=>"Count as header",
    3=>"Count as caption",-1=>"Exclude as float (i.e. include captions)",
    -2=>"Strong exclude (ignore begin-end groups)",
    -3=>"Stronger exclude (ignore all macros)",
    -9=>"Exclude as preamble");
  if (ref($def) eq 'ARRAY') {
    print "Takes ".scalar(@{$def})." parameter(s):\n";
    foreach my $i (@{$def}) {
      print " - ".$rules{$i}."\n";
    }
  } else {
    print "Takes ".$def." parameter(s), not included in counts.\n";
  }
}

# Print group handling rule
sub _print_rule_group {
  my ($arg,$def)=@_;
  my %rules=(
    0=>'Not included',
    1=>'Text, words included in text count',
    2=>'Header, words included in header count',
    3=>'Float caption, words included in float caption count',
    6=>'Inline mathematics, words not counted',
    7=>'Displayed mathematics, words not counted',
   -1=>'Float, not included, but looks for captions');
  print "Rule used: ".$rules{$def}."\n";
  if ($def=$TeXmacro{'begin'.$arg}) {
    _print_rule_macro($def);
  }
}


