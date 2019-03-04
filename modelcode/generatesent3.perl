#!/usr/bin/perl

$arguments = "@ARGV";
if ($arguments =~ /-d/){
    $debug = 1;
    shift @ARGV;
}

if ($arguments =~ /-s (\d+)/){
    srand($1); 
    shift @ARGV;
    shift @ARGV;
}
$lang = "en";
if ($arguments =~ /-lang (\S+)/){
    $lang = $1;
    shift @ARGV;
    shift @ARGV;
}

$ambig = 0;
if ($arguments =~ /-amb (\d+)/){
    $ambig = $1;
    shift @ARGV;
    shift @ARGV;
}
$level = -3;
$amb = -6;
if ($arguments =~ /-level (\-*\d+)/){
    $level = $1;
    shift @ARGV;
    shift @ARGV;
}

$fr = "(XX|YY|ZZ)";
if ($arguments =~ /-aa/){
    $fr = "AA"; 
    shift @ARGV;
}

$passiveperc = 10;
$prepdatperc = 50;

$symmetric =1;
if ($arguments =~ /-nosym/){
    $symmetric =0;
    shift @ARGV;
}

if ($arguments =~ /-alt (\d+),(\d+)/){
    ($passiveperc,$prepdatperc) = ($1,$2);
#    printf "\npassive $passiveperc prepdat $prepdatperc";
    shift @ARGV;
    shift @ARGV;
}

$header = "";
while(<>){
    chomp;

    if (/^\#/){
	$header .= "\n$_"; 
    }

    if (/#cmdlist:/){
	$header .= " | $arguments ";
	$envfile = $1 if /(env\S+)/;
	if (/-s (\d+)/){
	    srand($1);
	    $header .= " randomize $1";
	}else{
	    if (/-n (\d+)/){
		srand($1);
	    }else{
		srand(20);
	    }
	}
	open(ENV,"< $envfile");
	while(<ENV>){
	    if (/^([^\{]+)\{/){
		$sectionName = $1;
		while(<ENV>){
		    last if /^\}/;
		    $section{$sectionName} .= $_;
		}
	    }
	}

	$header .= "\n#parameters: amb $ambig pas $passiveperc pd $prepdatperc level $level amblevel $amb lang $lang";
	$header .= " fr $fr symmetric $symmetric";

	$_ = $header;
	eval $section{"cleanup"};    
	print $_;
    }

    if (/mess:/){  # change message to mark change in structure
	undef @flip;
	if (!/1A=/){ # these are simple sentences
	    s/(0E=\S+?),XX,YY/$1,$level,XX,-10,YY/ if rand(100) < $passiveperc;
	    s/(0E=\S+?),ZZ,YY/$1,$level,YY,-10,ZZ/ if rand(100) < $prepdatperc;
	    s/(0E=\S+?),XX,ZZ,YY/$1,$level,XX,-10,ZZ,YY/ if rand(100) < $passiveperc;
	    s/(E=.+?,$level,.+?LABEL=\S+)/$1-F/;
        }
	$mess = $_;
    }

    if (/sent:/){  # change order in sentence to correspond to message

	s/(0x=[^ ]+)( .*?)(0y=[^ ]+)/$3$2$1/ if $mess =~ /[-]\d,XX,[-]10,ZZ/;
	s/(0x=[^ ]+)( .*?)(0y=[^ ]+)/$3$2$1/ if $mess =~ /[-]\d,XX,[-]10,YY/;
	s/(0y=[^ ]+)( .*?)(0z=[^ ]+)/$3$2$1/ if $mess =~ /[-]\d,YY,[-]10,ZZ/;
	s/ to / / if $mess =~ /[-]\d,YY,[-]10,ZZ/;
       
    if ($section{"sent-rewrite-$lang"}){
        eval $section{"sent-rewrite-$lang"};
    }else{
        # apply language specific rewrite rules
        eval $section{"sent-rewrite"};
    }

    $summarymess =$mess;
    $summarymess =~ s/ \d[AXYZ]=\S+//g;
    $summarymess =~ s/ \dE=\S+?,(SIMP|PROG)//g;
    $summarymess =~ s/ LABEL=\S+//g;

    $changed = 0;

	$count{"napfourverb"}++ if $mess =~ /A=(NAP|JUMP|RUN|WALK)/;
	$count{"napfournoun"}++ if $mess =~ /(X|Y|Z)=(NAP|JUMP|RUN|WALK)/;
	$count{" MATCH $1 $summarymatch $changed"}++ if $mess =~ /LABEL=(\S+)/;

	$sent = $_;
	$_ = $mess;
	eval $section{"cleanup"};    
	print "\n$_";
	print "\n$sent";

	$_ = $sent;
    $count{"sip tea"}++ if / sip .*?tea /;
    $count{"sip water"}++ if / sip .*?water /;
    $count{"sip"}++ if / sip /;
    $count{"sniff wine"}++ if / sniff .*?wine /;
    $count{"sniff water"}++ if / sip .*?water /;
    $count{"sniff"}++ if / sniff /;
    
    }
    
}

for $i (sort keys %count){
    printf "\n#count $i $count{$i}";
}
