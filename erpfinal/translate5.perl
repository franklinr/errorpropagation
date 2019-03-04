#!/usr/bin/perl

$arguments = "@ARGV";
$nomess = 0;
if ($arguments =~ /-nm (\d+)/){
    $nomess = $1; 
    shift @ARGV;
    shift @ARGV;
}

$notarg = 0;
if ($arguments =~ /-notarg/){
    $notarg = 1;
    shift @ARGV;
}

$addlabel = 0;
if ($arguments =~ /-addlabel/){
    $addlabel = 1;
    shift @ARGV;
}
srand(12);

$seenTrain = 0;

while(<>){
    chomp;
#    next if $seenTrain == 1 && /^#/;
    printf "\n$_ " if /^#/;
    
    if (/^#(\S+?):\s+(\S.+)/ && !$size{$1}){
#   printf "\nfomd $_ ";
    $type = $1;
    $unittmp = $2;
    $unittmp =~ s/:\S+ / /g;
    $unittmp =~ s/ +/ /g;
#   printf "\n$unittmp";
    @unitarray = split(/ /,$unittmp);
    for $i (@unitarray){
        $mapLabelUnit{"$type $i"} = $unitcount{$type}++;
#       printf " $type $i $unitcount{$type}";
    }
    $size{$type} = $#unitarray + 1;
    $unitlist{$type} = $unittmp;

    $envType = $1 if $type eq "cmdlist" && /(envgram\S+)/;
    }
    
    if (/^(mess:.+)/){
    $mess = $1;
    $_ = <>;chomp;
    $sent = "$_.";
    $sent =~ s/^sent:\s+//;

    printf "\nproc:{ set environmentType $envType } \n;" if $envType ne "";
    $envType = "";

    $extra = $1 if $mess =~ /(LABEL=[^ ]+)/;
    $extra = "" if $addlabel == 0;

    printf "\nname:{ $sent $extra}";
    printf "\n#$mess";
    printf "\nproc:{ clear";
    $mess =~ s/LABEL=\S+//;
    $mess =~ s/^mess:\s+//;
    ## event semantics
    @allevsem = $mess =~ /(\d+E=\S+)/g;
    for $evv (@allevsem){
        if ($evv =~ /(\d+)E=(\S+)/){
        $clause = $1;
        $evsemstring = ",$2";
        $evsemstring =~ s/,:[0-9][A-Z]+,/,/g;
        @evsempart = split(/,/,$evsemstring); 

        $inc = -1* $size{"eventsemantics"};
        $inc += $size{"eventsemantics"};
        $lastnum = -1;
        for $x (@evsempart){
            if ($x =~ /[A-z]+/){
            $num = $mapLabelUnit{"eventsemantics $x"};
#           printf "\n evsem $x $num";
            
            $num += $size{"eventsemantics"}*$clause + 1;
            
            $evsemstring =~ s/,$x/ $num/;
            }
        }
        $mess =~ s/$evv/ ;flink$evsemstring /;
        }
    }
    ## roles
    for $x (0 .. $unitcount{"roles"}){
        if ($mess =~ /(\d*[A-Z])=/){
        $r = $1;
        $num = $mapLabelUnit{"roles $r"}; 
        printError("\n ### ERROR no role $r ") if $unitlist{"roles"} !~ /$r/;
            $mess =~ s/(\d+[A-Z])=/;link $num /;
        }
    }
    ## lexical semantics
    $mess =~ s/,/ /g;
    for $x (0 .. $unitcount{"semantics"}){
        if ($mess =~ /([A-Z]+) /){
        $s = $1;
        $num = $mapLabelUnit{"semantics $s"}; 
        printError("\n ### ERROR no semantics $s ") if $unitlist{"semantics"} !~ /$s/;
            $mess =~ s/([A-Z]+) /$num /;
        }
    }
    ## print mess 
    if (rand(100) < $nomess){
        printf " } ";
    }else{
        printf " $mess;} ";
    }

    @sentlist = split(/ /,$sent);
    printf "\n%d",$#sentlist + 1;
    for $word (@sentlist){
        printf "\ni:{targ 1.0} %d",$mapLabelUnit{"lexicon $word"} if !$notarg;
        printf "\nt:{word 1.0} %d",$mapLabelUnit{"lexicon $word"};
        printError(" \# ERROR word '$word' is not in lexicon \n") if (!$mapLabelUnit{"lexicon $word"});
    }
    printf ";";
    }
}

sub printError{
    my ($label) = @_;
    printf $label;
    printf STDERR $label;
    $error ++; 
    exit if $error > 2;
}

if (!-f "model.tcl"){
    open(MOD,"> model.tcl");
    printf MOD "set lexSize %d \n",$size{"lexicon"};
    printf MOD "set semSize %d \n",$size{"semantics"};
    printf MOD "set eventsemSize %d \n",$size{"eventsemantics"}*2+2;
#    printf MOD "set eventsemSize %d \n",$size{"eventsemantics"}+1;
    close(MOD);
}
