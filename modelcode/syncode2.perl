#!/usr/bin/perl

sub readGramHash{
    open(GRAM,"< gramhash");
    while(<GRAM>){
    chomp;
    $gramhash{$_}++;
    }
    $gramhash{"read"}++;
    close(GRAM);
}

sub readLexicon{
    my ($lexicon) = @_;
    my @lex = split(/ /,$lexicon);
    my $cat = "NOUN";
    for $i (@lex){
    $wcat = "";
    if ($i =~ /:([A-z]+)/){
        $cat = uc $1;
    }
    $wcat = $cat if $wcat eq "";
    $mapsyncat{"$i"} = $wcat;
#   printf "\ni $i cat $wcat";
    }
    ## result $mapsyncat
}

readGramHash if -f "gramhash";

sub tagUtt{
    my ($utter) = @_;
    my @utt = split(/ /,$utter);
    my $synutt = "";
    my $mesutt = "";
    for $i (@utt){
    next if $i !~ /\S/;
    last if $i eq "per";
    $extra = "";
    $extra = "-".$backmap{$i} if $backmap{$i};
    $mesutt .= $mapsyncat{$i}.":".$i.$extra." ";

    $synutt .= $mapsyncat{$i}.":$extra ";
    }
    return($synutt,$mesutt,@utt);
}

sub messProcess{
    my ($type) = @_;
    my $structure = "";
    #mes is set already

    $mes{$type} = " $mes{$type} ";

    $_ = $mes{$type};
    eval $coding;
    $mes{$type} = $_;

    $flip{$type} = "";
    $flip{$type} = $1 if $mes{$type} =~ s/(FLIP.+)$//;

    $mes{$type} =~ s/ +$//;

    if ($type eq "act"){  ## process starts after both tar and act are retrieved
    printf "\nmestar: $mes{tar} |";
    printf "\nmesact: $mes{act} |";

    if ($mes{"tar"} eq $mes{"act"}){  ## message same
        printf " mescorr";
        $count{"mes=$mestype"}++;

        $flipval = 0;
        if ($flip{"tar"} ne $flip{"act"}){
        printf " flipped";
        $count{"flip=$mestype"}++;
        $flipval = 1;
        }
        $count{"flip=$mestype all"}++;

    }else{
        printf " meswrong";
    }
    $count{"mes=$mestype all"}++;
    }
}

sub gramProcess{
    my ($type) = @_;
    # prepare utterance for grammatical check
    $gramutt{$type} = " $synutt{$type} ";
    $gramutt{$type} =~ s/:\S* / /g;  #remove role stuff for grammaticality
#    printf "\ngramutt $gramutt{$type}";
    
    if ($type eq "act"){  ## print out results
    
	$gramhash{$gramutt{"tar"}}++;
    
	printf "\nsyntar: $synutt{tar}";
	printf "\nsynact: $synutt{act}";

	if ($gramhash{$gramutt{"act"}}){
	    printf "= gram";
	    $count{"gram=$mestype"}++;
	    $count{" gram=$label"}++;
	}else{
	    printf "= ungram";
	}
	$count{"gram=$mestype all"}++;
	$count{" gram=$label all"}++;
	
    }
}

sub readMesrewrite{
    my ($envfile) = @_;
    $envfile = "env/$envfile" if -f "env/$envfile";
    open(EFILE,"< $envfile");
    while(<EFILE>){
    if (/^coding\{/){
        while(<EFILE>){
        last if /^\}/;
        $coding .= $_;
        }
    }
    }
    printf "\ncoding $envfile $coding \n}";
}


sub countDTcoding{
    my ($lab,$lastutt,$thisutt) = @_;
    $lastutt =~ s/ a / the /g;
    $lastutt =~ s/ (-ed|-s) / /g;
    $lastutt =~ s/ is (\S+) -ing / $1 /g;
    $thisutt =~ s/ a / the /g;
    $thisutt =~ s/ (-ed|-s) / /g;
    $thisutt =~ s/ is (\S+) -ing / $1 /g;

    $count{" dtsent=$label"}++ if $lastutt eq $thisutt;
    $count{" dtsent=$label all"}++;
    
    if ($lastutt =~ / to \./){
        $lastutt =~ s/ to \./ \. \./;
        $count{" dtsent=$label"}++ if $lastutt eq $thisutt;
        }
    }

while(<>){
    chomp;
    readLexicon($1) if (/^\#lexicon: (.+)/);
    readMesrewrite($1) if (/^\#cmdline:.*? (\S+)$/);
    if (/name:/){
    $mestype = "_";
    $mestype = "$1" if / \#([^\} ])/;
    }
    
    if (/mess:/){
    $message = $_;
    $message =~ s/\#mess:\s+//;
    $label = $1 if /LABEL=(\S+)/;
    $message =~ s/\d+E=\S+//;
    $message =~ s/([A-Z]=[^,]+),\S+/$1/g;
    @rolefill = $message =~ /(\d+[A-Z])=(\S+)/g;
    %backmap = ();
#   printf "\nroles $#rolefill";
    for $i (0 .. ($#rolefill+1)/2){
        $x = $rolefill[$i*2];
        $y = $rolefill[$i*2+1];
        $backmap{lc($y)} .= $x 
#if !$backmap{lc($y)};
    }
#   print "\nmessage $message ";
    }

    printf "\n$_";
    if (/(tar|act): (.+)/){
	$type = $1;
	$origutt = $2;

	$utt = $origutt;
	$utt =~ s/ that / /g; # remove that, since optional

	my @uttcopy;
	if ($type eq "act"){
#	    print("\nlast $lastutt \nnew $utt");
	    $count{"sent=$mestype"}++ if $lastutt eq $utt;
	    $count{"sent=$mestype all"}++;
	    
	    $count{" sent=$label"}++ if $lastutt eq $utt;
	    $count{" sent=$label all"}++;
	    
	    countDTcoding($label,$lastutt,$utt);
	    
	    printf " sentwrong " if $lastutt ne $utt;
	}

	($synutt{$type},$mes{$type},@uttcopy) = tagUtt($utt);
	gramProcess($type);

	messProcess($type);
    
#   printf " label $label ";
	if ($type eq "act"){
#       for $i (0 .. $#lastuttcopy){
#       if ($lastuttcopy[$i] ne "per"){
#           $count{"word=$mestype"}++ if $uttcopy[$i] eq $lastuttcopy[$i];
#           $count{"word=$mestype all"}++;
#       }
#       }
    }
    $lastutt = $utt;
    @lastuttcopy = @uttcopy;
    }

}

sub writeGramHash{
    open(GRAM,"> gramhash");
    for $i (keys %gramhash){
    printf GRAM "$i\n";
    }
    close(GRAM);
}

writeGramHash;

printf "\n##results ";
for $i (sort keys %count){
    next if $i !~ /synprime/;
    next if $i =~ / all/;
    $d = $i;
    $d =~ s/ target .+? (prime .+)/ $1 all/;
#    $d =~ s/ prime .+/ all/;
    $count{$i} = 0 if !$count{$i};
    if ($count{$d} > 0 ){
    printf "\n##$i corr c %d t %d",$count{$i},$count{$d};
    printf " perc %d%",$count{$i}/$count{$d}*100;
    }
}
for $d (sort keys %count){
    next if $d !~ / all/;
    next if $d =~ /synprime/;
    $i = $d;$i =~ s/ all//;
    $count{$i} = 0 if !$count{$i};

    printf "\n##$i corr c %d t %d perc %d%",$count{$i},$count{$d},$count{$i}/$count{$d}*100 if ($count{$d} > 0);
}
