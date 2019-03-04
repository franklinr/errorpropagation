#!/usr/bin/perl

srand(777);

$numtest = 30;

sub updateCloze{
    ($mess,$sent,$label,$verb,$noun) = @_;
    $mess =~ s/0Y=([^, ]+) /0Y=$noun /;
    $mess =~ s/LABEL=[^ ]+/LABEL=$label/;
    $mess =~ s/=SIP/=$verb/;
    $noun2 = lc($noun);
    $sent =~ s/ tea / $noun2 /;
    $verb2 = lc($verb);
    $sent =~ s/ sip / $verb2 /;
    return("$mess$sent");
}

sub createCloze{
    ($origmess,$origsent) = @_;
    return if $count{"CLOZE"} >= $numtest;
    $newout = "";
    $verb = lc($1) if $origmess =~ /0A=([^, ]+)/;
    $origsent =~ s/ $verb / drink /;
    $sent = $origsent;
    $mess = $origmess;
    $mess =~ s/LABEL=[^ ]+/LABEL=HIGHCLOZE/;
    $newout = $mess.$sent;
    $sent =~ s/ drink / taste /;
    $mess =~ s/LABEL=[^ ]+/LABEL=LOWCLOZE/;
    $newout .= $mess.$sent;
    $sent =~ s/ taste / take /;
    $mess =~ s/LABEL=[^ ]+/LABEL=ZEROCLOZE/;
    $newout .= $mess.$sent;
    
    $count{"CLOZE"}++;
    return($newout);
}

sub createClozeConstraint{
    ($origmess,$origsent) = @_;
    return if $count{"CLOZECON"} >= $numtest;
    $newout = "";
    $obj = lc($1) if $origmess =~ /0Y=([^, ]+)/;
    $origsent =~ s/ $obj / the tea /;
    $origsent =~ s/ (the|a) the / the /;
    $newout = $newout.updateCloze($origmess,$origsent,"STRONGCONS;EXPECTED","SIP","TEA");
    $newout = $newout.updateCloze($origmess,$origsent,"WEAKCONS;EXPECTED","SNIFF","WINE");
    $lowcloze = "WATER";
    $newout = $newout.updateCloze($origmess,$origsent,"STRONGCONS;UNEXPECTED","SIP",$lowcloze);
    $newout = $newout.updateCloze($origmess,$origsent,"WEAKCONS;UNEXPECTED","SNIFF",$lowcloze);
    
    $count{"CLOZECON"}++;
    return($newout);
}

sub createSubjVerbAgreement{
    ($origmess,$origsent) = @_;
    return if $count{"AGREE"} >= $numtest;
    $newout = "";
    $mess = $origmess;
    $sent = $origsent;
    $mess =~ s/LABEL=[^ ]+/LABEL=PLUR;CONT/;
    $newout = $mess.$sent;
    $mess =~ s/LABEL=[^ ]+/LABEL=PLUR;VIOL/;
    $sent =~ s/([-]s [^ ]+)/$1 -ss/;
    $newout .= $mess.$sent;
    $mess =~ s/LABEL=[^ ]+/LABEL=SING;CONT/;
    $sent =~ s/ [-]s / /;
    $newout .= $mess.$sent;
    $mess =~ s/LABEL=[^ ]+/LABEL=SING;VIOL/;
    $sent =~ s/ [-]ss / /;
    $newout .= $mess.$sent;
    
    $count{"AGREE"}++;
    return($newout);
}

sub createSemP6{
    ($origmess,$origsent) = @_;
    return if $count{"SEMP6"} >= $numtest;
    $newout = "";
    $mess = $origmess;
    $sent = $origsent;
    $mess =~ s/LABEL=[^ ]+/LABEL=PASSCONT/;
    $newout = $mess.$sent;
    $mess =~ s/LABEL=[^ ]+/LABEL=ACTCONT/;
    $pat = lc($1) if$mess =~ /0Y=([^, ]+)/;
    $agt = lc($1) if$mess =~ /0X=([^, ]+)/;
#    printf "agent $agt $pat $sent\n";
#    $sent =~ s/are (.+?) -par by/are $1 -ing/;
    $sent =~ s/is (.+?) -par by/is $1 -ing/;
    $sent =~ s/ $pat (.+?) $agt / $agt $1 $pat /;
    $newout .= $mess.$sent;
    $sent = $origsent;
    $mess =~ s/LABEL=[^ ]+/LABEL=SEMATTR/;
    $sent =~ s/ [-]par by / -ing /;
    $newout .= $mess.$sent;

#    $newverb = lc(randomWord("NONDRINK"));
#    $mess =~ s/LABEL=[^ ]+/LABEL=NOATTR/;
#    $sent =~ s/ [A-z]+ (is|are) / $newverb $1 /;
#    $newout .= $mess.$sent;
    
    $count{"SEMP6"}++;
    return($newout);
}

sub createTense{
    ($origmess,$origsent) = @_;
    return if $count{"TENSE"} >= $numtest;
    $newout = "";
    $mess = $origmess;
    $sent = $origsent;
    $mess =~ s/LABEL=[^ ]+/LABEL=TENSECONT/;
    $newout = $mess.$sent;
    $mess =~ s/LABEL=[^ ]+/LABEL=TENSEVIOL/;
    $sent =~ s/will (.+?) /will $1 -ed /;
    $newout .= $mess.$sent;
    
    $count{"TENSE"}++;
    return($newout);
}

sub createWordCat{
    ($origmess,$origsent) = @_;
    return if $count{"CAT"} >= $numtest;
    $newout = "";
    $mess = $origmess;
    $sent = $origsent;
    $intrans2 = lc($intrans);
    $sent =~ s/ $intrans2 / the $1 /;
    $sent =~ s/the the/the/;
    $mess =~ s/LABEL=[^ ]+/LABEL=CATCONT/;
    $newout = $mess.$sent;
    $mess =~ s/LABEL=[^ ]+/LABEL=CATVIOL/;
    $sent =~ s/-s [.]/-ed ./;
    $newout .= $mess.$sent;
    
    $count{"CAT"}++;
    return($newout);
}

sub randomWord{
    my ($cat) = @_;
    @lis = @{ $category{$cat} };
#    print(@lis);
    $r = int(rand(1000))%($#lis+1);
    return($lis[$r]);
}

sub createSubcat{
    ($origmess,$origsent) = @_;
    return if $count{"SUBCAT"} >= $numtest;
    $newout = "";
    $mess = $origmess;
    $sent = $origsent;
    $mess =~ s/LABEL=[^ ]+/LABEL=SUBCONT/;
    $newout = $mess.$sent;

    $oldverb = $1 if $mess =~ /0A=([^ ,]+)/;
    $newverb = randomWord("TRANSVERB");
#    print("new $newverb $oldverb ");
    $mess =~ s/LABEL=[^ ]+/LABEL=SUBVIOL/;
    $mess =~ s/0X=[^ ,]+/0X=$newverb/;
    $newverb = lc($newverb);
    $oldverb = lc($oldverb);
    $sent =~ s/ $oldverb / $newverb /;
    $newout .= $mess.$sent;
    
    $count{"SUBCAT"}++;
    return($newout);
}


sub createGarden{
    ($origmess,$origsent) = @_;
    return if $count{"GARDEN"} >= $numtest;
    $newout = "";
    $mess = $origmess;
    $sent = $origsent;
#    $sent =~ s/ that / /g;
    $mess =~ s/LABEL=[^ ]+/LABEL=GARDSC/;
    $newout = $mess.$sent;
    $sent =~ s/ that / /g;
#    $sent =~ s/(believe|know)(.*? )(the|a)( [A-z]+)( [-]s)*.+/$1$2$3$4$5 . /;
    $mess =~ s/LABEL=[^ ]+/LABEL=GARDAMB/;
    $newout .= $mess.$sent;
    
    $count{"GARDEN"}++;
    return($newout);
}

sub replaceDiffCategory{
    my ($role,$messsent) = @_;    
    
    $living = " @{ $category{LIVING} } ";
    $nonliving = " @{ $category{NONLIVING} } ";
    $x = $1 if $messsent =~ /$role=([^, ]+)/;

    if ($living =~ / $x /){
	$lx = lc($x);
	$new = lc(randomWord("NONLIVING"));
	$messsent =~ s/ $lx / $new /;
    }
    if ($nonliving =~ / $x /){
	$lx = lc($x);
	$new = lc(randomWord("LIVING"));
	$messsent =~ s/ $lx / $new /;
    }
    return($messsent);
}

sub createPosition{
    ($origmess,$origsent) = @_;
    return if $count{"POSITION"} >= $numtest;

    $newout = "";
    $mess = $origmess;
    $sent = $origsent;
#    $mess =~ s/,PL / /g;
#    $sent =~ s/ [-]s / /g;
    $mess =~ s/LABEL=[^ ]+/LABEL=CONG/;
    $newout = $mess.$sent;
    $newout2 = $newout;
    $newout2 =~ s/LABEL=[^ ]+/LABEL=INCOH/;
    $newout2 = replaceDiffCategory("0X",$newout2);
    $newout2 = replaceDiffCategory("0Y",$newout2);
    $newout2 = replaceDiffCategory("0Z",$newout2);
    $newout = $newout.$newout2;

    $count{"POSITION"}++;
    return($newout);
}



while(<>){
    if (/#cmdlist:.+? (env\S+)/){
        $envfile = $1;
        open(ENV,"< $envfile");
        while(<ENV>){
            if (/^categories\{/){
                $sectionName = $1;
                while(<ENV>){
		    chomp;
                    last if /^\}/;
		    s/ +/ /g;
		    @p = split(/ /,uc($_));
		    if ($#p > 2){
			$cat=shift @p;
			$syn =shift @p;
			@{ $category{$cat} } = @p;
#			print("@{ $category{$cat} }");
		    }
                }
            }
	}
	$intrans ="@{ $category{INTRANSVERB1} }";
	$intrans =~ s/ /|/g;
	$intrans = "($intrans)";
	print("intrans $intrans");
    }


    if (/^mess:/){
	$mess = $_;
	$_ = <>;
	$sent = $_;
	
	next if $mess =~ /PRN/;
#	$mess =~ s/PRN/THE/g;

	if ($mess =~ /0A=.+? 0Y=$intrans,THE,PL.+?LABEL=TR( |$)/){
	    $newout = createWordCat($mess,$sent);
	    print($newout);
	    next;
	}

	next if /0Y=$intrans/; # don't match any of the rest if run is used as noun

#	if ($mess =~ /0X=.+?,(THE|A) 0Y=.+?,(THE|A) 0Z=.+?,(THE|A) 0E=PRES,SIMP.+?LABEL=DAT/){
#	    print("pos $mess");
	    if ($mess =~ /(GIVE|LEND).+?DAT[-]F/ ||  $mess =~ /(SEND|THROW).+?DAT( |$)/){
#		print ("match");
		$newout = createPosition($mess,$sent);
		print($newout);
		next;
	    }
#	}

	if ($mess =~ /(SIP|SNIFF|TASTE|DRINK) 0X=.+?(THE|A) 0Y=.+?(THE|A) 0E=PRES,SIMP.+?LABEL=TR-F/ & $mess !~ /PRN/){
	    $newout = createSemP6($mess,$sent);
	    print($newout);
	    next;
	}

	if ($mess =~ /0X=[^ ]+? 0Y=.*?THE.+? 0E=FUTR.+? LABEL=TR( |$)/){
	    $newout = createTense($mess,$sent);
	    print($newout);
	    next;
	}

	if ($mess =~ / 0Y=WATER,THE .+?LABEL=TR( |$)/){
	    $newout = createCloze($mess,$sent);
	    print($newout);
	    next;
	}

	if ($mess =~ /=(SIP).+?0Y=.+?,THE .+?LABEL=TR( |$)/){
	    $newout = createClozeConstraint($mess,$sent);
	    print($newout);
	    next;
	}
	if ($mess =~ /0X=[^ ]+?,THE,PL 0Y=.*?0E=PRES,SIMP.+? LABEL=TR( |$)/){
	    $newout = createSubjVerbAgreement($mess,$sent);
	    print($newout);
	    next;
	}

	if ($mess =~ /LABEL=IW/){
	    $newout = createSubcat($mess,$sent);
	    print($newout);
	    next;
	}

	if ($mess =~ /1Y=[^ ]+?,THE 1E=PRES,SIMP LABEL=SC/ & $sent =~ / that /){
	    $newout = createGarden($mess,$sent);
	    print($newout);
	    next;
	}

    }else{
	print;
    }
}

for $i (keys %count){
    printf("\n# $i $count{$i}");
}
