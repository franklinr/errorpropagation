#!/usr/bin/perl
$record = -1;
$label = "d";
my %num;


open(FOO, "perl generatemes2.perl -s 222 -n 300   envgramSim116  | perl -nle 's/,PRN,/,THE,/g;s/=prn,/=the,/g;print' | perl generatesent3.perl -alt 50,50   | translate5.perl | ");
$save = 0;
$item = 0;
while(<FOO>){
    if (/LABEL=I *$/){
	$save=1;
	$item++;
	s/LABEL=I/LABEL=WEAK/;
    }
    $intrans[$item]=$intrans[$item].$last if $save == 1;
    $save = 0 if /^name/;
    s/proc:{ .+/proc:{ clear }/;
    $last = $_;
}
for $i (1 .. 10){
    print "\n$i ".$intrans[$i];
}

$replace = "WEAK";
$sniffnum = 52;
$sipnum = 51;
$drinknum = 49;
$tastenum = 53;
$takenum = 48;

$tarnoun = "water";
$othernoun = "beer";
$tarnounnum = 30; # tea
$othernounnum = 33; # beer

open(my $fh, '>', 'vromNotSeen.ex');
print $fh $header;
while(<>){
    if (/LABEL=$replace/){
	$ii++;
	$intrans[$ii] =~ s/(name:.+?)([}])/$1. LABEL=$replace$2/;
	print $fh $intrans[$ii];
	do{
	    $_ = <>;
	}while(!/;$/);
	$_ = <>;
    }

    $lab = $1 if /LABEL=([A-Z-]+)/;
    $nochange = 0 if /LABEL=STRONG/;
#    $nochange = 1 if /LABEL=STRONG/;
    $nochange = 1 if /LABEL=TEST/;
#    if ($lab eq "WEAK"){
#	s/ sniff / sip /;
#	s/(i|t):(.+?) $sniffnum/$1:$2 $takenum/;
#    }

    if ($lab eq "STRONG" || $lab eq "WEAK"){
	s/ $tarnoun / $othernoun /;
#	s/ tea / beer /;
#	print;
	s/(i:.+?) $tarnounnum/$1 $othernounnum/; # water-> tea
        s/(t:.+?) $tarnounnum/$1 $othernounnum/;
#	s/(i:.+?) 32/$1 30/; # water-> tea
#        s/(t:.+?) 32/$1 30/;
#	s/(i:.+?) 32/$1 29/; # water-> wine
#        s/(t:.+?) 32/$1 29/;
#	s/(i:.+?) 32/$1 31/; # water-> coffee
#        s/(t:.+?) 32/$1 31/;
#	s/(i:.+?) 30/$1 33/; # tea -> beer
#        s/(t:.+?) 30/$1 33/;
#	print;
    }

    print $fh $_;

}

close $fh;




