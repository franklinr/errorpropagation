#!/usr/bin/perl

sub searchCloze{
    my ($word) = @_;
    $count{"$word tea"}++ if / $word.*? tea /;
    $count{"$word wine"}++ if / $word.*? wine /;
    $count{"$word water"}++ if / $word.*? water /;
    $count{"$word"}++ if / $word / && !/ $word.*? (it|them|[-]par) /;    
#    print("\n$_") if / $word / && !/ $word.*? (it|them|[-]par) /;    
    
}

for $filen (@ARGV){
    print("\nreading file $filen");
    open(INP,"gunzip -cf $filen | ");
    while(<INP>){
	next if !/(mess:|name:)/;
	chomp;

	searchCloze("sip");
	searchCloze("drink");
	searchCloze("sniff");
	searchCloze("taste");
	searchCloze("take");

	$count{"dativePD"}++ if /=(SEND|THROW) /;
	$count{"dativeDO"}++ if /=(GIVE|LEND) /;
	$count{"dativePD $2"}++ if /=(SEND|THROW) .+?LABEL=(DAT[^ ]*)/;
	$count{"dativeDO $2"}++ if /=(GIVE|LEND) .+?LABEL=(DAT[^ ]*)/;

	$count{"pastfut will"}++ if / will /;
	$count{"pastfut"}++ if / ([-]ed|will) /;

	$count{"believeknow"}++ if / (believe|know) /;
	$count{"believeknow SC that"}++ if / (believe|know).*? that /;
	$count{"believeknow SC"}++ if / (believe|know).*? (nap|run|walk|jump) /;

	$subj = "" if /mess/;
	$subj = $1 if /(0X=.+) .+?0E=PRES,SIMP.+? LABEL=TR/;
	if ($subj ne ""){
#	    print("\n".$subj);
	    $num = "SG";
	    $num = "PL" if $subj =~ /PL/;
#	    print($num);
	    $count{"TRANS"}++;
	    $count{"TRANS $num"}++;
	}

	if (/name:{ *([A-z].+) *}/){
	    @w = split(" +",$1);
	    $prevw = "START";
	    foreach $i (0..$#w){
		$unigram{$w[$i]}++;
		$postotal{"$prevw $i"}++ if $pos{"$prevw $i $w[$i]"} == 0;
		$pos{"$prevw $i $w[$i]"}++;
		$prevw = $w[$i];
	    }
	}
#	print;
    }
}

print("position");
for $i (sort keys %postotal){
    print "$i,$postotal{$i}\n";
}


open(UNI,"> results/unigram.csv");
print UNI "pair,num\n";
for $i (sort keys %unigram){
    print UNI "$i,$unigram{$i}\n";
}
close(UNI);


open(OUT,"> results/counts.csv");
print OUT "pair,num,den,prop\n";
for $i (sort keys %count){
    @k = split(/ /,$i);
    if ($#k > 0){
	$p = $count{$i}/$count{$k[0]};
	print OUT "$i,$count{$i},$count{$k[0]},$p\n";
    }
}

close OUT;
print("\nCOUNTS from train.ex\n");
system("cat results/counts.csv");
system("egrep \"(tea|water|wine)\" results/unigram.csv ");
print("\n");
