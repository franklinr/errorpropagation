#!/usr/bin/perl
$record = -1;
$label = "d";
my %num;

sub min {
    my ($max, $next, @vars) = @_;
    return $max if not $next;
    return min( $max < $next ? $max : $next, @vars );
}

sub levenshtein_dist {

    my ($str1, $str2) = @_;
    my ($len1, $len2) = (length $str1, length $str2);

    if ($len1 == 0) {
	return $len2;
    }
    if ($len2 == 0) {
	return $len1;
    }

    my %mat;

    for (my $i = 0; $i <= $len1; ++$i) {
	$mat{0}{$i} = $i;
	$mat{1}{$i} = 0;
    }

    my @ar1 = split //, $str1;
    my @ar2 = split //, $str2;

    for (my $j = 1; $j <= $len2; ++$j) {
	my $p = $j % 2;
	my $q = ($j + 1) % 2;
	$mat{$p}{0} = $j;
	for (my $i = 1; $i <= $len1; ++$i) {
	    my $cost = 0;
	    if ($ar1[$i-1] ne $ar2[$j-1]) {
		$cost = 1;
	    }
	    $mat{$p}{$i} = min($cost + $mat{$q}{$i-1},
			       $mat{$p}{$i-1} + 1, $mat{$q}{$i} + 1);
	}
    }

    return $mat{$len2%2}{$len1};
}

while(<>){
    chomp;

    if (/name:/){
	$record = 0;
    }
    s/LABEL=́ZEROCLOZ/LABEL=ZEROCLOZE/;

    if (/name:.+? (drink|sip|sniff|taste|take).+? (wine|tea|water) [.] [.]/ && !/[-]par/){
# && !/(was|were|is|are|will)/){
#	print($sent{$labelnum});
	$label = $1."-".$2;
#	print($label);
	$record=1;
	$num{$label} = 	$num{$label} + 1;
	$labelnum = $label.$num{$label};
#	print("\n".$labelnum);
	$sent{$labelnum}=""
    }

    if ($record == 1){
	s/proc:.+/proc:{ clear }/; 
	s/(.+?) [.] [.]/$1 . . LABEL=AAA/;
	$sent{$labelnum} = $sent{$labelnum}."\n".$_;
#	print "\n$_";
    }

    if (/name:.+? LABEL=(PASSCONT|SUBCONT|GARDSC|CATCONT|TENSECONT)/){
	$record=3;
	$fillernum++;
	$filler{$fillernum}=""
    }
    if ($record == 3){
	$filler{$fillernum} = $filler{$fillernum}."\n".$_; 
    }

    if (/^t:{word 1.0}.+?;/){
	if ($record == 3){
#	    print $filler[$fillernum];
	}else{
	    print($sent{$labelnum});
	}
	$record = 0;
#
    }
    $header = $header."\n".$_ if $record < 0;
}

sub selectItems{
    my ($ii,$noun,$tarverb,$primeverb) = @_;
    
    my $labelnumtar = "$tarverb-$noun$ii";
    if (! exists $sent{$labelnumtar}){
	$ii = 1;
	$labelnumtar = "$tarverb-$noun$ii";
	print "\nnot found!!";
    }
    my $tar = $sent{$labelnumtar}; 
    $tarsent = $1 if $tar =~ /name:\{ +(.+?) . . LABEL/;   

    $j = $ii;
    my $prime = "";
 #   do{
	$j++;
	$labelnum = "$primeverb-$noun$j";
	if (! exists $sent{$labelnum}){
	    $j = 1;
	    $labelnum = "$primeverb-$noun$j";
	}
	$prime = $sent{$labelnum};
	$prisent = $1 if $prime =~ /name:\{ +(.+?) . . LABEL/;   
#	$dis =  levenshtein_dist($prisent,$tarsent);
#	print "\n$i $j dist = $dis $prisent  $tarsent";
#    }while($dis < 2);
    print "\n$i $j dist = $dis $prisent  $tarsent";
    $prime =~ s/LABEL=[A-Z;-]+/LABEL=STRONG/g;
    $tar =~ s/LABEL=[A-Z;-]+/LABEL=TEST-PRED/g;
    return($ii, $prime,$tar);
}

#strong drink water/sip tea
#weak sniff water/tea
#test taste water/tea
$sniffnum = 52;
$sipnum = 51;
$drinknum = 49;
$tastenum = 53;
$takenum = 48;

$tarnoun = "water";
$primeverbwater = "drink";
$primeverbwaternum = $drinknum;
$primeverbtea = "sniff";
$primeverbteanum = $sniffnum;
$testverb = "taste";
$testverb2 = "taste";


open(my $fh, '>', 'vrommer.ex');
print $fh $header;
$i = 0;
$k = 0;
$maxi = 15;
while($i < $maxi){
    $i++;
    ($i,$str,$test) = selectItems($i,$tarnoun,$testverb,$primeverbwater);
#    print($i);
#    print("here test1");
#    print("$test\n");
#    print $str;
#    print $test;
#    $test =~ s/$testverb/$testverb2/;
#    $test =~ s/i:{targ 1.0} $sniffnum/i:{targ 1.0} $takenum/;
#    $test =~ s/t:{word 1.0} $sniffnum/t:{word 1.0} $takenum/;
    $test =~ s/(LABEL=[^ ]+) */$1/g;
    print $fh $str;
    print $fh $test;

    $str =~ s/$primeverbwater/$primeverbtea/;
    $str =~ s/i:{targ 1.0} $primeverbwaternum/i:{targ 1.0} $primeverbteanum/;
    $str =~ s/t:{word 1.0} $primeverbwaternum/t:{word 1.0} $primeverbteanum/;
    $str =~ s/LABEL=[A-Z;-]+/LABEL=WEAK/g;
    print $fh $str;
    $test =~ s/LABEL=[A-Z;-]+/LABEL=TEST-UNPRED/g;
    print $fh $test;
#    print($test);

    next if $i == $maxi;
    $k++;
    $str ="";
    $test = "";
}

sub ot{

    ($k,$str,$test2) = selectItems($k,"tea",$testverb,$primeverbtea);
#    print($i);
#    print("here test2");
#    print("$test2\n");
#    $| = 1;

    print $fh $str;
    print $fh $test2;
    $str =~ s/$primeverbtea/$primeverbwater/;
    $str =~ s/i:{targ 1.0} $primeverbteanum/i:{targ 1.0} $primeverbwaternum/;
    $str =~ s/t:{word 1.0} $primeverbteanum/t:{word 1.0} $primeverbwaternum/;
    $str =~ s/LABEL=[A-Z;-]+/LABEL=WEAK/g;
    print $fh $str;
    $test2 =~ s/LABEL=[A-Z;-]+/LABEL=TEST-UNPRED/g;
    print $fh $test2;
#    $| = 1;
}

close $fh;




