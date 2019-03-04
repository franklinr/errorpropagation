#!/usr/bin/perl
$record = -1;
$label = "d";
my %num;

while(<>){
    chomp;

    if (/name:/){
	$record = 0;
    }
    if (/name:.+? LABEL=(PLUR|SING);(CONT|VIOL)/){
	$label = $1.";".$2;
#	print($label);
	$record=1;
	$num{$label} = 	$num{$label} + 1;
	$labelnum = $label.$num{$label};
#	print("\n".$labelnum);
	$sent{$labelnum}=""
    }
    if ($record == 1){
	$sent{$labelnum} = $sent{$labelnum}."\n".$_;
#	print "\n$_";
    }

    if (/name:.+? LABEL=(PASSCONT|ACTCONT|SUBCONT|GARDSC|CATCONT|TENSECONT)/){
	$record=3;
	$fillernum++;
	$filler{$fillernum}=""
    }
    if ($record == 3){
	$filler{$fillernum} = $filler{$fillernum}."\n".$_; 
    }

    if (/^t:{word 1.0}.+?;/){
	$record = 0;
#	print($sent{$labelnum});
#	print $filler[$fillernum];
    }
    $header = $header."\n".$_ if $record < 0;
}

for $prob (1..2){
    open(my $fh, '>', 'vcoul'.$prob.'.ex');
    print $fh $header;
    $i = 0;
    while($i < 30){
	$i++;
	$number = "PLUR";
	$number = "SING" if $i % 2 == 1; 
	if ($prob == 1){
	    $type = "CONT";
	    $type = "VIOL" if $i % 10 == 3 || $i % 10 == 8;
	}else{
	    $type = "VIOL";
	    $type = "CONT" if $i % 10 == 3 || $i % 10 == 8;
	}
	$itemlab = $number.";".$type.$i;
#	print("\n".$itemlab);
	print $fh $filler{$i*3};
	print $fh $sent{$itemlab};
    }
    close $fh;
}



