#!/usr/bin/perl

sub getNum{
    opendir(DIR,".");
    @files = grep { s/sim([0-9]+)[^ ]*/$1/ } readdir(DIR);
    $max = -1;
    printf "\nfiles @files";
    for $i (@files){
	s/^0+//;
	$i = int($1) if /^(\d+)/;
        $max = $i if $i > $max;
    }
    $max++;
    $max = "0$max" if $max < 10;
    return($max);
}

$max = getNum;

if (!$ARGV[0]){
    open(GO,"< go");
    while(<GO>){ chomp;$repeatnum = $_ if /\S/; }
    $dirname = sim.$max."sub".$repeatnum.$japanese;
}


$dirname = "sim$max".$ARGV[0].$japanese if $ARGV[0];
#$dirname =~ s/-message2//;
#$dirname =~ s/expand//;
#$dirname =~ s/lang//;

system "
mkdir $dirname;
echo $dirname > summary

rm results/res* 
mv *.wt.gz results
mv *.wt results
mv results sum* $dirname
cp * $dirname
cd $dirname
rm *.md *.png *.html *.RData *.rds *.csv *~
cd ..

gzip ./$dirname/sum*test ./$dirname/train.ex ./$dirname/test.ex ./$dirname/derivatives &
#tar -cvvf $dirname.tar ./$dirname/
rm output deriv* *.md *.png output
"
