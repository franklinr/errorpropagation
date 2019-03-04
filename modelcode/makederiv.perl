#!/usr/bin/perl

$args = "@ARGV";

sub readLexicon{
    my ($lexicon) = @_;
    $lexicon =~ s/ :per [.]/ per/;
    my @lex = split(/ /,$lexicon);
    my $cat = "NOUN";
#    print $lexicon;
#    print $lexicon;
    $vocab = $lexicon;
    $vocab =~ s/ :[^ ]+ / /g;
    @vocablist = split(/ /,$vocab);

    print($vocab);
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

$first =$ARGV[0];
$exfile = "violations.ex";
$outfile = "results/dataframe.csv";
if ($first =~ /(vcoul[12])/){
    $exfile = $1.".ex";
#    $outfile = "data".$1.".csv";
}

$first =~ s/\/deri.*//;
print("first $first");
$sentnum = 0;
#open(VIOL,"violations.ex");
open(VIOL,"gunzip -cf $first/$exfile | ");
    while(<VIOL>){
        readLexicon($1) if (/^\#lexicon: (.+)/);

        if (/^\#lexicon: .* :nouna (.*) :aux/){    
            $contentWords = $1;
            @contentWords = split(/ /,$contentWords);
            for $cw (@contentWords){
                $CW{$cw}++;
                }
            }    
        if (/^name:\{ *([^ ].+?) *\}/){
            $name = $1;
            }
        if (/LABEL=(\S+)/){
            $label = $1;
#            $sent{$name} = $label;
	    $sentnum = $sentnum+1;
	    $labels[$sentnum] = $label;
            push @{ $sent{$name} }, $label;
#            print "$name\n";
#    foreach ( @{$sent{$name}} )  {
#        print "$_\n";
#    }

#            print "$sent{$name}\n";

            }
        }
close VIOL;
#    foreach ( @{$sent{"the man eat -ed the apple . ."}} )  {
#        print "$_\n";
#    }


printf "\nmaking csv file $args";

open(OUT,">$outfile");
$filen = $ARGV[0];
#print($filen);
open(INP2,"gunzip -cf $filen | ");
<INP2>;
$_ = <INP2>;
#print($_);
@n = split(/,/,$_);
$unitlen = $#n;
#print($unitlen);

printf OUT "file,word,wordcat,contfunc,sent,condition,tick,layer,measure,none";
for ($i=1;$i<=$unitlen;$i++){
    if ($i < $#vocablist+1){
	print OUT ",$vocablist[$i]";
    }else{
	printf OUT ",U$i";
    }
}
printf OUT ",,,,,,,,,,,,,,,,,,,,,,,,,";
#printf("header\n");

for $filen (@ARGV){
    $lastsent = "ds";
    $sentnum = 0;
    print("\nreading file $filen");
    open(INP,"gunzip -cf $filen | ");
    while(<INP>){
	chomp;
	next if !/[A-z]/;
	
	@parts = split(/,/,$_);
#	    if (/^( *([^,]+?) *,(\d+)/ ){
	$sent = $parts[0];
	$sent =~ s/^ +//;
	$sent =~ s/ +$//;
	$tick = $parts[1];
#	    print($sent);
	@words = split(/ /,$sent);
	$word = $words[$tick];
	
	$wordcat = $mapsyncat{$word};
	$wordType = "F";
	if ($CW{$word}){
	    $wordType = "C";
	}
	s/ *LABEL=/,/;
	printf OUT "\n$filen,$word,$wordcat,$wordType,$_";
#            }
	$lastsent = $sent;
    }
}

close INP;
close OUT;

#system("Rscript -e \"library(knitr);require(markdown); knit('stat2.Rmd');markdownToHTML('stat2.md', 'stat2.html') \" ");

