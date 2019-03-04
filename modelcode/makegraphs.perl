#!/usr/bin/perl

$args = "@ARGV";
#printf "\n$args";
printf "\nmaking csv file";

open(OUT,"> results/testdata.csv");
printf OUT "sub,seed,model,epoch,measure,cond,corr,total,perc";

    for $filen (@ARGV){
        open(INP,"gunzip -cf $filen | ");
            while(<INP>){
                next if !/## sent/;
#                next if !/sent/;
                chomp;

                $filen =~ s/##(\S+)/## _ $1/;
                $filen =~ s/\.gz/ /;
                
                $filen =~ s/--//g;
                $filen =~ s/-(s\d+)/ $1 /;
                $filen =~ s/-alt.*message\d//g;
                $filen =~ s/\/sum/ /g;
                $filen =~ s/test/ /g;
                $filen =~ s/-n\d+(\d\d)/$1/;
                $filen =~ s/alt(\d+),(\d+)/alt$1-$2/g;
                               
                s/=/ /;
                s/%/ /;
                s/corr c//;
                s/ t / /;
                s/ perc / /;
     
                $line = "$filen $_";
                $line =~ s/-[CVM]//;
                $line =~ s/##//;
#                $line =~ s/dtsent//;
                $line =~ s/\s+/,/g;
                $line =~ s/,$//;
#		printf "\n$line ";
		printf OUT "\n$line ";
	    }
}
close(OUT);
close(INP);

printf "\nmaking graphs";
#system("cat ahhierarchy2.r | R -q --no-save --slave < ahhierarchy2.r > ahhierarchy.out 2>&1 ");
#system("Rscript polars3.R > polars.out");
#system("open validSD.pdf");
#system("checkfreq.perl train.ex"
