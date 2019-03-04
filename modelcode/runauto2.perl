#!/usr/bin/perl

sub updatecmdfile{
# updates cmdfile and returns cmd
    open(CMD,"cmdfile");
    $newout = "";
    while(<CMD>){
    last if !/^#/;
    $newout .= $_;
    }
    chomp;
    $cmd = $_;
    printf "\ncmd $cmd \n";
    if ($cmd =~ /SET .+? ([0-9]+)/){
        $num = $1;
        if ($num < 9){
            $num++;
#           print "\nnum $num $newout";
            $nextcmd = $cmd;
            $nextcmd =~ s/(SET .+?) ([0-9]+)/$1 $num/;
            $newout .= "$nextcmd\n";
        }else{
            $newcmdout = $cmd;
            $newcmdout =~ s/([-]s) [0-9]+/$1 0/;
#            $newcmdout =~ s/SET -s [0-9]+ /SET -s 228 /;
            $newout .= "#$newcmdout\n";
        }
        $cmd =~ s/SET //;
    }else{
	if ($cmd =~ /[A-z]/){
	    $newout .= "#$cmd\n";
	}else{
	    $newout .= "$cmd\n";
	}
    }
    while(<CMD>){
    $newout .= $_;
    }
    close(CMD);
    open(CMD," > cmdfile");
    print CMD "$newout";
    close(CMD);
    return($cmd);
}    

sub runcmdandmodel{
    ($cmd) = @_;
    $pnum = 0;
    ($cmd1,$cmd2,$tclcmd) = split(/===/,$cmd);
    $pnum = 10000 if $pnum == 0;

    print "\ncmd $cmd1 ";

    $nm="-nm 70";
    $nm = "-nm $1" if $cmd2 =~ s/NM *([0-9]+)//;

    system("rm -f derivatives model.tcl beforcode.tcl aftercode.tcl *.bak");
    $createtrain = "perl generatemes2.perl $cmd1 | perl generatesent3.perl $cmd2 | perl translate5.perl $nm > train.ex ";
    printf "\ndoing $createtrain ";
    system ($createtrain);
    system ("egrep \"(name:|mess)\" train.ex  | head -n 40");
        
    $maketest = $makeviolations = 0;
    $maketest = 1 if $cmd1 =~ /\-s 0/;
    $makeviolations = 1 if $cmd1 =~ /\-s 0/;
    # remove seed/number for test generation
    $cmd1 =~ s/-s \d+//;
    $cmd1 =~ s/-n \d+//;
    $cmd1 =~ s/-lev \d+//; 

    if ($maketest){
	$envfile = $1 if $cmd1 =~ /(env[^ ]+)/;
        $createtest = "perl generatemes2.perl -s 111 -n 200 $cmd1 | perl generatesent3.perl $cmd2 | perl translate5.perl > test.ex";
        printf "\ndoing $createtest ";
        system ($createtest);
    }
 
    if ($makeviolations){
	$cmd2 =~ s/[-]alt [0-9,]+//;
        $createviol = "perl generatemes2.perl -s 222 -n 30000 $cmd1 | perl -nle 's/,PRN,/,THE,/g;s/=prn,/=the,/g;print' | perl generatesent3.perl -alt 50,50 $cmd2 | perl filterViol3.perl | perl translate5.perl -nm 100 -addlabel > violations.ex";
        printf "\ndoing violations $createviol";
        system ($createviol);
	$createcoul = "perl coulson2.perl violations.ex";
        system ($createcoul);
	$createrom = "perl rommers.perl train.ex";
        system ($createrom);
	$createromNot = "perl romNotSeen.perl vrommer.ex";
        system ($createromNot);
    }
 
    
    printf "\ntclcmd $tclcmd \n";
    open(FI,"> beforecode.tcl");
    print FI "$tclcmd";
    close(FI);


    sleep 2;
    print("stdout from lens is in output file");
    if ($ENV{'USER'} eq "chang") {
	$runmodel = "/usr/local/bin/lens -b dualpath.in 'trainSave 10;exit' >>output ";
    }else{ # for hartmut
	$runmodel = "lens -b dualpath.in 'trainSave 10;exit' >> output";
    }
    printf "\nrunning model $runmodel";
    system ($runmodel);
    sleep 5;
}

sub archivemodel{
    ($cmd) = @_;
    $cmd =~ s/rmStuff//g;
    $cmd =~ s/perl/-/;
    $cmd =~ s/,/-/g;
    $cmd =~ s/envgram/-/g;
    $cmd =~ s/;+/ /g;
    $cmd =~ s/set//g;
    $cmd =~ s/ ([A-z][A-z])[^ ]+?([A-z][A-z])\b/ $1$2 /g; #shorten long words to first and last two letters   
    $cmd =~ s/\.pl//g;
    $cmd =~ s/\.perl//g;
    $cmd =~ s/level//;

    $cmd =~ s/-n \d+//;
    $cmd =~ s/===/-/g;
    $cmd =~ s/\|//g;
    $cmd =~ s/ +//g;
    $cmd =~ s/-+/-/g;
    $cmd =~ s/[-]$//g;
    system("perl arcthis.perl $cmd");
#    system("perl makedata.perl derivatives");
}

$cmd = updatecmdfile;
if ($cmd =~ /\S/){
    if ($cmd =~ /^cmd: *(\S+.+)/){
        print("\ndoing this command $1 ");
        system($1);
        print("\ndone with cmd");
        }else{
    runcmdandmodel($cmd);
    archivemodel($cmd);
        }
    system("perl runauto2.perl &");
}
