#!/usr/bin/perl

$args = "@ARGV";
#printf "\nmaking csv file $args";
$lrate = 0.2;
$dualpath = "../dualpath.in";
#$dualpath = "../sim10-s0-Sim116-alt10-50/dualpath.in";
$test = "/usr/local/bin/lens -b $dualpath 'testERPprimetar 100000 vrommer $lrate;exit' >> output ";
$test2 = "/usr/local/bin/lens -b $dualpath 'testERPprimetar 100000 vromNotSeen $lrate;exit' >> output ";

for $f (@ARGV){
    print("\n$f");
    system("cd $f;rm -f deriv*vrom*;cp ../vromNotSeen.ex .;cp ../vrommer.ex .");
    system("cd $f;cp results/comp100000.wt .;$test");
    system("cd $f;cp results/comp100000.wt .;$test2");
}
print("\ndone");



