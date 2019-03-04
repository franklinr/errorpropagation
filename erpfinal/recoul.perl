#!/usr/bin/perl

$args = "@ARGV";
#printf "\nmaking csv file $args";

$lrate = 0.1;

$test = "/usr/local/bin/lens -b ../dualpath.in 'testERPlearn 100000 vcoul1 $lrate;exit' >>output ";
$test2 = "/usr/local/bin/lens -b ../dualpath.in 'testERPlearn 100000 vcoul2 $lrate;exit' >>output ";

for $f (@ARGV){
    print($f);
    system("cd $f;rm -f deriv*vcoul*");
    system("cd $f;cp results/comp100000.wt .;$test");
    system("cd $f;$test2");
}



