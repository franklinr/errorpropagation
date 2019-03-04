#!/usr/bin/perl

$args = "@ARGV";
#printf "\nmaking csv file $args";

$lrate = 0.1;


for $f (@ARGV){
    print($f);
#    system("cd $f;rm -f deriv*vcoul*");
    system("cd $f;cp results/*.wt .");
#;mkdir devellearn;mv deriv*violation* devellearn");
    for $t (6 .. 9){
	$epoch = $t * 10000;
#	$test = "/usr/local/bin/lens -b ../dualpath.in 'testERPlearn $epoch violations $lrate;exit' >>output ";
#	print($test);
#	system("cd $f;$test");
	$test = "/usr/local/bin/lens -b ../dualpath.in 'testERP $epoch violations;exit' >>output ";
	print($test);
	system("cd $f;$test");
    }
}



