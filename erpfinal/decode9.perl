#!/usr/bin/perl

$threshhold = 0.3;
$threshhold = 0.5 if -f _sepmorph;
$arguments = "@ARGV";
$examplefile = $1 if $arguments =~ /res\d+([a-zA-Z][^.]+)./;
$examplefile = $1 if $arguments =~ /exp[0-9.-]+-(sp[A-Z]+\S+?[A-Za-z]+)-[^- ]+?.out/;
$threshhold = $1 if $arguments =~ /-t (\S+)/;
$lag = $1 if $arguments =~ /-lag (\S+)/;
$examplefile = $1 if $arguments =~ /-f (\S+)/;
$examplefile .= ".ex" if $examplefile !~ /.ex/;

$threshhold = 0.5 if -f "_t05";

printf "\n#var threshhold $threshhold examplefile $examplefile \n";
sub readWords{
    $examplefile = "env/$examplefile" if -f "env/$examplefile";
    open(PH,"< $examplefile");
    while(<PH>){
    printf "$_" if /^\#/;
    s/ :\S+ / /g;
    s/ :\S+ / /g;
    s/null //;
    @phones = split(/ /,$1) if /lexicon: (.+)/;
#   unshift @phones,1;
    last if /#cmdlist:/;
#   $l = $_;
    }
#    printf "phones @phones\n";
}

sub getNextMes{
    while(<PH>){
    chomp;
    last if /^\#mess:/;
    $l = $_;
    }
    return($l."\n".$_);
}

sub writeWord{
    my (%h) = @_;
    my $front = "",$back = "";
    for $i (sort { $b <=> $a } keys %h){
    split(" ",$i);
    $front .= "_".$phones[$h{$i}-2] if $i ne " 0";
    last;
#   printf "\nwriteWord: i$i h $h{$i} p $front"
    }
    return("$front");
}

sub readOneWord{
    my ($numgroup) = @_;
    $aa = "";$tt = "";
    for $g (0 .. $numgroup - 1){
    $_ = <IN>;($numunits) = split;#/(\d+) (\d+)/;
#    printf "\nnumunits $numunits " if $debug > 2;

    my $act = "",$tar = "";
    undef %ahash;undef %thash;
    for $u (0 .. $numunits - 1){
        $_ = <IN>;
        ($actual,$target) = split;# /(\S+) (\S+)/;
#print "\nactual: $actual, target: $target";
        $ahash{$actual." $u"} = $u+1 if $actual > $threshhold; # creates a hash for ordering
        $thash{$target." $u"} = $u+1 if $target > $threshhold;
    }
    my $t = writeWord(%thash);
    my $a = writeWord(%ahash);
#   $aa .= " " if $a =~ /-/;
#   $tt .= " " if $t =~ /-/;
    $aa .= "$a";
    $tt .= "$t";
    }
    $aa =~ s/_(-[a-z]+)_([a-z]+)/_$2_$1/;
    $tt =~ s/_(-[a-z]+)_([a-z]+)/_$2_$1/;

    $aa =~ s/^_//g;
    $tt =~ s/^_//g;
    $aa =~ s/_/,/g;
    $tt =~ s/_/,/g;
    return($aa,$tt);
}

# for each example:
#  <I total-updates> <I example-number>
#  <I ticks-on-example> <I num-groups>
#  for each tick on the example:
#    <I tick-number> <I event-number>
#    for each WRITE_OUTPUTS group:
#      <I num-units> <B targets?>
#      for each unit:
#        <R output-value> <R target-value>

open(IN,"< $ARGV[0]") if $ARGV[0] !~ /.gz/;
open(IN,"gunzip -c $ARGV[0] |") if $ARGV[0] =~ /.gz/;

readWords;
while(<IN>){
    ## this is the example loop, should end with #totalupdate #numexamptested
    ($totalupdate,$exnum) = split;#/(\d+) (\d+)/;
    $_ = <IN>;($tickexample,$numgroup) = split;#/(\d+) (\d+)/;
    printf "\ntotalupdate $totalupdate exnum $exnum"  if $debug > 2;
    printf "\ntickexample $tickexample numgroup $numgroup"  if $debug > 2;

    my $targetsent = "",$actualsent = "";
    for $tick (0 .. $tickexample - 1){
    $_ = <IN>;($ticknum,$eventnumber) = split;#/(\d+) (\d+)/;
    printf "\nticknum $ticknum eventnumber $eventnumber" if $debug > 2;

    ($a,$t) = readOneWord($numgroup);

    $targetsent .= "$t ";
#    print "$targetsent\n";
    $actualsent .= "$a ";
    }
    
#    $name = "name: { filler }";
    $name = getNextMes;
    ($targetsent) = $name =~ /name:\{ +(\S+ .+?) +(\}|\#)/ if $targetsent !~ /\S/;
#    $targetsent = "$targetsent per per " if $targetsent !~ /per *$/;


    printf "\n$name";
    printf "\ntar: $targetsent\nact: $actualsent";

    $lagon-- if $lagon > 0;
    $lagon = $lag if $name =~ /\#p/ && $lag > 0;
}
