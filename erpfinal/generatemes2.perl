#!/usr/bin/perl

$arguments = "@ARGV";

$numberOfUtterances = 20;
if ($arguments =~ /-n (\d+)/){
    $numberOfUtterances = $1;
    srand($numberOfUtterances);  # randomize on size by default
}

if ($arguments =~ /-d/){
    $debug = 1;
}

if ($arguments =~ /-s (\d+)/){
    srand($1); 
}

$envfile = $1 if $arguments =~ /(env\S+)/;
open(ENV,$envfile);
while(<ENV>){
    if (/^([^\{]+)\{/){
    $sectionName = $1;
    while(<ENV>){
        last if /^\}/;
        $section{$sectionName} .= $_;
    }
    }
}

# default conj put 1 clause first
$firstconj = "nothing";

## create the list of constructions
for $line (split(/\n/,$section{"constructions"})){
    $line =~ s/\s+/ /g;
    if (($type,$rest) = $line =~ /^(\S+?):\s+(.+)/){
    # store construction
    $type =~ s/noemb//g; # ignore this since it is used only by makeembedded
    $type = uc $type;
    $constructionStore{$type} = $rest;
    push @constructionOrder,$type;
#    print "$type $constructionStore{$type}\n";

    ## make array for selecting structure according to frequency
    if ($type =~ /^MESS(\d+)/){
        $connum = $1;
        $freqCon = 10;
        $freqCon = $1 if ($rest =~ /FREQ=(\d+)/);
          for $i (1 .. $freqCon){ #should run from 1, not 0!
            push @selectConst,$connum;
            }
        }
    # figure out default order of conjunctions
    $firstconj = $1  if $rest =~ /(\S+?,and,\S+)/;

    if ($type =~ /SENT(\d+)\S*/){       ## find and replace concepts
        $connum = $1;
        push @constructionList,$connum;
        for $rc ($constructionStore{"MESS$connum"} =~ /(\d[XYZ]=CONCEPT\d+)/g){
        ($r,$c) = split(/=/,$rc);
        $constructionStore{"MESS$connum"} =~ s/$c/$constructionStore{$c}/g;

        ## replace in form
        $f = $c;$f =~ s/CONCEPT/FORM/;
        $rr = $constructionStore{$f};
        $rr =~ s/(\d)/$r$1/g;
        $constructionStore{"SENT$connum"} =~ s/$r/$rr/g;
        }
    }
    }
}

## import the categories
for $line (split(/\n/,$section{"categories"})){
    $line =~ s/\s+/ /g;
    $line =~ s/\s+$//;
#    print "\n".$line;
    if ($line =~ /#event/){
	$eventSemanticsStart = 1;
	next;
    }
    if ($line =~ /#role.+/){
	$roleList = $line;
    }
    next if $line =~ /^#/;
    if ($eventSemanticsStart != 1){
	if (($cata,$catb) = $line =~ /^([^\#]+?)\#(\S+)/){                   # combined categories
#       printf "\ncat $cata $catb @{$categoryList{$1}} ";
	    @{$categoryList{$cata."#".$catb}} = @{$categoryList{$cata}};
	    push @{$categoryList{$cata."#".$catb}},@{$categoryList{$catb}};
	}else{                                                              # individual categories
	    ($category, $tag, $clist) = split(/ /,$line,3);
	    $clist = uc $clist;
	    
	    if ($clist =~ /\|(.*)\|(\d+)/){
		$stuff = $1;
		$multiplier = $2;
		for $k (1..$multiplier){
		    $clist .= " $stuff";
                }
		$clist =~ s/$stuff//;
		$clist =~ s/\|//g;
		$clist =~ s/\d+//g;
		$clist =~ s/\s+//;
            }
# print "\n$clist\n";
	    
	    @{$categoryList{$category}} = split(/ /,$clist);

        # appears at top to label units
	    $modelUnitsList{"semantics"} .= " :$tag $clist";

	    for $i (@{$categoryList{$category}}){
		$unseen{$i} = 1; #used later
#       $unseen{$category} = 1; #used later
	    }
	}
    }else{                                                       # create eventsemantic categories
	($category, $clist) = split(/ /,$line,2);
	@{$categoryList{$category}} = split(/ /,$clist);
	$modelUnitsList{"eventsemantics"} .= " $clist";
    }
}

#print("constraints");
for $line (split(/\n/,$section{"constraints"})){
#    print($line);
    $line =~ s/ +$//;
    @p = split(/ /,uc($line));
    $verb=shift @p;
    $role =shift @p;
    $numwords = $#p;
    @pp = @p;
    if ($line =~ /=/){
	@pp = ();
	for $i (@p){
	    if ($i =~ /=/){
		($w,$freq)=split(/=/,$i); 
		foreach $j (0..$freq){
		    push @pp,$w;
		}
	    }
	}
	$i = 0;
	while ($#pp < 100){
	    $w = $p[$i % $#p];
	    push @pp,$w if $w !~ /=/;
	    $i++;
	}

    }
    @{ $constraints{$verb} } = @pp;
    if ($debug){
	print("\nconstraint $verb @{ $constraints{$verb} }");
    }
}
#print("@{ $constraints{SIP} }");
#printf "done\n";

if ($debug){
    for $i (@constructionOrder){
    printf "\ncon $i $constructionStore{$i}";
    }
    for $i (sort keys %categoryList){
    printf "\ncat $i @{$categoryList{$i}}";
    }
}


## remove duplicates in lists 
$modelUnitsList{"semantics"} = " ".$modelUnitsList{"semantics"}."  ";
while($modelUnitsList{"semantics"} =~ / (\S+) (.+?) \1 /){
    $modelUnitsList{"semantics"} =~ s/ (\S+) (.+?) \1 / $1 $2 /g;
    $modelUnitsList{"semantics"} =~ s/ (\S+) \1 / $1 /g;
}
$modelUnitsList{"semantics"} =~ s/\s+/ /g;
$modelUnitsList{"lexicon"} = lc $modelUnitsList{"semantics"};
$unseen{"DETNOPRO"} = 0; # exception
for $c (keys %constructionStore){
    if ($c =~ /(CONCEPT|MESS)/){
    $messTemplate = $constructionStore{$c};
#   printf "\nchecking $c $messTemplate";
    $conceptonly = $messTemplate;
    $conceptonly =~ s/\d+[A-Z]==\S+/ /g;
    $conceptonly =~ s/\d+[A-Z]=/ /g;
    $conceptonly =~ s/(\#|_\d+|,)/ /g;
    $conceptonly =~ s/-\d+/ /g;
    $conceptonly =~ s/FREQ=\S+/ /g;
    $conceptonly =~ s/LABEL=\S+/ /g;

    for $i (split(/ +/,$conceptonly)){
        if (!$checked{$i}){
        $checked{$i}++;
#       printf "\n concept $i @{$categoryList{$i}}";
        for $i (@{$categoryList{$i}}){
            $unseen{$i} = 0;
        }
        }
    }
    }
}
#printf "\nunseen ";
for $c (keys %unseen){
    if ($unseen{$c} == 1){
    $c = "[.]" if $c eq ".";
#   printf "\nunseen $c $modelUnitsList{semantics}";
    $modelUnitsList{"semantics"} =~ s/ $c / /g;
    }
}
$modelUnitsList{"semantics"} =~ s/(:\S+ )+(:\S+)/ $2/g;
$modelUnitsList{"semantics"} =~ s/\s+/ /g;

$modelUnitsList{"eventsemantics"} = " ".$modelUnitsList{"eventsemantics"}."  ";
while($modelUnitsList{"eventsemantics"} =~ / (\S+) (.+?) \1 /){
    $modelUnitsList{"eventsemantics"} =~ s/ (\S+) (.+?) \1 / $1 $2 /;
    $modelUnitsList{"eventsemantics"} =~ s/ (\S+) \1 / $1 /;
}
$modelUnitsList{"eventsemantics"} =~ s/\s+/ /g;



sub verbconstraints{
    my ($role,$action,$m) = @_;


    @lis = @{ $constraints{$action} };
    if ($debug){
	printf("\nverb constraint $action @lis $#lis");
    }

    if ($#lis > 0){
#	printf("\nold $m");
#	print($action);
#	print(@lis);
	$r = int(rand(1000))%($#lis+1);
	$replaceword = $lis[$r];

	$m =~ s/$role=([^,]+)/$role=$replaceword/;
	if ($debug){
	    printf("\nnew $m $r ");
	}
    }
    return("$m");
}

printf "\n#semantics: NULL$modelUnitsList{semantics} ";
printf "\n#lexicon: null$modelUnitsList{lexicon} ";
printf "\n#eventsemantics:$modelUnitsList{eventsemantics} ";
printf "\n$roleList";
#printf "\n#cmdlist: $arguments conjorder $firstconj";
printf "\n#cmdlist: $arguments";
printf "\n##end";
$pos = 0;


printf "\n#number $numberOfUtterances ";

#generate messages
for $i (0 .. $numberOfUtterances - 1){
    $selectedConst = $selectConst[int(rand($#selectConst+1))];
    $messTemplate = $constructionStore{"MESS".$selectedConst};
#    printf "\n$messTemplate $sentForm ";

    $messTemplate =~ s/FREQ\S+//g;
    @mesphrase = split(/ +/,$messTemplate);
    $newmess = "";
    undef %usedwords;
    for $ph (@mesphrase){                          # for each phrase
	($role,$conceptString) = split(/=/,$ph,2);
#   printf "\nrole $role $conceptString $ph";
        #   printf "\nph $ph $role conc @conceptList";
	$newmess .= " $role=";

	if ($debug){
	    printf "\n $role $conceptString conceptList $#conceptList cat $#{$categoryList{$conceptList[0]}}";
	}
#   if ($#conceptList > -1 && $#{$categoryList{$conceptList[0]}}){
    
	$newconceptlist = $conceptString;
	@conceptList = split(/,/,$conceptString);
	
	for $cn (0 .. $#conceptList){              # pick random word for concept
	    $c = $conceptList[$cn];
	    if ($c =~ /_(\d+)/){
		$prob = $1;
		$newconceptlist =~ s/$c// if (rand(100) > $prob);  # deal with adjective probabilities
		$newconceptlist =~ s/_\d+//;
		$c =~ s/_\d+//g;
	    }
	    
	    if ($#{$categoryList{$c}} > -1){
		$tries = 0;
		do{                                                    # don't allow repeated words 
		    $randomWordFromCat = $categoryList{$c}[int(rand($#{$categoryList{$c}}+1))];
		    $tries++;
#           printf "\ntrying $tries $randomWordFromCat $usedwords{$randomWordFromCat}";
		}while($usedwords{$randomWordFromCat} && $tries < 5);
#                    print "\nrand $c $randomWordFromCat <- @{$categoryList{$c}}";
	    }else{
		$randomWordFromCat = $c;
	    }
	    $newconceptlist =~ s/$c/$randomWordFromCat/;
	    $usedwords{$randomWordFromCat}++ if $#{$categoryList{$c}} > 4;
	}
	$newmess .= $newconceptlist;
#   printf "\nrole $role $newconceptlist ";
    }
    ## now adjust
    for $pair ($newmess =~ /(\d+[A-Z]==\d+[A-Z])/g){  # make bindings between clauses
	($role,$orole) = split(/==/,$pair);
#   printf "\nrole $role orole $orole ";
	
	$newmess =~ s/$orole=(\S+?),PRN/$orole=$1,THE/;  # no pronoun heads
	$newmess =~ s/$orole=(\S+) (.+?)$role==$orole/$orole=$1 $2$role=$1/;   # copy concept from head role to gap role
    }   

    # put pronoun stuff here
    $newmess =~ s/ NOPRO=//g;

    $newmess .=" ";
    $newmess =~ s/, / /g;
    $newmess =~ s/^ +//g;
    $newmess =~ s/,\w+,PRN/,,PRN/g; #remove adjective when there is a pronoun
#    $newmess =~ s/,,/,/g;    

    if ($newmess =~ /0A=([^ ,]+)/){
	$newmess = verbconstraints("0Y",$1,$newmess);
    }

    $_ = $newmess;
    eval $section{"beforesent"};    
    $newmess = $_;
        
    printf "\nmess: $newmess";
    printf " LABEL=MESS$selectedConst" if $newmess !~ /LABEL=/;
    printf " const$selectedConst" if $debug;

    # create base sentence form from message
    printf "\nsent: ";
    $sentForm = " ".$constructionStore{"SENT".$selectedConst}." ";
    for $ph (split(/ /,$newmess)){
	$ph =~ s/-\d+,//g;
	($role,$rest) = $ph =~ /(\d[A-Z])=(\S+)/;
	@phstuff = split(/,/,$rest);
	$sentForm =~ s/ $role/ $role=$role/ if $role =~/\d+[A-Z]/;
	for $c (0 .. $#phstuff){
#       printf "\n $role$c $phstuff[$c]";
	    $sentForm =~ s/$role$c/$phstuff[$c]/g;
	    $sentForm =~ s/ \=/\=/g;
	    $sentForm =~ s/\s+/ /g;
	}
    }
    printf " ".lc($sentForm);

}
