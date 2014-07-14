#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Relation;
$VERSION = v1.0;
use strict;
use warnings;
use Carp; #helps with debugging
use utf8;
use open(IO => ':encoding(utf8)');
binmode(STDERR, ':utf8');
binmode(STDOUT, ':utf8');
binmode(STDIN, ':utf8');# All output will be UTF-8
#=========================================Debut module
sub TransformerData
{
	my ($data,$root) = @_;
	my @final = ();
	my @sentences = split m/(?<=[.!?;])/m, $data;
	my $mark = 0;
	my $reg = "<$root>(.*?)<\/$root>";
	my $phrase = "";
	$mark = 0;
	my $mark_old = 1;
	foreach (@sentences)
	{
		if($_ =~ /($reg)/gi)
		{
			$mark += 1; 
		}
		if($mark > $mark_old)
		{
			push @final,$phrase;
			$phrase = "";
			$mark_old = $mark;
		}
		$phrase .= $_." ";
	}
	if(length($phrase) > 0)
	{
		push @final,$phrase;
	}
	return @final;
}
sub PrintRelation2
{
	my @result = ();
	my @arr1 = @{$_[0]};
	my @arr2 = @{$_[1]};
	my $neg = $_[2];
	my $temp = "";
	my $test = "";
	if(scalar(@arr1) > 0 and scalar(@arr2) > 0)
	{
		foreach(@arr1)
		{
			$temp = $_.":";
			foreach(@arr2)
			{
				
				$test = $temp.$_.":$neg";
				push @result, $test;
			}
		}
	}
	return @result;
}
sub PrintRelation3
{
	my @result = ();
	my @arr1 = @{$_[0]};
	my @arr2 = @{$_[1]};
	my @arr3 = @{$_[2]};
	my $neg = $_[3];
	my $temp = "";
	my $test = "";
	if(scalar(@arr1) > 0 and scalar(@arr2) > 0 and scalar(@arr3))
	{
		foreach(@arr1)
		{
			my $key = $_;
			foreach(@arr2)
			{
				$temp = $key.":".$_.":";
				foreach(@arr3)
				{
					$test = $temp.$_.":$neg";
					push @result, $test;
				}
			}
		}
	}
	return @result;
}
sub CreateRelation
{
	my %final = ();
	
	my (@phrase) = @{$_[0]};
	my (@links) =  @{$_[1]};
	my ($neg) =  ${$_[2]};
	my (%result) = %{$_[3]};
	my %tags = ();
	#prendre tous les tags pour les rélations
	for(@links)
	{
		my @cut = split /[:]/,$_;
		foreach(@cut)
		{
			$tags{$_}++;
		}	
	}
	
	my %result_tag = ();
	my $count = 0;
	my $reg = "";
	#récupérer les rélations
	foreach(@phrase)
	{
		my $line = $_;
		my $value_neg = 1;#en stokant le valeur negatif
		#nagetion
		if(length( $neg || ''))
		{
			if(exists($result_tag{$neg}))
			{
				$count = 0;
				%result_tag = %{$result{$neg}};
				for my $key (keys %result_tag)
				{
					if($count eq 0)
					{
						$reg = $key;	
					}else
					{
						$reg .= "|".$key;
					}
					$count += 1;
				}
				if($line =~ /[’\\\/\[\]\{\},?;!\(\)\" \t]+($reg)[\\\/\[\]\{\},?;!\(\)\" \t]+/i) 
				{
					$value_neg = 0;#si on'a trouvé, le valeur négatif égale 0
				}		
			}
		}
		#trouver chaque tag
		my %result_filter = ();
		for my $tag (keys %tags)
		{
			if(!exists($result{$tag}))
			{
				print "The entity '$tag' doesn't exist in the configuration. Please, check again! \n";
				exit 0;
			}
			%result_tag = %{$result{$tag}};
			my @arr_result = ();
			for my $key (keys %result_tag)
			{
				$key =~ s/[\(\)+*?!]//gm;
				#$key =~ s/[^A-Za-z0-9A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ\%\-\.\,\/ ]//gm;
				#print $key."\n";
				if(length($key) > 1)
				{
					if($line =~ /[’\\\/\[\]\{\}.,?;!\(\)\" \t\>]+($key)[\\\/\[\]\{\},?.;!\(\)\" \t\<]+/gi) 
					{
						push @arr_result,$key;
					}
					if($line =~ /^($key)[\\\/\[\]\{\},?.;!\(\)\" \t]+/gi) 
					{
						push @arr_result,$key;
					}	
				}
			}
			#print scalar(@arr_result)."\n";
			if(scalar(@arr_result) > 0)
			{
				$result_filter{$tag} = \@arr_result;
			}
		}
		##retourner les résultats
		my $str = "";
		for(@links)
		{
			my $tag = $_;
			my @tags = split /[:]/,$tag;
			#si deux relations
			if(scalar(@tags) eq 2)
			{
				my @arr = PrintRelation2(\@{$result_filter{$tags[0]}},\@{$result_filter{$tags[1]}},$value_neg);
				if(scalar(@arr) >0)
				{
					foreach(@arr)
					{
						$str = 	"$tag:\$\$:$_";
						$final{$str}++;
					}
				}
			}
			#si trois relations
			if(scalar(@tags) eq 3)
			{
				my @arr = PrintRelation3(\@{$result_filter{$tags[0]}},\@{$result_filter{$tags[1]}},\@{$result_filter{$tags[2]}},$value_neg);
				if(scalar(@arr) >0)
				{
					foreach(@arr)
					{
						$str = 	"$tag:\$\$:$_";
						$final{$str}++;
					}
				}
			}
			#si des relations sont suppérieur 3
			if(scalar(@tags) > 3)
			{
				print "Le script n'est pas encore le support suppérieur 4 relation\n"; 
			}
			
		}
	}
	return %final;
}
#=========================================Fin module
1;