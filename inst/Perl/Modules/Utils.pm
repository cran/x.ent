#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Utils;
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
my $fin = "</fin>";
my $END = "end";
my $PHASE = "phase";
sub PrintHash
{
	my %hash = @_;
	foreach my $key (keys %hash)
	{
		my $value = $hash{$key};
		print "$key:$value\n";
	}
}
sub PrintArray
{
	my @array = @_;
	foreach(@array)
	{
		print $_."\n\n";
	}
}
sub PrintHashOfArray
{
	my %hash = @_;
	foreach my $key (keys %hash)
	{
		print "$key:";
		my $values = $hash{$key};
		foreach my $nam ( @{$values} ) 
		{
			print  $nam."\t";
		}
	print  "\n";
	}	
}
#vérifier la taile de hash 
sub SizeHash
{
	my (%hash) = @_;
	my @keys = keys %hash;
	my $size = @keys;
	return $size;
}
#tranformer un hash devenu une chaine que chaque key sépare par deux points ":"
sub HashToString
{
	my %data = @_;
	my $temp = "";
	foreach my $key (keys %data)
	{
		$key =~ s/[+*?!]//gm;
		$key = Trim($key);
		if(length($key)>1)
		{
			$temp = $temp.$key.":";
		}
	}
	return $temp;
}
sub HashToArray
{
	my %data = @_;
	my @result = ();
	foreach my $key (keys %data)
	{
		#check empty
		if(length($key)>0)
		{
			push(@result,$key);	
		}
	}
	return @result;
}
#normaliser les données
sub Normalize
{
    my($str) = @_;
    $str =~ s/\n//gm;
    $str =~ s/\s\s+/ /gm;
    return $str;
}
#function readfile, return une variable type SCALAR
sub ReadFile
{
	my($fn) = @_;
	my $data = "";
	my $line = "";
	my $prev_line = "";
	open(INPUT,'<:raw:encoding(UTF8)',$fn) || die "can't read this file: $fn\n";
	while(<INPUT>)
	{
		$line =~ s/\s\s+/ /gm;
		$line = Trim($_);
		#calcule pour ajouter le point (.)
		if((length($prev_line) > 0) and (length($line) eq 0))
		{
			#check la phrase précédent, si en existant un point, ne pas ajouter 
			if(not $prev_line =~ /\,$|\:$|\.$/)
			{
				$data .= ". ";
			}
		}
		if(length($line) > 0)
		{
			#vérifier si debut une phase est une caractère majuscule => ne pas combiner avec la phase précédente
			if($line =~/^[A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]+/g)
			{
				#changement 17/07/2014
				#print "$line\n";
				if($data eq  "")
				{
					$data = $line;
				}
				else
				{
					$data .= "{S}".$line;	
				}
			}
			else # voilà saute de linge, on vais combiner avec une phrase précédent
			{
				if($data eq  "")
				{
					$data = $line;
				}
				else
				{
					$data .= " ".$line;	#j'ai déjà changé {S}		
				}
			}
		}
		$prev_line = $line;
	}
	$data .= "$fin";
	$data =~ s/\s\s+/ /gm;
	close(INPUT);
	return $data;
}
#la function a deux paramètres: nom de fichier sortir, fichier + entité, hash contient des valeur
sub SaveFile
{
	my ($file_output,$data) = @_;
	open(OUTPUT,'>:raw:encoding(UTF8)',$file_output) || die "Can't open this file: $file_output";
	print OUTPUT $data."\n";
	close(OUTPUT);
}
sub AvoidPhase
{
	my ($data,$fichier) = @_;
	my %dico = %{$_[2]};
	my %tag_dico = %{$_[3]};
	my %phases = Modules::Dico::LoadDicoNameAndSyn($fichier);
	#chaque phrase a le format: debut -> stop(tag , phrase, end of document,)
	#foreach my $key (keys %phases)
	#{
	#	$data =~ s/$key(.*?)\.//g;
	#}
	foreach my $key (keys %phases)
	{
		my @arr = @{$phases{$key}};
		if(scalar(@arr) >= 2)
		{
			#quand le fini a pluisieurs choix: ex phase, end ...
			#priorité élément primier
			for(my $i=1;$i<scalar(@arr);$i++)
			{
				if($arr[$i] eq $PHASE)
				{
					if($data =~ m/$key(.*?)\./g)
					{
						$data =~ s/$key(.*?)\.//g;
						print "$key(.*?)\.\n"; 	
					}
				}elsif($arr[$i] eq $END)
				{
					if($data =~ m/$key(.*?)$fin/g)
					{
						$data =~ s/$key(.*?)$fin//g;
						print "$key(.*?)$fin\n"; 	
					}
				}else
				{
					my %plante = ();
					#trouver dans dictionnaire
					for my $tag (keys %tag_dico)
					{
						if($tag_dico{$tag} eq $arr[$i])
						{
							my %dico_tag = %{$dico{$tag}};
							#Modules::Utils::PrintHashOfArray(%dico_tag);
							for my $elm (keys %dico_tag)
							{
								my @arr_elm = @{$dico_tag{$elm}};
								my $reg = "";
								$reg = $arr_elm[0];
								for(my $i=1;$i<@arr_elm;$i++)
								{
									$reg .= "|".$arr_elm[$i];
									my @myarray = ($data =~ /($reg)/gi);
									foreach (@myarray)
									{
										if (CheckUpCase($_) eq 1)
										{
											$plante{$_}++;
										}
									}		
								}
							}	
						}
					}
					#obtenir la posision plus proche avec le position de key
					#en trouvant début clé
					
					my $pos_key = index($data,$key);
					my $tag_fin = "";
					my $min = "999999999999999";
					for my $p (keys %plante)
					{
						my $pos = index($data,"{S}".$p,$pos_key);
						#comparer pour prend la position plus porche
						if($pos > $pos_key)
						{
							if($pos < $min)
							{
								$min = $pos;
								$tag_fin = $p; 
							} 
						}
					}
					#modifier le $key si il a des caractère spéciale
					$key =~ s/\./\\\./;
					if($tag_fin ne "")
					{
						if($data =~ m/$key(.*?)($tag_fin)/g)
						{
							$data =~ s/$key(.*?)($tag_fin)/{S}$2/g;
							print "$key$1$tag_fin\n";
							last;
						}
					}		
				}
			}
		}
		else
		{
			#défaut en enlévant du mot debut au mot point (.))
			$data =~ s/$key(.*?)\.//g;
		}
	}
	return $data;
}
sub ReplaceWord
{
	my ($data, $file) = @_;
	my %mots_alternative = Modules::Dico::LoadDicoNameAndSyn($file);
	#$data =~ s/([Saint|saint|SAINT|St])\- ([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)/$1-$2/g;#Saint- XXXX
	$data =~ s/([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)\- ([a-zàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)/$1$2/g;#supprimer la caractère est - si un mot a été coupé 
	$data =~ s/([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)\- ([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]+)/$1-$2/g;
	$data =~ s/([A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ]+)\-([0-9]+)/$1 - $2/g;#Party-21
	$data =~ s/œ/oe/gi;
	$data =~ s/’/'/g;
	#replate le caractère ’ -> '
    for my $key (keys %mots_alternative)
	{
		my @elements = @{$mots_alternative{$key}};
		foreach(@elements)
		{
			$data =~ s/[\\\/\[\]\{\}.,?;!\(\)\" \t]+($_)[\\\/\[\]\{\}.,?;!\(\)\" \t]+/ $key/gi;
		} 
	}	
	return $data
}
sub CheckConfict
{
	#extraction tous les donnés dans les balises
	#et appliquer un algorithme pour trouver un mots est dans un autre mots
	my ($curr_data,$old_data) = @_;
	my (%tag_dico) = %{$_[2]};
	my %tag_unitex = %{$_[3]};
	#Modules::Utils::PrintHash(%tag_dico);
	#couper chaque phrase
	my @line_curr_data = split(/\.{S}/,$curr_data);
	my @line_old_data = split(/\.{S}/,$old_data);
	#print scalar(@line_curr_data)."\t".scalar(@line_old_data)."\n";
	my $result = "";
	my $total_line = (scalar(@line_curr_data) > scalar(@line_old_data))?scalar(@line_old_data):scalar(@line_curr_data);
	for(my $n_line = 0; $n_line<$total_line;$n_line++)
	{
		#print $line_curr_data[$n_line]."\n";
		#print $line_old_data[$n_line]."\n";
		my %test = ();
		foreach my $key (keys %tag_dico)
		{
			my $reg = "<$tag_dico{$key}>(.*?)<\/$tag_dico{$key}>";
			%test = Modules::Entite::ExtractDataParagraphe($line_curr_data[$n_line],$reg,$line_old_data[$n_line],\%test);
		}
		foreach my $key (keys %tag_unitex)
		{
			my $reg = "<$key>(.*?)<\/$key>";
			%test = Modules::Entite::ExtractDataParagraphe($line_curr_data[$n_line],$reg,$line_old_data[$n_line],\%test);
		}
		#vérifier le première, le format de donnée: <NUI>entre 1 et 10 pucerons <b>pucerons</b> pucerons par plante</NUI> ou <STA>diamètre de pomme <p>pomme</p> pomme supérieur à 25 cm</STA> ....
		my @temp = ();
		foreach my $key (keys %test)
		{
			$key =~ s/[\(\)+*?!]//gm;
			#$key =~ s/[^A-Za-z0-9A-Za-zÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïñòóôõöùúûüýÿ\%\-\.\,\/]//gm;
			if(length(Modules::Utils::Trim($key)) > 1 )
			{
				push @temp,$key;	
			}
		}
		#print "$line_curr_data[$n_line].\n";
		#print "pommes".$test{"pommes"}."\n";
		#Modules::Utils::PrintHash(%test);
		#Modules::Utils::PrintArray(@temp);
		my $items = scalar(@temp);
		for(my $i = 0;$i < $items - 1; $i++)
		{
			for(my $j = $i+1; $j < $items; $j++)
			{
				#print $temp[$i]."\t".$temp[$j]."\n";
				if(($temp[$i]=~m/$temp[$j]/) or ($temp[$j]=~m/$temp[$i]/))
				{
					my $court = 0;
					my $long = 0;
					#choisir le mot plus court et le plus long
					if(length($temp[$i]) > length($temp[$j]))
					{
						$long = $i;
						$court = $j;
					}
					else
					{
						$long = $j;
						$court = $i;
					}
					if( exists $test{$temp[$court]} and exists $test{$temp[$long]})
					{
						#print $temp[$i]."\t".$temp[$j]."\t"."$temp[$court] =>".$test{$temp[$court]}."\t"."$temp[$long] =>".$test{$temp[$long]}."\t"."\n";
						#vérifier le première, le format de donnée: <NUI>entre 1 et 10 pucerons <b>pucerons</b> pucerons par plante</NUI> ou <STA>diamètre de pomme <p>pomme</p> pomme supérieur à 25 cm</STA> ....
						if(($test{$temp[$court]} >= 0) and ($test{$temp[$long]} eq -1))
						{
							#print $temp[$i]."\t".$temp[$j]."\t"."$temp[$court] =>".$test{$temp[$court]}."\t"."$temp[$long] =>".$test{$temp[$long]}."\t"."\n";
							my $reg_del = "<[A-Za-z]+>$temp[$court]<\/[A-Za-z]+> $temp[$court] ";
							#print "$reg_del\n";
							$line_curr_data[$n_line] =~ s/$reg_del//g;
						}
						if(($test{$temp[$court]} >= 0) and ($test{$temp[$long]} >= 0) and ($test{$temp[$court]}>=$test{$temp[$long]}) and ($test{$temp[$court]} + length($temp[$court])<=$test{$temp[$long]}+ length($temp[$long])))
						{
							#print "deux mots sont \"$temp[$i]\": $test{$temp[$i]} et \"$temp[$j]\":$test{$temp[$j]}.\n";
							my $reg_del = "<[A-Za-z]+>$temp[$court]<\/[A-Za-z]+> $temp[$court] ";
							#print $reg_del."\n";
							$line_curr_data[$n_line] =~ s/$reg_del//g;
						}	
					}	
				}
			}
		}
		$result .= $line_curr_data[$n_line].".";
		#print "$line_curr_data[$n_line].\n\n";
	}
	#print $result."\n";
	return $result;
}
# Declare the trim subroutine
sub Trim
{
  my $string = shift;
  $string =~ s/^\s+//;            
  $string =~ s/\s+$//; 
  return $string;         
}
# vérifier le character primier est upper
sub CheckUpCase {
    if ($_[0] =~ /^[A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ]/) {
        return 1;
    }
    else {
        return 0;
    }
}
#le fonction utilise à fichier config, vérifier des colonnes et retourner le nummer de collone 
sub CheckColumn
{
	my ($col_val) = @_;
	my @result = ();
	if(length($col_val) > 0)
	{
		my @arr = split(/[\,]/,$col_val);
		foreach(@arr)
		{
			#si seleument number
			if($_ =~ /^[0-9]+$/)
			{
				push @result,$_;
			}
			else
			{
				my @arr1 = split(/[\..]/,$_);
				if(scalar(@arr1) eq 3)
				{
					if(($arr1[0] =~ /^[0-9]+$/) and ($arr1[2] =~ /^[0-9]+$/))
					{
						if($arr1[0] > $arr1[2])
						{
							my $tg = $arr1[0];
							$arr1[0] = $arr1[2];
							$arr1[2] = $tg;
						}
						for(my $i = $arr1[0];$i <= $arr1[2];$i++)
						{
							push @result,$i;
						}
					}
					else
					{
						push @result,$arr1[0];
						push @result,$arr1[2]; 
					}
				}
			}
		}	
	}
	return @result;
}
#=========================================Fin module
1;