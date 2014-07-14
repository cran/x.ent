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
sub CreateRelation
{
	#stock results
	my %final = ();
	
	my (@phrase) = @{$_[0]};
	my ($root) = ${$_[1]};
	my (@links) =  @{$_[2]};
	my ($neg) =  ${$_[3]};
	my (%result) = %{$_[4]};
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
	#nouvelle version	
	foreach(@phrase)
	{
		#init values of parametres
		my $value_root = "";
		#get paragraph
		my $line = $_;
		#get value of root
		my $reg = "{S}<$root>(.*?)<\/$root>";
		#my $reg1 = "<$root>([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ\\s]*)<\/$root>";
		my $reg1 = "<$root>([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ\\s]+[\\w\\s]*)<\/$root>";
		if($line =~ /($reg|$reg1)/g)
		{
			if($1 =~ /<$root>(.*?)<\/$root>/)
			{
				$value_root = $1;	
			}
		}
		#get value and build relation
		if(length( $value_root || ''))
		{
			my $str = "";
			#cut every sentences
			for(@links)
			{
				my $rel = $_;
				my @tags = split /[:]/,$rel;
				#if the relation is 2
				if(scalar(@tags) eq 2)
				{
					my @sentences = split m/(?<=[.!?;])/m, $line;
					for(@sentences)
					{
						my $value_neg = 1;
						my $sens = $_;
						if($sens =~ /<$neg>(.*?)<\/$neg>/gi)
						{
							$value_neg = 0;
						}
						
						my $reg = "<$tags[1]>(.*?)<\/$tags[1]>";
						my %r = Modules::Entite::ExtractDataLocal($sens,$reg);
						#Modules::Utils::PrintHash(%r);
						foreach my $v (keys %r)
						{
							if(lc($value_root) ne lc($v))#vérifier si les entités sont en doublons
							{
								$str = $rel.":\$\$:".$value_root.":".$v.":".$value_neg;
								$final{$str}++;
								$str= "";
							}
						}
						
					}
				}
				#si the relation is 3
				if(scalar(@tags) eq 3)
				{
					my $value_root1 = "";
					#get value of root
					my $reg = "{S}<$tags[1]>(.*?)<\/$tags[1]>";
					#my $reg1 = "<$root>([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ\\s]*)<\/$root>";
					my $reg1 = "<$tags[1]>([A-ZÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝ\\s]+[\\w\\s]*)<\/$tags[1]>";
					my @phrase1 = Modules::Structure::TransformerData($line,$tags[1]);
					foreach(@phrase1)
					{
						my $line1 = $_;
						if($line1 =~ /($reg|$reg1)/g)
						{
							if($1 =~ /<$tags[1]>(.*?)<\/$tags[1]>/)
							{
								$value_root1 = $1;	
							}
						}
					if(length( $value_root1 || '') and (lc($value_root1) ne lc($value_root)))
						{
							my @sentences = split m/(?<=[.!?;])/m, $line1;
								for(@sentences)
								{
									my $value_neg = 1;
									my $sens = $_;
									if($sens =~ /<$neg>(.*?)<\/$neg>/gi)
									{
										$value_neg = 0;
									}
									my $reg = "<$tags[2]>(.*?)<\/$tags[2]>";
									my %r = Modules::Entite::ExtractDataLocal($sens,$reg);
									foreach my $v (keys %r)
									{
										if(lc($value_root1) ne lc($v))#vérifier si les entités sont en doublons
										{
											$str = $rel.":\$\$:".$value_root.":".$value_root1.":".$v.":".$value_neg;
											$final{$str}++;
											$str= "";
										}
									}
										
								}
						}		
					}
				}
				#si the relation is greater than 3
				if(scalar(@tags) > 3)
				{
					print "The tool doesn't support create a relation that is greater than 3.\n"; 
				}
			}	
		}
	}
	return %final;
}
#=========================================Fin module
1;