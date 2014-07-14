#crée par: PHAN Trong Tien
#date: 12/05/2014
#===========================================
package Modules::Structure;
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
sub TraitementUnitex
{
    my ($file_source,$tool_unitex,$main_graph,$my_unitex) = @_;
    my @dico_unitex = @{$_[4]};
    my $dir_temp = $_[5];
    my $file_ex = $dir_temp."/test.txt";
    my $dir_snt = $file_source;
    $dir_snt =~ s/\.txt/_snt/;

    my $text_snt = $file_source;
    $text_snt =~ s/\.txt/.snt/;
   
    my $sentence_graphe = $my_unitex."/Graphs/Preprocessing/Sentence/Sentence.grf";
    my $replace_graphe =  $my_unitex."/Graphs/Preprocessing/Replace/Replace.grf";

    my $main_graphe_fst2 = $main_graph;
    $main_graphe_fst2 =~  s/\.grf/.fst2/;
   
    my $sentence_graphe_fst2 = $sentence_graphe;
    $sentence_graphe_fst2 =~  s/\.grf/.fst2/;
   
    #my $replace_graphe_fst2 = $replace_graphe;
    #$replace_graphe_fst2 =~  s/\.grf/.fst2/;
    #création du répertoire du texte du texte traité s'il n'existe pas déjà
    if(not -d "$dir_snt")
    {
        mkdir($dir_snt);
    }
    #Normalise
    system("\"$tool_unitex\" Normalize \"$file_source\" -r \"$my_unitex/Norm.txt\" -qutf8-no-bom > $file_ex");
    #sentence: copilation du graph, aplllaissement and appliquant
    system("\"$tool_unitex\" Grf2Fst2 \"$sentence_graphe\" -y --alphabet=\"$my_unitex/Alphabet.txt\" -d \"$main_graph/Graphs\">> $file_ex");
    system("\"$tool_unitex\" Flatten \"$sentence_graphe_fst2\" --rtn -d10 -qutf8-no-bom >> test.txt");
    system("\"$tool_unitex\" Fst2Txt -t \"$text_snt\" \"$sentence_graphe_fst2\" -a\"$my_unitex/Alphabet.txt\" -M >> $file_ex");
    #replace: compilation du graphe
    #system("$tool_unitex Grf2Fst2 $replace_graphe -y --alphabet=$my_unitex/Alphabet.txt >> test.txt");
    #system("$tool_unitex Flatten $replace_graphe_fst2 --rtn -d10 -qutf8-no-bom >> test.txt");
    #system("$tool_unitex Fst2Txt -t\"$text_snt\" $replace_graphe_fst2 -a$my_unitex/Alphabet.txt -R >> test.txt");
    #en utilisant le tool Flattent Ce programme prend une grammaire .fst2 en paramètre, et essaye de la transformer en un transducteur à états finis
    #applatissement du graphe
    #system("$tool_unitex Flatten $main_graphe_fst2 --rtn -d10 -qutf8-no-bom >> test.txt");
    #Decoupage en token
    system("\"$tool_unitex\" Tokenize \"$text_snt\" -a \"$my_unitex/Alphabet.txt\" -qutf8-no-bom >> $file_ex");
    # Application du dictionnaire français
    #print $dico_unitex."\n";
    my $dico = "";
    foreach (@dico_unitex)
    {
        $dico .= $_." ";
    }
    system("\"$tool_unitex\" Dico -t\"$text_snt\" -a \"$my_unitex/Alphabet.txt\" \"$dico\" -qutf8-no-bom >> $file_ex");   
    #system("$tool_unitex Dico -t\"$text_snt\" -a $my_unitex/Alphabet.txt $dico_unitex");
    #system("$tool_unitex Dico -t\"$text_snt\" -a $my_unitex/Alphabet.txt /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/dela-fr-public.bin");
    #system("$tool_unitex Dico -t\"$text_snt\" -a $my_unitex/Alphabet.txt /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Toponyme_Departement_France_FR_utf8.bin");
    #system("/Users/phantrongtien/Documents/Unitex/UnitexToolLogger Dico -t/Volumes/Data/Code/Java/VESPA/data/temp/temp.snt -a/Users/phantrongtien/unitex/French/Alphabet.txt /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/dela-fr-public.bin /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Communes_France_FR_utf8.bin /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Toponyme_Departement_France_FR_utf8.bin /Volumes/MacOS/Users/phantrongtien/unitex/French/Dela/Delaf_Toponyme_Region_France_FR_utf8.bin");
    #correct erreue
    system("\"$tool_unitex\" SortTxt \"$dir_snt/dlf\" -l\"$dir_snt/dlf.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    system("\"$tool_unitex\" SortTxt \"$dir_snt/dlc\" -l\"$dir_snt/dlc.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    system("\"$tool_unitex\" SortTxt \"$dir_snt/err\" -l\"$dir_snt/err.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    system("\"$tool_unitex\" SortTxt \"$dir_snt/tags_err\" -l\"$dir_snt/tags_err.n\" -o\"$my_unitex/Alphabet_sort.txt\" -qutf8-no-bom >> $file_ex");
    #compilation du graphe
    system("\"$tool_unitex\" Grf2Fst2 \"$main_graph\" -y --alphabet=\"$my_unitex/Alphabet.txt\" >> $file_ex");
    # Application du graphe
    system("\"$tool_unitex\" Locate \"-t$text_snt\" \"$main_graphe_fst2\" \"-a$my_unitex/Alphabet.txt\" -L -M --all -b -Y -qutf8-no-bom>> $file_ex");
    # Génération du texte annoté
    system("\"$tool_unitex\" Concord \"$dir_snt/concord.ind\" -m \"$file_source\" -qutf8-no-bom >> $file_ex");
    return $file_source;
}
#=========================================Fin module
1;