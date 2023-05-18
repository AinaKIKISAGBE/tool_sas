/* *********************************************************************/
/* *********************************************************************/
/*            ETUDE DE CAS EN STATISTIQUE DECISIONNELLE
/*                       STEPHANE TUFFERY
/*                    EDITIONS TECHNIP - 2009
/* *********************************************************************/
/* *********************************************************************/


/* 15/06/2009 - Version 1.0                                            */


/* *********************************************************************/
/*                 CHAPITRE 1 : DATA MINING DESCRIPTIF
/* *********************************************************************/


/* Le lecteur désireux de tester le présent programme dans un environnement 
Windows doit préalablement télécharger sur le site des éditions Technip :
- le fichier compressé "base_assurance.zip" contenant la table SAS
"assurancetic.sas7bdat"
- le fichier compressé "analyse_donnees.zip" contenant le catalogue
de macros SAS "sasmacr.sas7bcat".

Le fichier "assurancetic.sas7bdat" doit ensuite être décompressé dans
un répertoire de travail qui sera utilisé tout au long de la session SAS.
C'est le fichier de données en entrée du présent programme.

Le catalogue de macros "sasmacr.sas7bcat" doit être décompressé dans un
sous-répertoire appelé "Macros INSEE" du répertoire de travail.

La macro-variable suivante C correspond au chemin menant au répertoire de travail */

/* Exemple sous Windows Vista : */
%LET C=C:\Users\ASK32\Documents\m2\SEGMENTATION;

/* Exemple sous Windows XP : 
%LET C=C:\Documents and Settings\tufferst\My Documents\Data Mining\Etudes de cas\Assurance\Technip;
*/

/* Le sous-répertoire des macros INSEE sera donc : "&C\Macros INSEE". */


/* La macro-variable suivante correspond à la bibliothèque SAS associée
par un LIBNAME au répertoire de travail. */
LIBNAME assuranc "&C" ;
%LET ASSURANC=assuranc ;



/* ==================================================================== */
/* SECTION 1.2
/* ==================================================================== */


/* LECTURE DU FICHIER DE DONNEES */
/* on duplique le fichier dans la work pour travailler sans modifier le fichier initial */
DATA test ;
 SET &ASSURANC..assurancetic ;
RUN ;


/* CREATION D'UN STYLE ODS pour la mise en page des sortie OUTPUT que nous voulons plutôt afficher directement dans un fichier RTF ou PDF ou Word ou HTML, ...*/

ODS PATH fmtccf.templat(update) sashelp.template(read) sashelp.tmplmst(read) sasuser.templat(update) work.templat(update);
PROC TEMPLATE ;                                                                
   DEFINE style Styles.EC ; /* on nomme le style de mise en forme créé " Styles.EC" */                   
      PARENT = styles.RTF ;                                                
      STYLE Header from HeadersAndFooters                                     
         "Controls the header style" /                                        
         JUST = center
         BACKGROUND = GRAYEE ;                                                    
      STYLE rowHeader from HeadersAndFooters                                  
         "Controls the header style" /                                        
         JUST = center
         BACKGROUND = GRAYEE ;       
   END ;
RUN ;


/* OUVERTURE OU CREATION  DU FICHIER ODS (RFT nomé "sas_assurance_descriptif.doc") CONTENANT LES RESULTATS de l'OUTPUT*/
/* et on appelle le style créé en haut "Styles.EC" pour la mise en forme */
/* pour fermer le fichier , on exécutera ODS RTF CLOSE (a la fin du chapitre) */
ODS RTF FILE = "&C\sas_assurance_descriptif.doc" STYLE=Styles.EC ;


/* LISTE DES VARIABLES explicatives que nous souhaitons utiliser */
/* NB: sans la variable d'âge */
%LET var = SINISTRES_NB SINISTRES_mt auto_nb habit_nb rc_nb soins_nb ij_nb gav_nb prev_nb 
epret_nb iard_nb vie_nb sante_nb auto_mt habit_mt rc_mt soins_mt ij_mt gav_mt prev_mt 
eparg_mt iard_mt vie_mt sante_mt tot_sante_mt tot_sante_nb nbtot_primes csp_cl ancien age ;

/*tot1 c_famille_prod c_tot1 c_csp ID;*/

%LET var_quali = c_mtSINISTRE c_nbSINISTRE c_attrib c_mtprev c_mteparg c_nbauto1  
c_nbauto2 c_nbhab1 c_nbHab2 c_nbrc1 c_nbrc2 c_nbSoins c_nbij c_nbgav c_nbvie c_mtiard 
c_mtpro1 c_mtpro2 c_mtpro3 c_mttot_sante c_age c_ancien;

DATA test ;
SET assuranc.assure ;
run;



PROC corresp data=test mca
outc=sortie ;
/* noprint
DIMENS=5; */
tables c_mtSINISTRE c_nbSINISTRE c_attrib c_mtprev c_mteparg c_nbauto1  
c_nbauto2 c_nbhab1 c_nbHab2 c_nbrc1 c_nbrc2 c_nbSoins c_nbij c_nbgav c_nbvie c_mtiard 
c_mtpro1 c_mtpro2 c_mtpro3 c_mttot_sante c_age c_ancien ;

run;

/* representation graphique */
%plotit(datatype=mca, href=0,vref=0);


/* coordonnee sur les n premiers axes factoriels */

PROC corresp data=test 
DIMENS=18  noprint
outc=sortie2 (rename=(_type_=type_obs _name_=id) keep=_name_ _type_ dim1-dim18 contr1-contr18) ;
 
tables id, &var_quali ;

run;

data sortie23;
set sortie2;
where type_obs='OBS';
run;

proc gplot data=sortie23;
plot dim1*dim2;
run; quit;


proc fastclus data=sortie23
summary maxc=50
out=preclasse cluster=preclasse
mean=res_stat
maxiter=50
converge=0.01
delete=5;
var dim1-dim18;
id id;
/*mean=proj_lib.centres
out=proj_lib.partition
cluster=presegmentation;
id ide;*/
run;
proc means data=preclasse mean max min std;
class preclasse;
var distance;
run;

proc cluster  data=res_stat 
method = ward
outtree=arbre;
var dim1--dim18;
run;

PROC UNIVARIATE DATA=arbre NORMAL ;
run;

/* construction du dendogramme*/


proc tree data=arbre out=table(drop=clustname) n=3 graphics;
copy dim1--dim18;
id preclasse;
run;

/* fusion pour remonter les informations de les informations de la preclasse vers la table */
proc sort data=preclasse; by preclasse; run;
proc sort data=table; by preclasse; run;


data preclasse;
merge preclasse(in=a) table(in=b keep=preclasse cluster);
by preclasse;
run;

proc sql;
create table assureb
as select a.*,
b.preclasse,
b.cluster,
b. dim1--dim18
from test a, preclasse b
where a.id = b.id;
quit;

data assureb;
set assureb;
length cluster2 $ 10;
if cluster=1 then cluster1='segment1';
	else if cluster=2 then cluster2='segment2';
		else if cluster=3 then cluster3='segment3';
			else if cluster=4 then cluster4='segment4';
run;

proc means data=assureb min max;
var dim1 dim2;
run;

proc gplot data=assureb;
symbol1 v=dot c=red h=0.4;
symbol2 v=dot c=blue h=0.4;
symbol3 v=dot c=orange h=0.4;
symbol4 v=dot c=green h=0.5;
axis1 order=(-1.1 to 1.4 by 0.2);
axis2 order=(-0.7 to 1.2 by 0.2);
plot dim1*dim2 = cluster2/href = 0 vref=0 haxis=axis2 vaxis=axis1;
run; quit;



data assureb;
set assureb;
length cluster3 $ 10;
if cluster2 in ('segment 2') and c_famille_prod in (' 111') then cluster3='Segment 1';
	else if cluster=2 then cluster2='segment2';
		else if cluster=3 then cluster3='segment3';
			else if cluster=4 then cluster4='segment4';
run;

proc means data=assureb min max;
var dim1 dim2;
run;

proc gplot data=assureb;
symbol1 v=dot c=red h=0.4;
symbol2 v=dot c=blue h=0.4;
symbol3 v=dot c=orange h=0.4;
symbol4 v=dot c=green h=0.5;
axis1 order=(-1.1 to 1.4 by 0.2);
axis2 order=(-0.7 to 1.2 by 0.2);
plot dim1*dim2 = cluster2/href = 0 vref=0 haxis=axis2 vaxis=axis1;
run; quit;


outtree=tabledendogramme
method=ward
ccc pseudo
print=50;
copy preclasse;
var dim1-dim18;
run;

proc tree data=tabledendogramme ncl=4 out=segmhier;
copy preclasse;
run;


symbol1 color=black interpol=join value=dot height=1;
proc gplot data=tabledendogramme;
plot _sprsq_*_ncl_ _rsq_*_ncl_ _PST2_*_ncl_ _PSF_*_ncl_;
run;


proc discrim data=proj_lib.origine2
method=normal
pool=yes
crossvalidate
canonical
out=proj_lib.discrim
outstat=proj_lib.discrim_stat;
class cluster;
var cl_ANNEE depart_residence cl_AGE code_prof situ_fami sexe frac_paiemen cl_CORPO
cl_MAT;
run;


proc logistic data=proj_lib.origine2 descending;
model cl_CORPO = depart_residence cl_AGE code_prof situ_fami sexe nb_voiture
frac_paiemen cl_MAT /link=logit
lackfit ctable pprob = (0 to 1 by 0.025)
rsquare
risklimits
outroc=proj_lib.roc1;
output out=proj_lib.logistic
p=prob
xbeta=xbeta;
run;






/* création des FORMAT SAS à UTILISES */
/* on stocke les formats créés dans la works dans un dossier nomé "FORMATS"*/

PROC FORMAT ;
VALUE age
1     = ' < 30 ans'
2     = '30-40 ans'
3     = '40-50 ans'
4     = '50-60 ans'
5     = '60-70 ans'
6     = ' > 70 ans' ;
VALUE autrevar
0 = '     0 %'
1 = '1 - 10 %'
2 = '11 - 23%'
3 = '24 - 36%'
4 = '37 - 49%'
5 = '50 - 62%'
6 = '63 - 75%'
7 = '76 - 88%'
8 = '89 - 99%'
9 = '    100%' ;
RUN ;


/* CREATION DE dummy variables VARIABLES D'AGE qui sont binaire (9 ou 0) */ 
/* pourquoi multiplié par 9 et non pas simplement multiplié par le nombre de personnes respectif dans les foyer (nbpers_au_foyer)? */
/* pourqoui ne pas laisser en binaire (1 ou 0)*/
DATA test ;
SET test ;
agemoins30 = (age_moyen = 1) * 9 ;
age30a40 = (age_moyen = 2) * 9 ;
age40a50 = (age_moyen = 3) * 9 ;
age50a60 = (age_moyen = 4) * 9 ;
age60a70 = (age_moyen = 5) * 9 ;
ageplus70 = (age_moyen > 5) * 9 ;
RUN ;

/* nous ajoutons la liste des nouvelles variable d'age créé à notre liste des variables explicatives plus haut  que nous souhaitons utiliser */
%LET var_avec_age =
&var agemoins30 age30a40 age40a50 age50a60 age60a70 ageplus70 ;



/* ==================================================================== */
/* SECTION 1.3.1
/* ==================================================================== */

/* -------------------------------------------------------------------- */
/* ACP avec l'âge
/* -------------------------------------------------------------------- */
/* nous avons utilisé la procedure PRINCOMP de SAS pour l'ACP qui est sa procédure usuelle.
Si on désire faire des ACP avec rotation orthogonales ou obliques, on va utiliser la procedure FACTOR et elle est plus lente */


PROC PRINCOMP DATA=test n=4 OUT=individus OUTSTAT=stat;
VAR &var_avec_age;
RUN;

/* interpretation de l'exécution ci-dessus*/
/* on retien 4 axe comme comme on l'a préciser dans le code (n=4) */
/* le fichier individu créé dans la work contient juste la table de départ*/
/* le fichier Stat créé dans la work contient les stats desc et les corrélation (coefficient de chaques variables dans l'expression de la composante principale 
comme combinaison lineaire des variables initiales centrées-réduits).

Noter que pour avoir les coordonnée des variables sur l'axe factoriel, il faut multiplier (corr*racinne_carré(valeur propre de l'axe))
*/
/* notons aussi que dans la suite, nous allons récupérer les coéficient de l'ACP pour les compléter à nos données*/


/* nous commenteron correctement les sortie de l'ACP en section 1.3.3
pour l'instant, on va simplement s'interresser à réaliser les représentation graphiques dans les différents axes factoriel*/


/* affiche le contenu de la table "STAT"*/
PROC PRINT DATA=stat ;
RUN ;

/* transpose la table STAT vers la table SORTIE*/

PROC TRANSPOSE DATA=stat OUT=sortie;
RUN;

/* on on recupère les valeures propres et on essaie de les afficher en 4 colonnes  */
/* chaque li pour chaque valeurs propre*/

PROC TRANSPOSE DATA=stat OUT=eigenval;
BY _name_;
WHERE _type_ = 'EIGENVAL';
RUN;


DATA eigenval;
 SET eigenval;
IF _n_ = 1 THEN l1 = col1;
IF _n_ = 2 THEN l2 = col1;
IF _n_ = 3 THEN l3 = col1;
IF _n_ = 4 THEN l4 = col1;
RUN;

PROC MEANS DATA=eigenval SUM;
VAR l1 l2 l3 l4;
OUTPUT OUT=eigenval SUM=;
RUN;

/* on crée les macro variables "eigen_i_" pour chaque valeurs propres*/
 
DATA _null_;
SET eigenval;
CALL SYMPUT('eigen1',l1);
CALL SYMPUT('eigen2',l2);
CALL SYMPUT('eigen3',l3);
CALL SYMPUT('eigen4',l4);
RUN;

/* on a fini de récupérer les valeurs propres*/

/* maintenant, on prend la table transposé "sortie" et on calcul les coordonnées de chaques variables sur les axes factoriel*/
/* on envoie les resultats dans la tables "SORTIE2"*/

DATA sortie2;
SET sortie;
prin1 = prin1 * sqrt(&eigen1);
prin2 = prin2 * sqrt(&eigen2);
prin3 = prin3 * sqrt(&eigen3);
prin4 = prin4 * sqrt(&eigen4);
RUN;

/* on affiche les graphiques des variables dans les 3 premiers axes factoriels (combinaison des axes 1 à 3)*/
/* l'instruction _name_ permet d'afficher les nom des variables dans le graphique*/
/* les graph seront interprété plus loin section 1.3.3, mais dejà remarquons sur le graph 1x2 que 
les ages (-30, 50-60, et 60-70 sont très proche)
cela se comprend mieux (plus bas avec les commandes après celle-ci) quand on croise l'age moyen 
avec certaines variable (avec_enfant, nb_pers_au_foyer, celibataire)
qui montrent des similarité entre ces classe d'ages(plus forte proportion de célibataire, 
en notant biensur que les veufs sont considéré comme célibataire dans notre fichier)
*/
PROC PLOT DATA=sortie2;
PLOT prin2*prin1=_name_ $ _name_ prin3*prin1=_name_ $ _name_ prin3*prin2=_name_ $ _name_ ;
RUN;
QUIT;


/* Recherche des variables les plus liées à l'âge */

/* avec l'OQS ci-dessous, on ouvre une table nommé "ChiSq" qui va contenir les sortie */
ODS OUTPUT ChiSq = ChiSq; 

/* on fait des tableaux croisé entre "age" et chacune des varibles contenu dans la parenthèse, 
ensuite on met des option pour ne pas afficher certaines statistique 
et il reste juste les pourcentage en ligne qui sont affiché*/

/* ici, on a appliqué les formats créés plus haut dans la work à "age_moyen" et aux variables contenu
dans la macro variable "&var". mais on a appliqué accun format à "nbpers_au_foyer" .*/

/* rappel, si ki2<5%, on rejette Ho d'indépendance, et donc alors les deux cont liés*/
/* cependant, notons que ici, nous avons des variables discretes 
et non des variable qualitatives dans nos tableaux croisé*/

/* demander, comment il a interprèté et identifier les proximité dans les tableaux (page 13 à 14 du livre)*/

PROC FREQ DATA=test;
TABLES age_moyen*(avec_enfant nbpers_au_foyer celibataire) / NOCOL NOPERCENT NOCUM CHISQ ;
FORMAT age_moyen age. &var autrevar. nbpers_au_foyer ; 
RUN;

/* on récupère les valeurs de cramer de chaque tableaux croisé, 
ceci est normal car les données ne sont pas qualitatives et donc pas possibilité d'utiliser le ki2*/

DATA ChiSq (keep = Table Value);  
 SET ChiSq;
WHERE  Statistic like '%Cramer%';
RUN;

PROC SORT DATA = ChiSq;
BY DESCENDING Value; 
RUN;

PROC PRINT DATA = ChiSq (obs = 30);
RUN;
/* meme si ces trois ages sont proche et peuvent s'expliquer par les tableaux précédents, il n'en demeure pas 
moins qu'il sont proche du centre (0,0) du plan au lieu d'être proche des bors du crecle de corrélation (de rayon 1) 
et donc ces trois ages sont mal représenté sur l'axe 1x2 alors on peut chercher à les représenter sur d'autres axes*/

/* par la suite, on remarque dans les deux ou troix autres projection que ces variables Age_I_ 
NE SONT TOUJOURS PAS BIEN REPR2SENTé  car ils sont toujours loin du bord du cercle de corrélation. 
Ainsi, ils seront suprimé dans la suite de l'analyse.*/

/* ce constat doit nous rappeler dans le cours que la proximité de deux variables n'entrainent pas
qu'elles sont fortement correlé positivement si elles sont loins du bord du cercle de corrélation.
car elle sont mal représenté. */


/* on le comprend mieux en calculant le coeficient de corrélation de ces variables comme ci-dessous . */

/* ou on remarque de très très faibles valeurs (en valeur absolue) des corficient de corrélation entre ces variables et meme parfois elles sont negatives*/
PROC CORR DATA=test ;
VAR agemoins30 age30a40 age40a50 age50a60 age60a70 ageplus70 ;
RUN;

/* de nos 4 valeur propres, il faut noter que les deux premiers expliquent 28% de la variance, 
cesi n'est pas énorme pour une ACP, mais il faut egalement tenir compte du nombre très important de 
variables explicative dans notre ACP (36).

Ainsi, ce n'est pas enorme, mais dans la suite, on va garder les deux premières valeurs propres. */






/* 1.3.2 c'est la meme chose realisé sous SAS STat Studio qui est un interface de clic bouton de SAS 
et dans lequel on peut coder aussi à volonté en langage IMLPlus qui est plus amélioré que le IML classique du SAS usuel.


et les codes doivent etre inséré sous "SUBMIT" et code ... puis "ENDSUBMIT".

*/




/* ==================================================================== */
/* SECTION 1.3.3
/* ==================================================================== */

/* -------------------------------------------------------------------- */
/* ACP sans l'âge
/* -------------------------------------------------------------------- */

/* on relance l'ACP sans l'age*/

ODS OUTPUT Eigenvalues = valprop ; /* cette instruction permet de récupérer directement  le tableau des 
valeurs propres nommé "Eigenvalues" dans l'output dans une nouvelle table nommé "valprop" que l'on crée.

biensur, les output a lire sont celui de l'ACP réalisé par la suite ci-dessous. */


PROC PRINCOMP DATA = test n=2 OUT=individus OUTSTAT=stat ;
VAR &var;
RUN ;

/* On peut vérifier que la variance d'une composante principale = valeur propre */
PROC UNIVARIATE DATA=individus ;
VAR prin1 prin2 ;
RUN ;

/* On peut vérifier que les composantes principales ont une corrélation nulle */
PROC CORR DATA=individus ;
VAR prin1 prin2 nbpers_au_foyer ;
RUN ;



/* ==================================================================== */
/* SECTION 1.3.4
/* ==================================================================== */

/* -------------------------------------------------------------------- */
/* Affichage du nuage des variables
/* -------------------------------------------------------------------- */


/* Préparation de l'affichage du nuage des variables */

DATA coord ;
MERGE stat (WHERE = (_type_ = "SCORE")) valprop (KEEP = eigenvalue) ;
ARRAY coeff _numeric_ ;
DO OVER coeff ;
	coeff = coeff * SQRT(eigenvalue) ;
END ;
/*DO i = 1 TO DIM(coeff) ;
	coeff{i} = coeff{i} * SQRT(eigenvalue) ;
END ;*/
DROP eigenvalue _type_ ;
RUN ;

PROC PRINT DATA = coord ;
RUN ;

PROC TRANSPOSE DATA = coord OUT = vect (RENAME = (_name_ = Variable)) ;
VAR _numeric_ ;
RUN ;


/* Affichage du nuage des variables en basse résolution */

PROC PLOT DATA = vect ;
PLOT prin2*prin1=Variable $ Variable;
RUN ;
QUIT ;


/* Graphique recourant à l'ANNOTATE comme indiqué dans opus 2 de O. Decourt, page 135 */

DATA vect ;
 SET vect ;
IF prin2 >= 0 THEN angle_var = ARCOS(prin1) ; ELSE angle_var = ARCOS(-prin1) + 3.14159 ;
RUN ;

PROC SORT DATA = vect ;
BY angle_var prin2 ;
RUN ;

DATA planfactoriel ;
	SET vect END = dernier ;
	xsys = "2" ; ysys = "2" ; when = "A" ;
	x = 0 ; y = 0 ; function = "MOVE " ;
	OUTPUT ;
	x = prin1 ; y = prin2 ; function = "DRAW " ;
	OUTPUT ;
	function = "LABEL" ; text = variable ;
	IF prin2 >= 0 THEN POSITION = "2" ; ELSE POSITION = "8" ;
	IF DIF(angle_var) < 0.03 AND ABS(DIF(prin2)) < 0.03 THEN DO ;
		IF prin2 >= 0 THEN POSITION = "9" ; ELSE POSITION = "3" ;
	END ;
	OUTPUT ;
	IF dernier THEN DO ;
		x = 1 ; y = 0 ; function = "MOVE " ;
		OUTPUT ;
		function = "DRAW " ; color = "BLUE" ;
		DO angle = 0 TO 2*3.14 BY 0.04 ;
			x = COS(angle) ; y = SIN(angle) ;
			OUTPUT ;
		END ;
	END ;
RUN ;

SYMBOL INTERPOL = none VALUE = dot POINTLABEL = NONE /*("#variable")*/ ;
AXIS1 ORDER=(-1 TO 1 BY 0.5) ;
PROC GPLOT DATA = vect ;
PLOT prin2 * prin1 / HREF=0 VREF=0 HAXIS=axis1 VAXIS=axis1 ANNOTATE=planfactoriel ;
RUN ;
QUIT ;


/* Autre représentation des variables plus belle qu'avec la PROC PLOT */

%PLOTIT(DATA=vect, plotvars=prin2 prin1, labelvar=Variable, href=0, vref=0, color=black, colors=black)


/* Autre graphique recourant à l'ANNOTATE 
/* Référence : SASV9-Preudhomme.pdf (voir la bibliographie) */
/* VERSION NON PUBLIEE DANS L'OUVRAGE */

DATA ANNOTER;
SET vect; /*On part du fichier SORTIE2*/
X=PRIN1; Y=PRIN2;
TEXT=Variable; /*Le texte à afficher est le nom de la variable*/
SIZE=1;      /*Taille du texte.*/
XSYS='2'; YSYS='2';
IF PRIN2 >= 0 THEN POSITION = "2" ; ELSE POSITION = "8" ;
LABEL Y='AXE 2' X='AXE 1';
RUN;

DATA CERCLE ;
XSYS='2'; YSYS='2'; WHEN = "A" ;
DO angle = 0 TO 360 BY 0.1 ;
IF angle = 0 THEN DO ;
function = "MOVE" ;
x = 1 ;
y = 0 ;
OUTPUT ;
END ;
ELSE DO ;
function = "DRAW" ;
x = cos(angle*3.1416/180) ;
y = sin(angle*3.1416/180) ;
OUTPUT ;
END ;
END ;
RUN ;

DATA ANNOTER ;
SET ANNOTER CERCLE ;
RUN ;

GOPTIONS RESET=all;
QUIT;
PROC GPLOT DATA=ANNOTER;
PLOT Y*X / ANNOTATE=WORK.ANNOTER HREF=0 VREF=0;
RUN;
QUIT;


/* Graphique recourant à l'ODS GRAPHICS et le langage GTL */
/* Référence : Olivier Decourt - Reporting avec SAS (voir la bibliographie) */
/* VERSION NON PUBLIEE DANS L'OUVRAGE */

ODS HTML;
ODS GRAPHICS ON;

PROC TEMPLATE ;
	DEFINE STATGRAPH exemples.acp ;
		LAYOUT OVERLAY / XGRID=TRUE YGRID=TRUE  
						 XAXISOPTS=(LABEL="Axe n°1" TICKS=(-1 0 1))
						 YAXISOPTS=(LABEL="Axe n°2" TICKS=(-1 0 1)) ;
			VECTORPLOT X=prin1 Y=prin2 / DATALABEL=variable XMIN=-1 XMAX=1 YMIN=-1 YMAX=1 ;
			ELLIPSEPARM SEMIMAJOR=1 SEMIMINOR=1 SLOPE=0 /    
               LINECOLOR=BLUE LINEPATTERN=DASH; 
		ENDLAYOUT ;
	END ;
RUN ;
ODS EXCLUDE ALL ;
ODS OUTPUT Eigenvalues = valprop ;
PROC PRINCOMP DATA = test n=2 OUTSTAT=stat ;
VAR &var;
RUN ;
DATA coord ;
MERGE stat (WHERE = (_type_ = "SCORE")) valprop (KEEP = eigenvalue) ;
ARRAY coord _numeric_ ;
DO OVER coord ;
coord = coord * sqrt (eigenvalue) ;
END ;
DROP eigenvalue _type_ ;
RUN ;
PROC TRANSPOSE DATA = coord OUT = vect (RENAME = (_name_ = Variable)) ;
VAR _numeric_ ;
RUN ;
ODS SELECT ALL ;
DATA _NULL_ ;
	SET vect ;
	FILE PRINT ODS=(TEMPLATE="exemples.acp") ;
	PUT _ODS_ ;
RUN ;

ODS GRAPHICS OFF;
ODS HTML CLOSE;



/* ==================================================================== */
/* SECTION 1.3.5
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Affichage du nuage des individus
/* -------------------------------------------------------------------- */

GOPTIONS RESET = all ; QUIT ;
PROC GPLOT DATA=individus ;
PLOT prin2*prin1 ;
RUN ;
QUIT ;

PROC PLOT DATA=individus ;
PLOT prin2*prin1 ;
RUN ;
QUIT ;



/* ==================================================================== */
/* SECTION 1.4
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Classification par la méthode des k-means
/* -------------------------------------------------------------------- */


/*PROC STANDARD DATA= individus OUT=reduit MEAN=0 STD=1;
VAR &var;
RUN;*/

PROC FASTCLUS DATA=individus MAXC=20 MAXITER=50 CONVERGE=0.02
MEAN=centres OUT=partitio CLUSTER=presegm DELETE=5 DRIFT;
VAR &var;
RUN;

PROC PRINT DATA=centres (obs=20); RUN;

PROC PRINT DATA=partitio (obs=10); RUN;

PROC FREQ DATA=partitio ORDER=freq;
TABLE presegm;
RUN;

PROC FREQ DATA=centres;
TABLE _freq_;
RUN;

GOPTIONS RESET=all;
GOPTIONS COLORS=(black);
/*goptions csymbol=black;*/
PROC GPLOT DATA=partitio;
PLOT prin2*prin1=presegm;
RUN;
QUIT;

PROC GPLOT DATA=centres;
PLOT _gap_*_freq_='G' _radius_*_freq_='R'/ OVERLAY;
RUN;
QUIT;



/* ==================================================================== */
/* SECTION 1.5
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Classification ascendante hiérarchique
/* -------------------------------------------------------------------- */


PROC CLUSTER DATA=centres OUTTREE=tree METHOD=ward CCC PSEUDO PRINT=10;
/*PROC CLUSTER DATA=centres OUTTREE=tree METHOD=density k=10 CCC PSEUDO PRINT=10;*/
/*PROC CLUSTER DATA=centres OUTTREE=tree METHOD=twostage hybrid CCC PSEUDO PRINT=10;*/
VAR &var;
COPY presegm;
RUN;

PROC PRINT DATA=tree;
RUN;

PROC SORT DATA=tree;
BY _ncl_;
RUN;

SYMBOL1 color=black INTERPOL=join VALUE=dot HEIGHT=1 ;
PROC GPLOT DATA=tree ;
WHERE _ncl_ < 20 ;
PLOT (_rsq_ _sprsq_ _ccc_ _rmsstd_ _psf_ _pst2_) * _ncl_ ;
RUN ;
GOPTIONS RESET=all ;
QUIT ;

PROC TREE DATA=tree NCL=4 OUT=segmhier ;
COPY presegm ;
RUN ;
PROC PRINT DATA=segmhier ;
RUN ;

PROC SORT DATA=partitio; BY presegm; RUN;
PROC SORT DATA=segmhier; BY presegm; RUN;
DATA segm;
MERGE partitio segmhier;
BY presegm;
RUN;

PROC FREQ ORDER=freq; TABLE cluster; RUN;

GOPTIONS RESET=all;
GOPTIONS COLORS=(black);
PROC GPLOT;
PLOT prin2*prin1=cluster;
RUN; QUIT;



/* -------------------------------------------------------------------- */
/* SAUVEGARDE DU FICHIER INITIAL + PRIN1 + PRIN2 + CLUSTER              */
/* -------------------------------------------------------------------- */

DATA &ASSURANC..assurancetic_segm (COMPRESS = BINARY) ;
SET segm ;
DROP presegm clusname _name_ distance ;
RUN ;



/* ==================================================================== */
/* SECTION 1.6
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Interprétation des classes d'individus par le test de Kruskal-Wallis
/* -------------------------------------------------------------------- */


PROC TRANSREG DATA = segm DESIGN NOPRINT ;
MODEL CLASS (cluster / ZERO = NONE) ;
ID cle &var ;
OUTPUT OUT = segm_disjonctif (drop = _name_ _type_) ;
RUN ;

%PUT &_TRGIND ;


%MACRO interpret(cible) ;

ODS EXCLUDE ALL ;
ODS OUTPUT KruskalWallisTest = kruskal WilcoxonScores = wilcoxon ;

PROC NPAR1WAY WILCOXON data=segm_disjonctif correct=no;
CLASS &cible ;
VAR &var;
RUN;

ODS SELECT ALL ;

DATA kruskal (KEEP = Variable nValue1 RENAME=(nValue1=KWallis));
SET kruskal;
WHERE name1 = '_KW_';
RUN;

PROC SORT DATA=wilcoxon ; BY variable DESCENDING class ; RUN;

DATA wilcoxon (KEEP = variable signe) ;
SET wilcoxon (KEEP = variable class Meanscore) ;
BY variable ;
RETAIN score 0;
IF FIRST.variable THEN score = meanscore ;
IF LAST.variable THEN do ;
   IF meanscore > score THEN signe = "moins" ;
                        ELSE signe = "plus" ;
   OUTPUT ;
END ;
RUN ;

PROC SORT DATA = kruskal; BY variable ;
PROC SORT DATA = wilcoxon; BY variable ;

DATA resultat ;
MERGE kruskal wilcoxon ;
BY variable ;
RUN ;

PROC SORT DATA=resultat ; BY DESCENDING KWallis ; RUN;

TITLE1 "Caractérisation de la variable &cible" ;
PROC PRINT DATA = resultat (OBS = 20) ; RUN ;
TITLE1 " " ;

%MEND interpret ;

%interpret(cluster1);
%interpret(cluster2);
%interpret(cluster3);
%interpret(cluster4);



/* -------------------------------------------------------------------- */
/* INTERPRETATION DES CLASSES PAR UNE MACRO D'OLIVIER DECOURT
/* Référence : http://www.od-datamining.com/
/* VERSION NON PUBLIEE DANS L'OUVRAGE */
/* -------------------------------------------------------------------- */

%MACRO caracParQuanti (tableIn =, varTarget =, varInput = _NUMERIC_, poids =, pValFiltre = 0.15) ;
	/* Format d'affichage des valeurs-tests */
	PROC FORMAT ;
		VALUE vTest
			LOW  - -5  	  = "****|    "
			 -5 <- -2.31  = " ***|    "
		  -2.31 <- -1.64  = "  **|    "
		  -1.64 <- -1.28  = "   *|    "
		  -1.28 <-< 1.28  = "    |    "
		   1.28  -< 1.64  = "    |*   "
		   1.64  -< 2.31  = "    |**  "
		   2.31  -< 5  	  = "    |*** "
			  5  - HIGH   = "    |****"
		;
	RUN ;
	ODS EXCLUDE ALL ;
	ODS OUTPUT simpleStatistics = work.statsGpe (KEEP = variable &varTarget mean n
							WHERE = (&varTarget IS NOT MISSING)) ;
	PROC DISCRIM DATA = &tableIn SIMPLE ;
	CLASS &varTarget ;
	VAR &varInput ;
	%IF &poids NE %THEN %DO ;
		WEIGHT &poids ;
	%END ;
	RUN ;
	ODS OUTPUT simpleStatistics = work.statsTot (KEEP = variable &varTarget mean n stdDev
							WHERE = (&varTarget IS MISSING));
	PROC DISCRIM DATA = &tableIn SIMPLE ;
	CLASS &varTarget ;
	VAR &varInput ;
	%IF &poids NE %THEN %DO ;
		WEIGHT &poids ;
	%END ;
	RUN ;
	ODS SELECT ALL ;
	PROC SQL ;
		CREATE TABLE work.temp AS
			SELECT modalite.&varTarget,
			       modalite.variable AS variable LENGTH = 32,
				   modalite.mean	AS mClasse
								   	   LABEL = "Moyenne dans la classe",
				   ensemble.mean	AS mGen
								   	   LABEL = "Moyenne générale",
				   modalite.n		AS nk
				   					   LABEL = "Effectif dans la classe",
				   (modalite.mean - ensemble.mean) / 
					(SQRT(((ensemble.n - modalite.n)/
	                       ((ensemble.n - 1)*(modalite.n))))*ensemble.stdDev) 
									AS vTest 
									   FORMAT = NUMX8.2
								   	   LABEL = "Valeur test",
				   1-PROBNORM(ABS(CALCULATED vTest)) /* proba d'une loi normale centrée réduite */
									AS pValue
								   	   LABEL = "Probabilité de la valeur test"
									   FORMAT = PVALUE8.4,
				   CALCULATED vTest	AS etoiles
				   					   FORMAT = vTest.
									   LABEL = "---------"	
			FROM work.statsGpe AS modalite,
				 work.statsTot (DROP = &varTarget) AS ensemble
			WHERE modalite.variable = ensemble.variable
			ORDER BY &varTarget,
			         vTest DESC
		;
	QUIT ;
	TITLE1 "Caractérisation de la variable &varTarget" ;
	TITLE2 "Par les variables quantitatives" ;
	PROC PRINT DATA = work.temp NOOBS LABEL ;
		WHERE vTest IS NOT MISSING AND pvalue <= &pValFiltre ;
		VAR variable -- etoiles ;
		BY &varTarget ;
	RUN ;
	PROC SQL ;
		DROP TABLE work.statsGpe ;
		DROP TABLE work.statsTot ;
		DROP TABLE work.temp ;
	QUIT ;
	TITLE1 " " ;
	TITLE2 " " ;
%MEND caracParQuanti ;

%caracParQuanti (tableIn =segm, varTarget =cluster, varInput = &var, poids =, pValFiltre = 0.15);



/* ==================================================================== */
/* SECTION 1.7
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* ACP AVEC NOS DE CLASSE en variables actives comme les autres
/* -------------------------------------------------------------------- */


PROC PRINCOMP DATA=segm_disjonctif (KEEP = &var cluster1-cluster4) n=2
OUT=temp OUTSTAT=stat;
VAR &var cluster1-cluster4;
RUN;

PROC TRANSPOSE DATA=stat OUT=sortie; RUN;

PROC TRANSPOSE DATA=stat OUT=eigenval;
BY _name_;
WHERE _type_ = 'EIGENVAL';
RUN;

DATA eigenval;
SET eigenval;
IF _n_ = 1 THEN l1 = col1;
IF _n_ = 2 THEN l2 = col1;
RUN;

PROC MEANS DATA=eigenval SUM;
VAR l1 l2;
OUTPUT OUT=eigenval SUM=;
RUN;

DATA _null_;
SET eigenval;
CALL symput('eigen1',l1);
CALL symput('eigen2',l2);
RUN;

DATA sortie2;
SET sortie;
prin1 = prin1 * sqrt(&eigen1);
prin2 = prin2 * sqrt(&eigen2);
RUN;

PROC PLOT DATA=sortie2;
PLOT prin2*prin1=_name_ $ _name_;
RUN;
QUIT;


/* -------------------------------------------------------------------- */
/* ACP AVEC NOS DE CLASSE en variables supplémentaires : macro INSEE
/* -------------------------------------------------------------------- */

LIBNAME bibmacro "&C\Macros INSEE";
OPTIONS sasmstore=bibmacro mstored ;
%acp(DATAACT=segm_disjonctif,VARACT=&var,VARSUP=cluster1-cluster4,impress=O,vecp=MAX,ioa=0,iva=2,ivs=2,OUT=SOR,NAXER=4,FILL=ALL);

TITLE1 "Les variables dans le plan 1-2";
%PLOTACP(AXEH=1,AXEV=2,POINTS=VARACT VARSUP);
TITLE1 " ";

TITLE1 "Les variables dans le plan 1-3";
%PLOTACP(AXEH=1,AXEV=3,POINTS=VARACT VARSUP);
TITLE1 " ";



/* ==================================================================== */
/* SECTION 1.8
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Classification hiérarchique descendante des variables
/* -------------------------------------------------------------------- */


PROC VARCLUS DATA=segm_disjonctif (KEEP = &var cluster1-cluster4)
MAXEIGEN = 1.5 OUTSTAT=coef /* OUTTREE=tree */;
VAR &var cluster1-cluster4;
RUN;


/*PROC TREE DATA=tree HORIZONTAL ;
HEIGHT _propor_ ;
RUN ;*/


DATA coef2;
 SET coef;
IF _ncl_ = . OR _ncl_ = 4;
DROP _ncl_;
RUN;


PROC SCORE DATA=segm_disjonctif (KEEP = &var cluster1-cluster4)
SCORE=coef2 OUT=segm_var_obs ;
VAR &var cluster1-cluster4 ;
RUN ;

%ACP(DATAACT=segm_var_obs,VARACT=&var,varsup=cluster1-cluster4 clus1-clus4,impress=O,vecp=MAX,ioa=0,iva=2,ivs=2,OUT=SOR,NAXER=2,FILL=ALL);

TITLE1 "Les variables dans le plan 1-2";
%PLOTACP(AXEH=1,AXEV=2,POINTS=VARACT VARSUP);
TITLE1 " ";



/* ==================================================================== */
/* SECTION 1.9
/* ==================================================================== */


/* FERMETURE DU FICHIER ODS CONTENANT LES RESULTATS */

ODS RTF CLOSE ;


/* © 2009 www.editionstechnip.com ETUDE DE CAS EN STATISTIQUE DECISIONNELLE par Stéphane Tufféry */


/* *********************************************************************/
/* *********************************************************************/
/*            ETUDE DE CAS EN STATISTIQUE DECISIONNELLE
/*                       STEPHANE TUFFERY
/*                    EDITIONS TECHNIP - 2009
/* *********************************************************************/
/* *********************************************************************/


/* 15/06/2009 - Version 1.0                                            */


/* *********************************************************************/
/*                 CHAPITRE 2 : DATA MINING PREDICTIF
/* *********************************************************************/


/* Le lecteur désireux de tester le présent programme dans un environnement 
Windows doit préalablement télécharger sur le site des éditions Technip :
- le fichier compressé "base_assurance_segmentee.zip" contenant la table SAS
"assurancetic_segm.sas7bdat"
- le code source des macro-programmes SAS xmacro.sas et treedisc.sas
- le fichier sasmacr.sas7bcat, qui contient le code compilé du macro-programme
"discretisation", utilisé dans la section 2.5
(- pour les vérifications de la Conclusion (section 2.29) sur l'échantillon
complémentaire de 4000 clients, le fichier compressé "base_assurance_validation.zip"
contenant la table SAS "assurancetic_validation.sas7bdat").

Le fichier "assurancetic_segm.sas7bdat" doit ensuite être décompressé dans
un répertoire de travail qui sera utilisé tout au long de la session SAS.
C'est le fichier de données en entrée du présent programme.
C'est aussi le fichier créé par le programme du 1er chapitre.

Les macro-programmes xmacro.sas et treedisc.sas doivent aussi être
placés dans le répertoire de travail, de même que le fichier sasmacr.sas7bdat.

La macro-variable suivante C correspond au chemin menant au répertoire de travail */

/* Exemple sous Windows Vista : */
%LET C=C:\Users\Stéphane\Documents\Datamining\Etudes de cas\Assurance\Technip;

/* Exemple sous Windows XP : */
%LET C=C:\Documents and Settings\tufferst\My Documents\Data Mining\Etudes de cas\Assurance\Technip;

/* Le sous-répertoire des macros INSEE sera donc : "&C\Macros INSEE". */


/* La macro-variable suivante correspond à la bibliothèque SAS associée
par un LIBNAME au répertoire de travail. */
LIBNAME assuranc "&C" ;
%LET ASSURANC=assuranc ;



/* ==================================================================== */
/* SECTION 1.2
/* ==================================================================== */


/* LECTURE DU FICHIER DE DONNEES */

DATA test ;
 SET &ASSURANC..assurancetic_segm ;
RUN ;


/* CREATION D'UN STYLE ODS */

ODS PATH fmtccf.templat(update) sashelp.template(read) sashelp.tmplmst(read) sasuser.templat(update) work.templat(update);
PROC TEMPLATE ;                                                                
   DEFINE style Styles.EC ;                     
      PARENT = styles.RTF ;                                                
      STYLE Header from HeadersAndFooters                                     
         "Controls the header style" /                                        
         JUST = center
         BACKGROUND = GRAYEE ;                                                    
      STYLE rowHeader from HeadersAndFooters                                  
         "Controls the header style" /                                        
         JUST = center
         BACKGROUND = GRAYEE ;       
   END ;
RUN ;


/* OUVERTURE DU FICHIER ODS CONTENANT LES RESULTATS */

ODS RTF FILE = "&C\sas_assurance_predictif.doc" STYLE=Styles.EC ;


/* LISTE DES VARIABLES */

/* variables sociodémographiques */
%LET varsociodemo = nbmaisons nbpers_au_foyer age_moyen
catholique protestant autre_religion sans_religion marie concubin
autre_relation celibataire sans_enfant avec_enfant niv_etude_haut
niv_etud_moy niv_etud_bas PCStop PCScadre PCSagri PCSinter
PCSouvr_quali PCSouvr locataire proprietaire auto1 auto2 auto0
assur_sante_public assur_sante_prive revenu1 revenu2 revenu3
revenu4 revenu5 revenu_moyen pouvoir_achat ;

/* variables de détention pour 21 types de produit */
%LET varmontant =
mt_RC mt_RC_entreprise mt_RC_agri mt_auto mt_camion_livraison
mt_moto mt_camion mt_remorque mt_tracteur mt_machine_agri
mt_cyclomoteur mt_assur_vie mt_accident_perso mt_accident_famil
mt_invalidite mt_incendie mt_planche_voile mt_bateau mt_velo
mt_MRH mt_securite_soc ;

%LET varnombre =
nb_RC nb_RC_entreprise nb_RC_agri nb_auto nb_camion_livraison
nb_moto nb_camion nb_remorque nb_tracteur nb_machine_agri
nb_cyclomoteur nb_assur_vie nb_accident_perso nb_accident_famil
nb_invalidite nb_incendie nb_planche_voile nb_bateau nb_velo
nb_MRH nb_securite_soc ;

%let vardetention = &varmontant &varnombre ;

%let var = &varsociodemo &vardetention
type_client /* variable sociodémo mise à part car non quantitative */
cluster /* variable calculée par classification dans le 1er chapitre */
/* assur_caravane -> variable cible */
;


/* FORMAT SAS UTILISES */

PROC FORMAT ;
VALUE supzero      
low-0   = "  0"     
0<-high = "> 0" ;
VALUE age
1     = ' < 30 ans'
2     = '30-40 ans'
3     = '40-50 ans'
4     = '50-60 ans'
5     = '60-70 ans'
6     = ' > 70 ans' ;
VALUE cotisation
0 = '            0'
1 = '    1 -    49'
2 = '   50 -    99'
3 = '  100 -   199'
4 = '  200 -   499'
5 = '  500 -   999'
6 = ' 1000 -  4999'
7 = ' 5000 -  9999'
8 = '10000 - 19999'
9 = '     >= 20000';
VALUE autrevar
0 = '     0 %'
1 = '1 - 10 %'
2 = '11 - 23%'
3 = '24 - 36%'
4 = '37 - 49%'
5 = '50 - 62%'
6 = '63 - 75%'
7 = '76 - 88%'
8 = '89 - 99%'
9 = '    100%' ;
VALUE typeclient
1 = 'Successful hedonists'
2 = 'Driven Growers'
3 = 'Average Family'
4 = 'Career Loners'
5 = 'Living well'
6 = 'Cruising Seniors'
7 = 'Retired and Religeous'
8 = 'Family with grown ups'
9 = 'Conservative families'
10 = 'Farmers';
RUN ;



/* ==================================================================== */
/* SECTION 2.3.1
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* EXPLORATION DES DONNEES : PREMIERS TABLEAUX CROISES
/* -------------------------------------------------------------------- */

PROC UNIVARIATE DATA=test NORMAL ;
VAR &var ;
HISTOGRAM &var / NORMAL (MU=EST SIGMA=EST COLOR=black L=2) MIDPOINTS=0 to 9 by 1 ;
INSET MEAN MEDIAN CV NMISS NORMAL(KSDPVAL) NORMAL(ADPVAL) NORMAL(CVMPVAL) ;
RUN ;

PROC UNIVARIATE DATA=test NORMAL ;
VAR &var  ;
CLASS assur_caravane ;
HISTOGRAM &var  / NORMAL (MU=EST SIGMA=EST COLOR=black) MIDPOINTS=0 to 9 by 1 ;
RUN ;



/* ==================================================================== */
/* SECTION 2.3.2
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Produits d'assurance : cohérence entre nombre de contrats et cotisations
/* -------------------------------------------------------------------- */

PROC FREQ DATA=test;
TABLE
nb_RC			*	mt_RC
nb_RC_entreprise	*	mt_RC_entreprise
nb_RC_agri		*	mt_RC_agri
nb_auto			*	mt_auto
nb_camion_livraison	*	mt_camion_livraison
nb_moto			*	mt_moto
nb_camion		*	mt_camion
nb_remorque		*	mt_remorque
nb_tracteur		*	mt_tracteur
nb_machine_agri		*	mt_machine_agri
nb_cyclomoteur		*	mt_cyclomoteur
nb_assur_vie		*	mt_assur_vie
nb_accident_perso	*	mt_accident_perso
nb_accident_famil	*	mt_accident_famil
nb_invalidite		*	mt_invalidite
nb_incendie		*	mt_incendie
nb_planche_voile	*	mt_planche_voile
nb_bateau		*	mt_bateau
nb_velo			*	mt_velo
nb_MRH			*	mt_MRH
nb_securite_soc		*	mt_securite_soc
/ NOROW NOCOL ;
FORMAT &varmontant cotisation. ;
RUN ;



/* ==================================================================== */
/* SECTION 2.3.3
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Tableaux croisés entre variables explicatives et variable à expliquer
/* -------------------------------------------------------------------- */


/* Tableaux croisés des montants avec la variable cible */
PROC FREQ DATA=test ;
TABLE (mt_auto mt_incendie)*assur_caravane / NOCOL ;
FORMAT &varmontant cotisation. ;
RUN ;

/* Tableaux croisés des variables sociodémographiques avec la variable cible */
PROC FREQ DATA=test ;
TABLE (&varsociodemo type_client cluster)*assur_caravane / NOCOL ;
FORMAT &varsociodemo autrevar. age_moyen age. type_client typeclient. 
nbmaisons nbpers_au_foyer revenu_moyen pouvoir_achat cluster ;
RUN ;



/* ==================================================================== */
/* SECTION 2.3.4
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Création de nouvelles variables explicatives 
/* -------------------------------------------------------------------- */

DATA test ;
 SET test ;
 mt_contrats = SUM(OF mt_RC -- mt_securite_soc) ;
 nb_contrats = SUM(OF nb_RC -- nb_securite_soc) ;
RUN ;

PROC FREQ DATA = test ;
TABLE (nb_contrats mt_contrats) * assur_caravane / NOCOL ;
RUN ;

PROC UNIVARIATE DATA=test NORMAL ;
VAR mt_contrats nb_contrats ;
HISTOGRAM mt_contrats nb_contrats / NORMAL (MU=EST SIGMA=EST COLOR=black) MIDPOINTS=0 to 9 by 1 ;
RUN ;

PROC UNIVARIATE DATA=test NORMAL ;
VAR mt_contrats nb_contrats ;
CLASS assur_caravane ;
HISTOGRAM mt_contrats nb_contrats / NORMAL (MU=EST SIGMA=EST COLOR=black) MIDPOINTS=0 to 9 by 1 ;
RUN ;



/* ==================================================================== */
/* SECTION 2.3.4
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Détention de contrats d'assurance dans chaque segment de clientèle
/* -------------------------------------------------------------------- */

/* La caractérisation qui est effectuée ici repose sur le test de Kruskal-Wallis
et la macro %INTERPRET de la section 1.6, cette macro mesurant l'intensité
et le sens de la liaison entre chaque montant de primes d'assurances 
et l'indicatrice de chaque segment.*/

%MACRO interpret(cible) ;

PROC TRANSREG DATA = test DESIGN NOPRINT ;
MODEL CLASS (cluster / ZERO = NONE) ;
ID cle &var ;
OUTPUT OUT = segm_disjonctif (drop = _name_ _type_) ;
RUN ;

ODS EXCLUDE ALL ;
ODS OUTPUT KruskalWallisTest = kruskal WilcoxonScores = wilcoxon ;

PROC NPAR1WAY WILCOXON data=segm_disjonctif CORRECT=no;
CLASS &cible ;
VAR &varmontant ;
RUN ;

ODS SELECT ALL ;

DATA kruskal (keep = Variable nValue1 RENAME=(nValue1=KWallis));
 SET kruskal;
WHERE name1 = '_KW_';
RUN;

PROC SORT DATA=wilcoxon; BY variable DESCENDING class;RUN;

DATA wilcoxon (keep = variable signe);
 SET wilcoxon (keep = variable class Meanscore);
BY variable ;
RETAIN score 0;
IF first.variable THEN score =  meanscore;
IF last.variable  THEN do;
   IF meanscore > score THEN signe = "moins" ;
                        ELSE signe = "plus" ;
   OUTPUT;
END;
RUN;

PROC SORT DATA = kruskal; BY variable;
PROC SORT DATA = wilcoxon; BY variable;

DATA resultat;
MERGE kruskal wilcoxon ;
BY variable ;
RUN;

PROC SORT DATA=resultat;
BY DESCENDING KWallis;
RUN;

TITLE1 "Caractérisation de la variable &cible" ;
PROC PRINT DATA = resultat (obs = 20); RUN;
TITLE1 " " ;

%MEND interpret ;


%interpret(cluster1);
%interpret(cluster2);
%interpret(cluster3);
%interpret(cluster4);




/* ==================================================================== */
/* SECTION 2.4
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* REGROUPEMENT DE MODALITES POUR LES VARIABLES DE PRIMES D'ASSURANCE
/* -------------------------------------------------------------------- */


PROC FORMAT;
VALUE cotis_auto
0-5    = '<= 999'
6-high = ' > 999';
VALUE cotis_incendie
0-2    = '    <= 99'
3      = '100 - 199'
4      = '200 - 499'
5-high = '    > 499';
RUN;


PROC FREQ DATA=test;
TABLE (mt_auto mt_incendie)*assur_caravane / CHISQ NOCOL;
FORMAT mt_auto cotis_auto. mt_incendie cotis_incendie.;
RUN;



/* ==================================================================== */
/* SECTION 2.5
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* DISCRETISATION PAR ARBRE DE DECISION : MACRO TREEDISC DE SAS
/* -------------------------------------------------------------------- */


/* Il faut exécuter auparavant les macros XMACRO et TREEDISC */
%INC "&C\xmacro.sas" ;
%INC "&C\treedisc.sas" ;

/* appel de la macro compilée */
LIBNAME bibmacro "&C" ;
OPTIONS MSTORED sasmstore=bibmacro ;

%discretisation (test,assur_caravane,&varsociodemo,100,x,WORK);

/* Affichage des formats créés dans la bibliothèque de formats sélectionnée
et écriture des formats créés dans un fichier WORK.FORMATS */
PROC FORMAT LIB=WORK FMTLIB CNTLOUT=formats; RUN ;


/* Création d'une macro-variable "&vardiscrformat" contenant le nom de chaque variable discrétisée
/* dans l'étape précédente et le nom du format correspondant créé également précédemment
/* Création d'une macro-variable "&vardiscr" contenant le nom de chaque variable discrétisée
/* Dans l'étape DATA ci-dessous le WHERE doit correspondre au préfixe
/* passé en paramètre dans la macro précédente */
DATA temp (DROP=longueur) ;
      SET formats (KEEP=fmtname WHERE=(fmtname =: "X"));
      BY fmtname ;
      longueur = LENGTH(fmtname) - 2;
      name = SUBSTR(fmtname,2,longueur);
      IF FIRST.fmtname THEN OUTPUT ;
RUN ;

PROC PRINT DATA = temp ;
RUN ;

PROC SQL NOPRINT ;
      SELECT STRIP(name),
             STRIP(name)!!" "!!STRIP(fmtname)!!"."
      INTO : varDiscr       SEPARATED BY " ",
           : varDiscrFormat SEPARATED BY " "
      FROM temp
      ;
QUIT ;

/* Affichage de la valeur des macrovariables créées par l'étape précédente */
%PUT &vardiscr ;
%PUT &vardiscrformat ;



/* ==================================================================== */
/* SECTION 2.6
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Croisement des variables discrétisées avec la variable cible         */
/* -------------------------------------------------------------------- */


/*OPTION FMTSEARCH  = (SASUSER) ;*/

PROC FREQ DATA = test ;
TABLE (&vardiscr) * assur_caravane / NOCOL ;
FORMAT &vardiscrformat ;
RUN ;



/* ==================================================================== */
/* SECTION 2.7
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Représentation graphique du taux de souscription de chaque variable explicative
/* -------------------------------------------------------------------- */


%MACRO Distrivar (data, varY , varX) ;

%LET nb_var = %EVAL(%SYSFUNC(COUNTC(%SYSFUNC(COMPBL(&varX)),' ')) + 1);

/* Affichage dans la fenêtre LOG du nombre de variables comptées */
%PUT Nombre de scores traités : &nb_var ;

/* Boucle pour traiter l'ensemble des variables passées en paramètre */
%DO i = 1 %TO &nb_var ;

/* On commence à traiter la i ème variable. */

/* La fonction %SCAN permet d'extraire le &i e mot de &varX */
%LET var_temp = %SCAN(&varX,&i);

PROC FREQ DATA=&data NOPRINT;
TABLES &var_temp * &varY / out=t ;

PROC TRANSPOSE data=t out=t ;
VAR count ;
ID &varY ;
BY &var_temp ;

DATA t ;
SET t ;
proba = SUM(_1,0)/SUM(_0,_1) ;
logit = LOG(SUM(_1,0)/SUM(_0,0)) ;

SYMBOL v=DOT c=BLUE i=JOIN ;
PROC GPLOT DATA=t ;
PLOT (proba logit) *&var_temp ;
RUN ;
QUIT ;

/* Fin de la boucle sur l'ensemble des variables explicatives */
%END ;

%MEND ;

%Distrivar(test,assur_caravane,&var) ;



/* ==================================================================== */
/* SECTION 2.8
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* SELECTION DES VARIABLES : CALCULS DE CORRELATION
/* -------------------------------------------------------------------- */


PROC CORR DATA = test PEARSON SPEARMAN OUTP=pearson OUTS=spearman NOPRINT;
VAR &vardetention;

PROC PRINT DATA=pearson ;
RUN ;

DATA pearson (DROP  = _TYPE_ RENAME =(_NAME_ = variable1));
 SET pearson;
WHERE _TYPE_ = "CORR";

PROC PRINT DATA=pearson; RUN ;

PROC TRANSPOSE DATA = pearson NAME=variable2 PREFIX=correlation
OUT = pearson ;
VAR &vardetention;
BY variable1 NOTSORTED ;

PROC PRINT DATA=pearson; RUN ;

DATA pearson;
 SET pearson;
WHERE variable1 < variable2;
abscorrelation = ABS(correlation1);

PROC SORT DATA=pearson;
BY DESCENDING abscorrelation ;

PROC PRINT DATA=pearson;
RUN;

PATTERN1 C = grayCC ;
PROC GCHART DATA=pearson ;
VBAR abscorrelation ;
RUN;
QUIT;
GOPTION RESET = PATTERN ;



/* ==================================================================== */
/* SECTION 2.9
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* SELECTION DES VARIABLES (SUITE) : LIAISON AVEC LA VARIABLE CIBLE
/* DES VARIABLES EXPLICATIVES QUANTITATIVES
/* -------------------------------------------------------------------- */


ODS EXCLUDE ALL ;

ODS OUTPUT KruskalWallisTest = kruskal ; 
 
PROC NPAR1WAY WILCOXON DATA = test ; 
CLASS assur_caravane ;
VAR &varsociodemo &vardetention ;
RUN ;

ODS SELECT ALL ;

DATA kruskal ;  
 SET kruskal ; 
WHERE name1 = '_KW_' ;
KEEP Variable nValue1 ;
RENAME nValue1 = KWallis ;
RUN ;

PROC SORT DATA = kruskal ;
BY DESCENDING KWallis ; 

PROC PRINT DATA = kruskal ;
RUN;

PATTERN1 C = grayCC ;
PROC GCHART DATA = kruskal ;
VBAR KWallis ;
RUN;
QUIT;
GOPTION RESET = PATTERN ;



/* ==================================================================== */
/* SECTION 2.10
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* SELECTION DES VARIABLES (SUITE) : LIAISON AVEC LA VARIABLE CIBLE
/* DES VARIABLES EXPLICATIVES QUALITATIVES OU DISCRETISEES
/* -------------------------------------------------------------------- */


/*ODS EXCLUDE ALL ;*/

ODS OUTPUT ChiSq = ChiSq ; 

PROC FREQ DATA=test ;
TABLES (&vardiscr &varmontant type_client cluster)*assur_caravane / CHISQ ;
FORMAT &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie.
age_moyen age. nbmaisons cluster ;
RUN ;

/*ODS SELECT ALL ;*/

DATA ChiSq ;  
 SET ChiSq ; 
WHERE  Statistic CONTAINS "Cramer" ;
abs_V_Cramer = ABS(Value) ;
Variable = SCAN (Table,2) ;
KEEP Variable Value abs_V_Cramer ;
RUN ;
  

PROC SORT DATA = ChiSq;
BY DESCENDING abs_V_Cramer; 

PROC PRINT DATA = ChiSq;
RUN;


PATTERN1 C = grayCC ;
PROC GCHART DATA = ChiSq ;
VBAR abs_V_Cramer ;
RUN;
QUIT;
GOPTION RESET = PATTERN ;



/* ==================================================================== */
/* SECTION 2.11
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* SELECTION DES VARIABLES QUANTITATIVES PAR ANALYSE DISCRIMINANTE
/* -------------------------------------------------------------------- */


ODS OUTPUT Summary = varselec; 

PROC STEPDISC DATA= test ;
CLASS assur_caravane ;
VAR &varsociodemo &varmontant ;
RUN;

PROC SQL NOPRINT ;
CREATE TABLE varselec2
AS SELECT Entered AS Variable, step 
FROM varselec 
WHERE Entered NOT IN (SELECT Removed FROM varselec)
ORDER BY step ;
SELECT Entered INTO : vardiscrim SEPARATED BY ' '
FROM varselec 
WHERE Entered NOT IN (SELECT Removed FROM varselec)
ORDER BY step ;
QUIT ;

%PUT &vardiscrim ;



/* ==================================================================== */
/* SECTION 2.12
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* SELECTION DE VARIABLES PAR LOGIT SUR ECHANTILLONS BOOTSTRAP
/* Variante présentée dans l'ouvrage :
/* - avec variable REPLICATE (les n tirages bootstrap sont effectués
/*   en une seule fois grâce à l'instruction REP)
/* - avec utilisation ATTRIB dans une étape DATA pour associer une
/*   étiquette et un format à une variable 
/* -------------------------------------------------------------------- */


%MACRO SelectBoot(data , varY , ref , varX , class , format , n ) ;

DATA boot_result ;
ATTRIB 
variable length=$30 LABEL = 'Prédicteur'
prob   FORMAT = 6.4
sig1   FORMAT = 4.  LABEL = 'Signif à 1%'
sig2   FORMAT = 4.  LABEL = 'Signif à 5%'
sig3   FORMAT = 4.  LABEL = 'Signif à 10%'
sig4   FORMAT = 4.  LABEL = 'Non signif à 10%'
nb_Wald_bas FORMAT = 4. LABEL = 'nb Wald < 3.84'
Wald   FORMAT = 6.4 LABEL = '+ petit Wald des modalités'
;
RUN ;

DATA _NULL_ ;
  CALL SYMPUT('SAMPLE',N) ;
  STOP ;
  SET &data  NOBS=N ;
RUN ;
%PUT Nombre de tirages bootstrap = &sample ;

ODS EXCLUDE ALL ;

PROC SURVEYSELECT DATA = &data METHOD = urs OUT = &data._tmp SAMPSIZE = &sample NOPRINT REP = &n ;
RUN ;

ODS OUTPUT type3 = model_tmp ParameterEstimates=param_tmp  ;
PROC LOGISTIC DATA = &data._tmp ;
BY replicate ;
FREQ numberhits ;
CLASS &class ;
MODEL &varY (event = &ref) = &varX ;
%IF &format NE " " %THEN FORMAT &format ; ;
RUN ;

PROC SUMMARY DATA = param_tmp NWAY ;
WHERE ProbChiSq > 0.05 ;
CLASS replicate variable ;
OUTPUT OUT = wald_tmp (drop = _type_ rename = (_freq_ = nb_Wald_bas)) min(WaldChiSq)= Wald ;
RUN ;

PROC SQL ;
INSERT INTO boot_result
SELECT
a.effect AS variable, a.ProbChiSq AS prob,
(a.ProbChiSq <= 0.01 AND a.ProbChiSq > .) AS sig1,
(a.ProbChiSq > 0.01 AND a.ProbChiSq <= 0.05) AS sig2,
(a.ProbChiSq > 0.05 AND a.ProbChiSq <= 0.1) AS sig3,
(a.ProbChiSq > 0.1) AS sig4,
b.nb_Wald_bas, b.Wald AS Wald
FROM model_tmp AS a LEFT JOIN wald_tmp AS b
ON a.effect = b.variable AND a.replicate = b.replicate ;
QUIT ;


ODS SELECT ALL ;

PROC SUMMARY DATA = boot_result NWAY ;
CLASS variable ;
OUTPUT OUT = out_tmp (drop = _type_ rename = (_freq_ = count))
sum(sig1) = sum(sig2) = sum(sig3) = sum(sig4) = sum(nb_Wald_bas)= min(Wald)=;
RUN ;

DATA out_tmp ;
 SET out_tmp;
IF nb_Wald_bas = . THEN nb_Wald_bas = 0 ;
RUN ;

PROC SORT DATA=out_tmp ;
BY DESCENDING sig1 DESCENDING sig2 DESCENDING sig3 DESCENDING sig4 nb_Wald_bas ;
RUN ;

PROC PRINT DATA=out_tmp LABEL;
LABEL count='Occurrences';
RUN ;

%MEND SelectBoot ;


%SelectBoot(test , assur_caravane , '1' , &vardiscr &varmontant type_client cluster , &vardiscr &varmontant type_client cluster , &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie. age_moyen age. cluster, 1000);



/* -------------------------------------------------------------------- */
/* SELECTION DE VARIABLES PAR LOGIT SUR ECHANTILLONS BOOTSTRAP
/* Variante non présentée dans l'ouvrage :
/* - avec une boucle remplaçant l'usage de la variable REPLICATE
/*   (un seul tirage bootstrap par boucle)
/* - avec remplacement des deux premières étapes DATA par une PROC SQL 
/* -------------------------------------------------------------------- */


%MACRO SelectBoot(data , varY , ref , varX , class , format , n ) ;

PROC SQL NOPRINT ;
CREATE TABLE boot_result
(variable CHAR(30) LABEL = 'Prédicteur', prob NUM FORMAT = 6.4,
sig1 NUM FORMAT = 4. LABEL = 'Signif à 1%', sig2 NUM FORMAT = 4. LABEL = 'Signif à 5%',
sig3 NUM FORMAT = 4. LABEL = 'Signif à 10%', sig4 NUM FORMAT = 4. LABEL = 'Non signif à 10%',
nb_Wald_bas INT LABEL = 'nb Wald < 3.84',
Wald NUM FORMAT = 6.4 LABEL = '+ petit Wald des modalités') ;
SELECT COUNT(*) INTO :sample FROM &data;
QUIT ;

ODS EXCLUDE ALL ;

%DO i = 1 %TO &n ;

PROC SURVEYSELECT DATA = &data METHOD = urs OUT = &data._tmp SAMPSIZE = &sample NOPRINT ;
RUN ;

ODS OUTPUT type3 = model_tmp ParameterEstimates=param_tmp  ;
PROC LOGISTIC DATA = &data._tmp ;
FREQ numberhits ;
CLASS &class ;
MODEL &varY (event = &ref) = &varX ;
%IF &format NE " " %THEN FORMAT &format ; ;
RUN ;

DATA wald_tmp (DROP = ProbChiSq) ;
 SET param_tmp (KEEP = variable ProbChiSq WaldChiSq) ;
 WHERE ProbChiSq > 0.05 ;
 RENAME WaldChiSq = Wald ;
 RUN;

PROC SORT DATA=wald_tmp; BY variable; RUN ;

PROC SUMMARY DATA = wald_tmp NWAY ;
CLASS variable ;
OUTPUT OUT = wald_tmp (drop = _type_ rename = (_freq_ = nb_Wald_bas)) min(Wald)=;
RUN ;

PROC SQL ;
INSERT INTO boot_result
SELECT
a.effect AS variable, a.ProbChiSq AS prob,
(a.ProbChiSq <= 0.01 AND a.ProbChiSq > .) AS sig1,
(a.ProbChiSq > 0.01 AND a.ProbChiSq <= 0.05) AS sig2,
(a.ProbChiSq > 0.05 AND a.ProbChiSq <= 0.1) AS sig3,
(a.ProbChiSq > 0.1) AS sig4,
b.nb_Wald_bas, b.Wald AS Wald
FROM model_tmp AS a LEFT JOIN wald_tmp AS b
ON a.effect = b.variable ;
QUIT ;

%END ;

ODS SELECT ALL ;

PROC SUMMARY DATA = boot_result NWAY ;
CLASS variable ;
OUTPUT OUT = out_tmp (drop = _type_ rename = (_freq_ = count))
sum(sig1) = sum(sig2) = sum(sig3) = sum(sig4) = sum(nb_Wald_bas)= min(Wald)=;
RUN ;

DATA out_tmp ;
 SET out_tmp;
IF nb_Wald_bas = . THEN nb_Wald_bas = 0 ;
RUN ;

PROC SORT DATA=out_tmp ;
BY DESCENDING sig1 DESCENDING sig2 DESCENDING sig3 DESCENDING sig4 nb_Wald_bas ;
RUN ;

PROC PRINT DATA=out_tmp LABEL;
LABEL count='Occurrences';
RUN ;

%MEND SelectBoot ;


%SelectBoot(test , assur_caravane , '1' , &vardiscr &varmontant type_client cluster , &vardiscr &varmontant type_client cluster , &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie. age_moyen age. cluster, 100);



/* ==================================================================== */
/* SECTION 2.13
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* SYNTHESE DES SELECTIONS DE VARIABLES
/* -------------------------------------------------------------------- */


PROC RANK DATA=ChiSq OUT=ChiSqr DESCENDING ;
VAR abs_V_Cramer ;
RANKS RgCramer ;
RUN ;

PROC PRINT DATA=ChiSqr ; RUN ;


PROC RANK DATA=kruskal OUT=kruskalr DESCENDING ;
VAR KWallis ;
RANKS RgKruskal ;
RUN ;

PROC PRINT DATA=kruskalr ; RUN ;


PROC SORT DATA=varselec2 ; BY Variable ; RUN ;
PROC SORT DATA=ChiSqr ; BY Variable ; RUN ;
PROC SORT DATA=kruskalr ; BY Variable ; RUN ;

DATA synth_selec ;
 MERGE varselec2 (in=a RENAME=(Step=RgDisc))
       ChiSqr    (in=b)
       kruskalr  (in=c) ;
BY Variable ;
IF a OR b OR c;
RUN ;

PROC SORT ; BY DESCENDING KWallis ; RUN ;

PROC PRINT DATA = synth_selec ;
RUN ;

PROC EXPORT DATA = synth_selec OUTFILE = "&C\selection_variable.xls"
DBMS=EXCEL REPLACE ;
RUN ;



/* ==================================================================== */
/* SECTION 2.14
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* SELECTION DE VARIABLES AU VU DU FICHIER SYNTHETIQUE PRECEDENT
/* -------------------------------------------------------------------- */


%LET varselect =
mt_auto
mt_incendie
revenu_moyen
mt_RC
pouvoir_achat
niv_etud_bas
revenu1
proprietaire
niv_etude_haut
auto0
auto1
revenu3
nb_incendie
mt_securite_soc
marie
PCStop
PCSagri
PCSouvr_quali
type_client
CLUSTER
;



/* ==================================================================== */
/* SECTION 2.15
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* CLASSIFICATION DES VARIABLES
/* -------------------------------------------------------------------- */


PROC VARCLUS DATA = test OUTSTAT = resultat ;
VAR &varsociodemo &varmontant type_client cluster ;
RUN ;

DATA resultat (DROP = _name_) ;
 SET resultat ;
 WHERE _type_ = "GROUP" ;
RUN ;

PROC SORT DATA = resultat ;
BY DESCENDING _ncl_ ;
RUN ;

PROC SORT DATA = resultat NODUPKEY ;
BY _type_ ;
RUN ;

PROC TRANSPOSE DATA = resultat OUT = classes_var (DROP = _label_) NAME = Variable PREFIX = noclasse ;
RUN ;

PROC SORT DATA = classes_var ; BY Variable ; RUN ;

PROC SORT DATA = synth_selec ; BY Variable ; RUN ;

DATA synth_selec2 ;
 MERGE synth_selec (in=a)
       classes_var (in=b) ;
BY Variable ;
IF a ;
RUN ;

PROC SORT ; BY DESCENDING KWallis ; RUN ;

PROC PRINT DATA = synth_selec2 ;
RUN ;

PROC EXPORT DATA = synth_selec2 OUTFILE = "&C\selection_variable2.xls"
DBMS=EXCEL REPLACE ;
RUN ;


/* -------------------------------------------------------------------- */
/* SELECTION DE VARIABLES AU VU DU FICHIER SYNTHETIQUE PRECEDENT
/* -------------------------------------------------------------------- */

%let varselect2 =
auto0
niv_etude_haut
pouvoir_achat
mt_RC_agri
niv_etud_bas
mt_incendie
sans_religion
mt_invalidite
nbpers_au_foyer
revenu_moyen
mt_securite_soc
concubin
PCSagri
mt_MRH
mt_auto
mt_RC
type_client
;



/* ==================================================================== */
/* SECTION 2.16
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* ECHANTILLONS D'APPRENTISSAGE ET DE VALIDATION
/* Echantillonnage simple
/* -------------------------------------------------------------------- */


%LET seed = 123;

DATA apprent valid test ;
 SET test ;
IF RANUNI(&seed) < 0.66 THEN DO ; OUTPUT apprent ; cible = assur_caravane ; OUTPUT test ; END ;
                        ELSE DO ; OUTPUT valid ;   cible = . ; OUTPUT test ; END ;
RUN ;

PROC FREQ DATA = test ; TABLE cible*assur_caravane / MISSING ; RUN ;
PROC FREQ DATA = apprent ; TABLE assur_caravane ; RUN ;
PROC FREQ DATA = valid   ; TABLE assur_caravane ; RUN ;


/* -------------------------------------------------------------------- */
/* ECHANTILLONS D'APPRENTISSAGE ET DE VALIDATION
/* Echantillonnage stratifié
/* -------------------------------------------------------------------- */


%LET seed = 123;

PROC FREQ DATA = test ;
TABLES assur_caravane / OUT=effectifs ;
RUN ;

PROC SQL NOPRINT ;
SELECT MIN(count) INTO :effectif FROM effectifs ;
QUIT ;

%PUT &effectif ;

PROC SORT DATA = test ;
BY assur_caravane ;
RUN ;

PROC SURVEYSELECT DATA = test METHOD = srs OUT = test2 OUTALL SAMPSIZE=&effectif SEED = &seed ;
STRATA assur_caravane ;
RUN ;

PROC FREQ DATA = test2 ;
TABLES Selected * assur_caravane ;
RUN ;

PROC SURVEYSELECT DATA = test2 METHOD = srs OUT = test3 OUTALL SAMPRATE=66.66 SEED = &seed ;
WHERE selected = 1 ;
STRATA assur_caravane ;
RUN ;

PROC FREQ DATA = test3 ;
TABLES Selected * assur_caravane ;
RUN ;

DATA apprent2 valid2 test2 ;
 SET test3 ;
IF Selected = 1 THEN DO ; OUTPUT apprent2 ; cible = assur_caravane ; OUTPUT test2 ; END ;
                ELSE DO ; OUTPUT valid2 ;   cible = . ; OUTPUT test2 ; END ;
RUN ;

PROC FREQ DATA = test2    ; TABLE cible*assur_caravane / MISSING ; RUN ;
PROC FREQ DATA = apprent2 ; TABLE assur_caravane ; RUN ;
PROC FREQ DATA = valid2   ; TABLE assur_caravane ; RUN ;
RUN ;



/* ==================================================================== */
/* SECTION 2.17
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* MACROS DE CALCUL D'AIRE SOUS LA COURBE ROC
/* -------------------------------------------------------------------- */


%MACRO AUC(data,cible,score,ciblepart,borne);

ODS OUTPUT WilcoxonScores = wilcoxon;
                                          
PROC NPAR1WAY WILCOXON DATA=&data CORRECT=no;
where &ciblepart LT &borne;
CLASS &cible;
VAR &score;
RUN;

DATA auc;
SET wilcoxon;
n0 = N; R0 = SumOfScores ;
n1 = LAG(N); R1 = LAG(SumOfScores) ;
U1 = (n1*n0) + (n1*(n1+1)/2) - R1 ;
U0 = (n1*n0) + (n0*(n0+1)/2) - R0 ;
AUC = ROUND(MAX(U1,U0)/(n1*n0),0.001) ;
RUN;

PROC PRINT DATA=auc (KEEP = AUC) NOOBS;
TITLE "Aire sous la courbe ROC de &data";
WHERE AUC > .;
RUN;

TITLE " ";

%MEND AUC;


%MACRO AUC2(data,cible,score);

ODS OUTPUT WilcoxonScores = wilcoxon;
                                          
PROC NPAR1WAY WILCOXON DATA=&data CORRECT=no;
CLASS &cible;
VAR &score;
RUN;

DATA auc;
SET wilcoxon;
n0 = N; R0 = SumOfScores ;
n1 = LAG(N); R1 = LAG(SumOfScores) ;
U1 = (n1*n0) + (n1*(n1+1)/2) - R1 ;
U0 = (n1*n0) + (n0*(n0+1)/2) - R0 ;
AUC = ROUND(MAX(U1,U0)/(n1*n0),0.001) ;
RUN;

PROC PRINT DATA=auc (KEEP = AUC) NOOBS;
TITLE "Aire sous la courbe ROC de &data";
WHERE AUC > .;
RUN;

TITLE " ";

%MEND AUC2;



/* ==================================================================== */
/* SECTION 2.18
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* ANALYSE DISCRIMINANTE LINEAIRE
/* -------------------------------------------------------------------- */


/* SECTION 2.18.1 */
/* Analyse discriminante sur l'ensemble des variables */

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes CROSSVALIDATE ALL CANONICAL
OUT=scores OUTSTAT=statdescr TESTDATA=valid TESTOUT=validout ;
CLASS assur_caravane ;
PRIORS prop ;
VAR &varsociodemo &varmontant ;
RUN;

%AUC2(scores,assur_caravane,_1);
%AUC2(validout,assur_caravane,_1);


/* SECTION 2.18.2 */
/* Analyse discriminante sur une sélection de 21 variables */

PROC TRANSPOSE DATA = statdescr OUT = coeff_discrim ;
WHERE _type_ = "SCORE";
RUN ;

DATA coeff_discrim ;
 SET coeff_discrim ;
Coeff = ABS(Can1) ;
RUN ;

PROC SORT DATA=coeff_discrim ; BY DESCENDING Coeff; RUN;

PROC PRINT DATA=coeff_discrim ;
RUN ;

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes crossvalidate all canonical
OUT= scores OUTSTAT=statdescr TESTDATA=valid TESTOUT=validout ;
CLASS assur_caravane ;
PRIORS prop;
VAR locataire assur_sante_prive assur_sante_public proprietaire mt_auto marie niv_etud_bas
mt_bateau autre_relation mt_incendie mt_securite_soc PCSinter mt_invalidite revenu1
sans_enfant revenu2 age_moyen auto0 niv_etud_moy mt_velo PCSouvr ;
RUN;

%AUC2(scores,assur_caravane,_1);
%AUC2(validout,assur_caravane,_1);


/* SECTION 2.18.3 */
/* Analyse discriminante sur les 10 premières variables de la liste précédente */

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes crossvalidate all canonical
OUT= scores OUTSTAT=statdescr TESTDATA=valid TESTOUT=validout ;
CLASS assur_caravane ;
PRIORS prop ;
VAR locataire assur_sante_prive assur_sante_public proprietaire mt_auto
marie niv_etud_bas mt_bateau autre_relation mt_incendie ;
RUN ;

%AUC2(scores,assur_caravane,_1);
%AUC2(validout,assur_caravane,_1);


/* SECTION 2.18.4 */
/* Analyse discriminante sur les 21 variables précédemment sélectionnées par la PROC STEPDISC */

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes crossvalidate all canonical
OUT= scores OUTSTAT=statdescr TESTDATA=valid TESTOUT=validout ;
CLASS assur_caravane ;
PRIORS prop;
VAR &vardiscrim ;
RUN;

%AUC2(scores,assur_caravane,_1);
%AUC2(validout,assur_caravane,_1);


/* SECTION 2.18.5 */
/* Analyse discriminante sur les 10 variables précédemment sélectionnées par la PROC STEPDISC */

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes crossvalidate all canonical
OUT= scores OUTSTAT=statdescr TESTDATA=valid TESTOUT=validout ;
CLASS assur_caravane ;
PRIORS prop;
VAR mt_auto pouvoir_achat mt_bateau mt_RC niv_etud_bas marie mt_incendie mt_securite_soc PCSagri
mt_velo ;
RUN;

%AUC2(scores,assur_caravane,_1);
%AUC2(validout,assur_caravane,_1);


/* SECTION 2.18.6 */
/* Analyse discriminante sur les 6 premières variables sélectionnées par la PROC STEPDISC */

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes crossvalidate all canonical
OUT= scores OUTSTAT=statdescr TESTDATA=valid TESTOUT=validout ;
CLASS assur_caravane ;
PRIORS prop;
VAR mt_auto pouvoir_achat mt_bateau mt_RC niv_etud_bas marie ;
RUN;

%AUC2(scores,assur_caravane,_1);
%AUC2(validout,assur_caravane,_1);



/* ==================================================================== */
/* SECTION 2.19
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* CLASSEMENT PAR ARBRE DE DECISION : MACRO TREEDISC DE SAS
/* -------------------------------------------------------------------- */


/* Il faut exécuter auparavant les macros XMACRO et TREEDISC */
%INC "&C\xmacro.sas" ;
%INC "&C\treedisc.sas" ;

%TREEDISC( data=apprent, depvar=assur_caravane, ordinal=&varmontant &varsociodemo,
            outtree=arbre, branch=100, leaf=50, maxdepth=3, options=noformat, trace=short);

/* Trace l'arbre  : cette fonctionnalité nécessite le module SAS/OR pour la PROC NETDRAW */
%TREEDISC(intree=arbre, draw=lp);
%TREEDISC(intree=arbre, draw=graphics);

/* Génère le code SAS pour un classement des observations */
%TREEDISC( intree=arbre, code=print);

/* Sauvegarde le code dans un fichier nommé arbre.code */
%TREEDISC( intree=arbre, code="&C\chaid.code")

/* Application du classement par arbre à l'échantillon d'apprentissage */
DATA arbre_apprent ;
   SET apprent ;
   %INC "&C\chaid.code" ;
   IF into_ = 0 THEN proba = 1-post_ ; ELSE proba = post_ ;
RUN ;

/* Application du classement par arbre à l'échantillon de validation */
DATA arbre_valid ;
   SET valid ;
   %INC "&C\chaid.code" ;
   IF into_ = 0 THEN proba = 1-post_ ; ELSE proba = post_ ;
RUN ;

%AUC2(arbre_apprent,assur_caravane,proba);
%AUC2(arbre_valid,assur_caravane,proba);



/* ==================================================================== */
/* SECTION 2.20
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* REGRESSION LOGISTIQUE SUR VARIABLES QUANTITATIVES
/* -------------------------------------------------------------------- */


/* SECTION 2.20.1 et 2.20.2 */
/* Syntaxe de base de la régression logistique (sur toutes les variables) */

PROC LOGISTIC DATA=test ;
MODEL cible (ref='0') = &var / SELECTION=stepwise RSQUARE ;
OUTPUT OUT=modele1 PREDICTED=proba1 ;
RUN ;

%AUC(modele1,assur_caravane,proba1,cible,0);

PROC LOGISTIC DATA=apprent ;
MODEL assur_caravane (ref='0') = &var /SELECTION=stepwise RSQUARE ;
SCORE DATA=apprent OUT=apprent_score;
SCORE DATA=valid   OUT=valid_score;
RUN ;

%AUC2(apprent_score, assur_caravane,P_1);
%AUC2(valid_score, assur_caravane,P_1);


/* SECTION 2.20.3 */
/* Régression logistique sur toutes les variables avec des seuils plus sévères */

PROC LOGISTIC DATA=test ;
MODEL cible (ref='0') = &var / SELECTION=stepwise SLE=0.01 SLS=0.01 RSQUARE ;
OUTPUT OUT=modele1 PREDICTED=proba1 ;
RUN ;

%AUC(modele1,assur_caravane,proba1,cible,0);


/* SECTION 2.20.4 */
/* Régression logistique avec sélection descendante sur toutes les variables */

PROC LOGISTIC DATA=test ;
MODEL cible (ref='0') = &var / SELECTION=backward SLE=0.01 SLS=0.01 RSQUARE ;
OUTPUT OUT=modele2 PREDICTED=proba2 ;
RUN ;

%AUC(modele2,assur_caravane,proba2,cible,0);


/* SECTION 2.20.5 */
/* Régression logistique sur les 21 variables sélectionnées par la PROC STEPDISC */

PROC LOGISTIC DATA=test ;
MODEL cible (ref='0') = &vardiscrim / SELECTION=stepwise RSQUARE ;
OUTPUT OUT=modele3 PREDICTED=proba3 ;
RUN ;

%AUC(modele3,assur_caravane,proba3,cible,0);


/* SECTION 2.20.6 */
/* Régression logistique sur les 6 premières variables sélectionnées par la PROC STEPDISC */

PROC LOGISTIC DATA=test ;
MODEL cible (ref='0') = mt_auto pouvoir_achat mt_bateau mt_RC niv_etud_bas marie
 / SELECTION=stepwise RSQUARE ;
OUTPUT OUT=modele4 PREDICTED=proba4 ;
RUN ;

%AUC(modele4,assur_caravane,proba4,cible,0);


/* SECTION 2.20.8 */
/* Régression logistique avec effets principaux et interactions */

PROC LOGISTIC DATA=test ;
MODEL cible (ref='0') = mt_auto | pouvoir_achat | mt_bateau | mt_RC | niv_etud_bas | marie @2
/ SELECTION=stepwise RSQUARE ;
OUTPUT OUT=modele4 PREDICTED=proba4 ;
RUN ;



/* ==================================================================== */
/* SECTION 2.21
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* REGRESSION LOGISTIQUE SUR VARIABLES DISCRETISEES
/* -------------------------------------------------------------------- */


/* SECTION 2.21.1 */
/* Syntaxe de base de la régression logistique (sur toutes les variables) */

OPTION FMTSEARCH  = (WORK SASUSER) ;


PROC LOGISTIC DATA=test ;
CLASS &vardiscr &varmontant type_client cluster / PARAM=glm;
MODEL cible (ref='0') = &vardiscr &varmontant type_client cluster / SELECTION=stepwise RSQUARE ;
FORMAT &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie. age_moyen age.  ;
OUTPUT OUT=modele5 PREDICTED=proba5 ;
RUN ;

%AUC(modele5,assur_caravane,proba5,cible,0);


/* SECTION 2.21.2 */
/* Comme précédemment mais avec des seuils de sélection plus sévères */

PROC LOGISTIC DATA=test ;
CLASS &vardiscr &varmontant type_client cluster / PARAM=glm;
MODEL cible (ref='0') = &vardiscr &varmontant type_client cluster 
/ SELECTION=stepwise SLE=0.01 SLS=0.01 RSQUARE ;
FORMAT &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie. age_moyen age.  ;
OUTPUT OUT=modele6 PREDICTED=proba6 ;
RUN ;

%AUC(modele6,assur_caravane,proba6,cible,0);


/* SECTION 2.21.3 */
/* Régression logistique sur toutes les variables sélectionnées par tests statistiques */

ODS OUTPUT FitStatistics = stats_modele_tmp ;

PROC LOGISTIC DATA=test ;
CLASS &vardiscr &varmontant type_client cluster / PARAM=glm;
MODEL cible (ref='0') = &varselect 
/ SELECTION=stepwise SLE=0.01 SLS=0.01 RSQUARE OUTROC = roc7 ;
FORMAT &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie. age_moyen age. ;
OUTPUT out=modele7 PREDICTED=proba7 ;
RUN ;

%AUC(modele7,assur_caravane,proba7,cible,0);


/* SECTION 2.21.4 */
/* Autre mode de sélection du meilleur modèle */

ODS OUTPUT FitStatistics = stats_modele_tmp ;

PROC LOGISTIC DATA=test ;
CLASS &vardiscr &varmontant type_client cluster / PARAM=glm;
MODEL cible (ref='0') = &varselect 
/ SELECTION=stepwise SLE=1 SLS=1 RSQUARE OUTROC = roc7 ;
FORMAT &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie. age_moyen age. ;
OUTPUT out=modele7 PREDICTED=proba7 ;
RUN ;

SYMBOL1 v=CIRCLE i=JOIN c=BLACK;
SYMBOL2 v=SQUARE i=JOIN c=BLACK;
SYMBOL3 v=TRIANGLE i=JOIN c=BLACK;
AXIS LABEL = (ANGLE = 90) ;
PROC GPLOT DATA=stats_modele_tmp ;
WHERE Step > 0 ;
PLOT InterceptAndCovariates*step = Criterion / HMINOR = 1 VAXIS=axis ;
RUN ;
QUIT ;


/* SECTION 2.21.5 */
/* Affinage du meilleur modèle */

PROC FREQ DATA=test ;
TABLES (revenu_moyen niv_etud_bas mt_auto mt_invalidite mt_incendie mt_bateau type_client) * assur_caravane / NOCOL;
FORMAT &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie. ;
RUN ;


PROC FORMAT ;
VALUE niv_etud
0-2    = '0 à 2'
3-high = '3 et plus' ;
VALUE cotis_incendie_bis
0-2    = '    <= 99 ou > 499'
3      = '100 - 199'
4      = '200 - 499'
5-high = '    <= 99 ou > 499' ;
RUN ;


ODS HTML;
ODS GRAPHICS ON;

ODS OUTPUT ParameterEstimates=coeff_logit  ;

PROC LOGISTIC DATA=test ;
CLASS &vardiscr &varmontant / PARAM=glm;
MODEL cible (ref='0') = mt_auto mt_incendie revenu_moyen niv_etud_bas /selection=stepwise sle=0.01 sls=0.01 rsquare OUTROC = roc8 ;
FORMAT &vardiscrformat mt_auto cotis_auto. mt_incendie cotis_incendie_bis. niv_etud_bas niv_etud. ;
OUTPUT OUT=modele8 PREDICTED=proba8 ;
RUN ;

ODS GRAPHICS OFF;
ODS HTML CLOSE; 

%AUC(modele8,assur_caravane,proba8,cible,0);


/* -------------------------------------------------------------------- */
/* Graphique de la densité du score en fonction des valeurs de la variable cible */
/* MACRO-PROGRAMME NON PUBLIE DANS L'OUVRAGE
/* -------------------------------------------------------------------- */

%MACRO scoreDist (table, variableScore, variableY, nbPoints = 30) ;
	PROC SORT DATA = &table ;
		BY &variableY ;
	RUN ;
	ODS EXCLUDE ALL ;
	%IF &sysVer >= 9 %THEN %DO ;
		PROC KDE DATA = &table ;
			UNIVAR &variableScore / OUT = work.distribution NGRID = &nbPoints ;
			%LET variableScore = value ;
	%END ;
	%ELSE %DO ;
		PROC KDE DATA = &table OUT = work.distribution NGRID = &nbPoints ;
			VAR &variableScore ;
	%END ;
			BY &variableY ;
		RUN ;
	ODS SELECT ALL ;
	SYMBOL i = spline ;
	PROC GPLOT DATA = work.distribution ;
		PLOT density * &variableScore = &variableY / HREF = .5 
										   HAXIS = 0 TO 1 BY .2 ;
	RUN ; QUIT ;
%MEND scoreDist ;

%scoreDist(modele8,proba8,assur_caravane,nbPoints =10);


/* SECTION 2.21.6 */
/* Implémentation du modèle logistique */

DATA modele8 ;
 SET modele8 ;
 logit = -2.9411 
+ (mt_auto >= 6) * 1.4911
- (mt_incendie <= 2) *	1.1303
- (mt_incendie >= 5) *	1.1303
- (mt_incendie = 3) * 0.2967
- (revenu_moyen <= 3) * 0.6029
+ (niv_etud_bas <= 2) * 0.5460 ;
score_logit = exp(logit) / (1 + exp(logit));
IF abs(score_logit - proba8) > 0.01 THEN ecart = 1 ;
                                    ELSE ecart = 0 ;
RUN ;

PROC FREQ ; TABLES ecart ; RUN;

PROC CORR DATA=modele8 PEARSON SPEARMAN ;
VAR score_logit proba8 ;
RUN ;

%AUC(modele8,assur_caravane,score_logit,cible,0);
%AUC(modele8,assur_caravane,proba8,cible,0);


/*
PROC SQL ;
SELECT CATT(estimate,'* (',TRANWRD(variable,'Intercept','1'),ClassVal0,' )')
INTO : AppliqModele separated by ' + '
FROM coeff_logit ;
QUIT ;
%PUT AppliqModele =&AppliqModele ;


DATA modele8 ;
 SET modele8 ;
 logit = &AppliqModele ;
score_logit = exp(logit) / (1 + exp(logit));
IF abs(score_logit - proba8) > 0.0001 THEN ecart = 1 ;
                                      ELSE ecart = 0 ;
RUN ;
*/



/* ==================================================================== */
/* SECTION 2.22
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Voir après la section 2.23 car on utilisera le résultat du bagging
/* dans la section 2.22 (contrairement à l'ouvrage qui ne compare
/* dans la section 2.22 que les trois autres modèles)
/* -------------------------------------------------------------------- */



/* ==================================================================== */
/* SECTION 2.23
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* Bagging sur arbres de décision
/* -------------------------------------------------------------------- */


%MACRO Bagging (apprent, valid , varY , varXordinal , varXnominal , min_feuille, n ) ;

%INC "&C\xmacro.sas" ;
%INC "&C\treedisc.sas" ;

PROC SQL NOPRINT ;
SELECT COUNT(*) INTO :sample FROM &apprent;
QUIT ;

%PUT &sample ;

%DO iter = 1 %TO &n ;

PROC SURVEYSELECT DATA = &apprent METHOD = urs OUT = &apprent._tmp SAMPSIZE = &sample OUTHITS NOPRINT ;
RUN ;

%TREEDISC( data=&apprent._tmp, depvar=&varY, ordinal=&varXordinal, nominal=&varXnominal,
            outtree=modele, leaf=&min_feuille, maxdepth=3, options=noformat, trace=none);

%TREEDISC( intree=modele, code="&C\arbre.code")

DATA arbre&iter (KEEP = cle &varY proba) ;
   SET &valid ;
   %INC "&C\arbre.code" ;
   IF into_ = 0 THEN proba = 1-post_ ; ELSE proba = post_ ;
RUN ;

%IF &iter = 1 %THEN %DO ;
 DATA bagging_tmp ;
  SET arbre&iter ;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = bagging_tmp
  DATA = arbre&iter ;
 RUN ;
%END ;

PROC SORT DATA=bagging_tmp ; BY cle ; RUN ;

PROC SUMMARY DATA = bagging_tmp NWAY ;
CLASS cle ;
OUTPUT OUT = bagging_agreg (drop = _type_ _freq_) MEAN(proba)= MEAN(&varY)=;
RUN ;

ODS EXCLUDE ALL ;
ODS OUTPUT WilcoxonScores = wilcoxon;
PROC NPAR1WAY WILCOXON DATA=bagging_agreg CORRECT=no;
CLASS &varY;
VAR proba;
RUN;
ODS SELECT ALL ;

DATA auc_tmp (KEEP = auc) ;
SET wilcoxon;
n0 = N; R0 = SumOfScores ;
n1 = LAG(N); R1 = LAG(SumOfScores) ;
U1 = (n1*n0) + (n1*(n1+1)/2) - R1 ;
U0 = (n1*n0) + (n0*(n0+1)/2) - R0 ;
AUC = ROUND(MAX(U1,U0)/(n1*n0),0.001) ;
RUN ;

%IF &iter = 1 %THEN %DO ;
 DATA auc_out ;
  SET auc_tmp ;
  WHERE AUC > .;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = auc_out
  DATA = auc_tmp ;
  WHERE AUC > .;
 RUN ;
%END ;

%END ;

PROC SORT DATA=bagging_tmp ; BY cle ; RUN ;

PROC SUMMARY DATA = bagging_tmp NWAY ;
CLASS cle ;
OUTPUT OUT = bagging_out (drop = _type_ rename = (_freq_ = count)) MEAN(proba)= STD(proba) = proba_std MEAN(&varY)=;
RUN ;

%MEND Bagging ;

%Bagging (apprent, valid , assur_caravane , &varmontant &varsociodemo , type_client cluster , 50, 100);



/* -------------------------------------------------------------------- */
/* Bagging sur analyse discriminante linéaire
/* VARIANTE NON PUBLIEE DANS L'OUVRAGE
/* -------------------------------------------------------------------- */


%MACRO BaggingADL (apprent, valid , varY , varX , n ) ;

PROC SQL NOPRINT ;
SELECT COUNT(*) INTO :sample FROM &apprent;
QUIT ;

%PUT &sample ;

%DO iter = 1 %TO &n ;

PROC SURVEYSELECT DATA = &apprent METHOD = urs OUT = &apprent._tmp SAMPSIZE = &sample OUTHITS NOPRINT ;
RUN ;

PROC DISCRIM DATA=&apprent._tmp METHOD=normal POOL=yes crossvalidate all canonical NOPRINT
OUT= scores OUTSTAT=statdescr TESTDATA=&valid TESTOUT=adl&iter (KEEP = cle &varY _1 RENAME=(_1=proba)) ;
CLASS &varY ;
PRIORS prop;
VAR &varX ;
RUN;

%IF &iter = 1 %THEN %DO ;
 DATA bagging_tmp ;
  SET adl&iter ;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = bagging_tmp
  DATA = adl&iter ;
 RUN ;
%END ;

PROC SORT DATA=bagging_tmp ; BY cle ; RUN ;

PROC SUMMARY DATA = bagging_tmp NWAY ;
CLASS cle ;
OUTPUT OUT = bagging_agreg (drop = _type_ _freq_) MEAN(proba)= MEAN(&varY)=;
RUN ;

ODS EXCLUDE ALL ;
ODS OUTPUT WilcoxonScores = wilcoxon;
PROC NPAR1WAY WILCOXON DATA=bagging_agreg CORRECT=no;
CLASS &varY;
VAR proba;
RUN;
ODS SELECT ALL ;

DATA auc_tmp (KEEP = auc) ;
SET wilcoxon;
n0 = N; R0 = SumOfScores ;
n1 = LAG(N); R1 = LAG(SumOfScores) ;
U1 = (n1*n0) + (n1*(n1+1)/2) - R1 ;
U0 = (n1*n0) + (n0*(n0+1)/2) - R0 ;
AUC = ROUND(MAX(U1,U0)/(n1*n0),0.001) ;
RUN ;

%IF &iter = 1 %THEN %DO ;
 DATA auc_out ;
  SET auc_tmp ;
  WHERE AUC > .;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = auc_out
  DATA = auc_tmp ;
  WHERE AUC > .;
 RUN ;
%END ;

%END ;

PROC SORT DATA=bagging_tmp ; BY cle ; RUN ;

PROC SUMMARY DATA = bagging_tmp NWAY ;
CLASS cle ;
OUTPUT OUT = bagging_out (drop = _type_ rename = (_freq_ = count)) MEAN(proba)= STD(proba) = proba_std MEAN(&varY)=;
RUN ;

%AUC2(bagging_out,&varY,proba);

%MEND BaggingADL ;

%BaggingADL (apprent, valid , assur_caravane , mt_auto pouvoir_achat mt_bateau mt_RC niv_etud_bas marie , 100);



/* -------------------------------------------------------------------- */
/* Bagging sur régression logistique
/* VARIANTE NON PUBLIEE DANS L'OUVRAGE
/* -------------------------------------------------------------------- */


PROC FORMAT ;
VALUE niv_etud
0-2    = '0 à 2'
3-high = '3 et plus' ;
VALUE cotis_incendie_bis
0-2    = '    <= 99 ou > 499'
3      = '100 - 199'
4      = '200 - 499'
5-high = '    <= 99 ou > 499' ;
RUN ;


%MACRO BaggingLogit (data , varY , varYappr , varX , varXclasse , selection, n ) ;

PROC SQL NOPRINT ;
SELECT COUNT(*) INTO :sample FROM &data WHERE &varYappr > . ;
QUIT ;

%PUT &sample ;

%DO iter = 1 %TO &n ;

DATA apprent_tmp ;
 SET &data ;
 WHERE &varYappr > . ;
 RUN ;

 DATA valid_tmp ;
 SET &data ;
 WHERE &varYappr = . ;
 RUN ;

PROC SURVEYSELECT DATA = apprent_tmp METHOD = urs OUT = &data._tmp SAMPSIZE = &sample OUTHITS NOPRINT ;
RUN ;

DATA &data._tmp ;
 SET &data._tmp valid_tmp ;
 RUN ;

PROC LOGISTIC DATA=&data._tmp NOPRINT ;
CLASS &varXclasse / PARAM=glm;
MODEL &varYappr (ref='0') = &varX /selection=&selection sle=0.01 sls=0.01 rsquare ;
FORMAT &vardiscrformat type_client typeclient.
&varmontant supzero. mt_auto cotis_auto. mt_incendie cotis_incendie_bis. niv_etud_bas niv_etud. age_moyen age. ;
OUTPUT out=logit (KEEP = cle &varY &varYappr proba) predicted=proba ;
RUN ;

/* Application du classement à l'échantillon de validation */
DATA logit&iter (DROP = &varYappr) ;
 SET logit ;
 WHERE &varYappr = . ;
 RUN ;

%IF &iter = 1 %THEN %DO ;
 DATA bagging_tmp ;
  SET logit&iter ;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = bagging_tmp
  DATA = logit&iter ;
 RUN ;
%END ;

PROC SORT DATA=bagging_tmp ; BY cle ; RUN ;

PROC SUMMARY DATA = bagging_tmp NWAY ;
CLASS cle ;
OUTPUT OUT = bagging_agreg (drop = _type_ _freq_) MEAN(proba)= MEAN(&varY)=;
RUN ;

ODS EXCLUDE ALL ;
ODS OUTPUT WilcoxonScores = wilcoxon;
PROC NPAR1WAY WILCOXON DATA=bagging_agreg CORRECT=no;
CLASS &varY;
VAR proba;
RUN;
ODS SELECT ALL ;

DATA auc_tmp (KEEP = auc) ;
SET wilcoxon;
n0 = N; R0 = SumOfScores ;
n1 = LAG(N); R1 = LAG(SumOfScores) ;
U1 = (n1*n0) + (n1*(n1+1)/2) - R1 ;
U0 = (n1*n0) + (n0*(n0+1)/2) - R0 ;
AUC = ROUND(MAX(U1,U0)/(n1*n0),0.001) ;
RUN ;

%IF &iter = 1 %THEN %DO ;
 DATA auc_out ;
  SET auc_tmp ;
  WHERE AUC > .;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = auc_out
  DATA = auc_tmp ;
  WHERE AUC > .;
 RUN ;
%END ;

%END ;

PROC SORT DATA=bagging_tmp ; BY cle ; RUN ;

PROC SUMMARY DATA = bagging_tmp NWAY ;
CLASS cle ;
OUTPUT OUT = bagging_out (drop = _type_ rename = (_freq_ = count)) MEAN(proba)= STD(proba) = proba_std MEAN(&varY)=;
RUN ;

%MEND BaggingLogit ;

%BaggingLogit (test , assur_caravane , cible, &vardiscr &varmontant type_client cluster , &vardiscr &varmontant type_client cluster , stepwise, 100);



/* ==================================================================== */
/* SECTION 2.22
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* CROISEMENT DES SCORES : DISCRIMINANT, CHAID, LOGISTIQUE
/* ET BAGGING (EN VALIDATION)
/* -------------------------------------------------------------------- */


/* -------------------------------------------------------------------- */
/* Modèles retenus pour chaque méthode
/* Recalcul sur l'ensemble de la population
/* -------------------------------------------------------------------- */

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes CROSSVALIDATE ALL CANONICAL
OUT= scores OUTSTAT=statdescr TESTDATA=test TESTOUT=modele_discr ;
CLASS assur_caravane ;
PRIORS prop;
VAR mt_auto pouvoir_achat mt_bateau mt_RC niv_etud_bas marie ;
RUN;

DATA modele_arbre (keep = cle proba) ;
   SET test ;
   %INC "&C\chaid.code" ;
   IF into_ = 0 THEN proba = 1-post_ ; ELSE proba = post_ ;
RUN ;

PROC SORT DATA = modele_discr ; BY cle ; RUN ;
PROC SORT DATA = modele_arbre ; BY cle ; RUN ;
PROC SORT DATA = modele8 ; BY cle ; RUN ;

DATA compscores ;
MERGE modele_discr (KEEP = cle _1 assur_caravane)
	  modele_arbre (KEEP = cle proba )
      modele8      (KEEP = cle proba8) ;
BY cle ;
RUN ;

ODS HTML ;
ODS GRAPHICS ON ;
PROC CORR DATA = compscores PEARSON SPEARMAN PLOTS = (MATRIX SCATTER) ;
VAR _1 proba proba8 ;
RUN ;
ODS GRAPHICS OFF ;
ODS HTML CLOSE ;

/* AUC des trois modèles sur l'ensemble de la population */
%AUC2(compscores,assur_caravane,_1); /* AUC = 0,746 */
%AUC2(compscores,assur_caravane,proba); /* AUC = 0,781 */
%AUC2(compscores,assur_caravane,proba8); /* AUC = 0,766 */



/* -------------------------------------------------------------------- */
/* Modèles retenus pour chaque méthode
/* Recalcul sur l'échantillon de validation
/* -------------------------------------------------------------------- */

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes CROSSVALIDATE ALL CANONICAL
OUT= scores OUTSTAT=statdescr TESTDATA=valid TESTOUT=validout ;
CLASS assur_caravane ;
PRIORS prop;
VAR mt_auto pouvoir_achat mt_bateau mt_RC niv_etud_bas marie ;
RUN;

DATA arbre_valid ;
   SET valid ;
   %INC "&C\chaid.code" ;
   IF into_ = 0 THEN proba = 1-post_ ; ELSE proba = post_ ;
run;

PROC SORT DATA = validout ; BY cle ; RUN ;
PROC SORT DATA = arbre_valid ; BY cle ; RUN ;
PROC SORT DATA = modele8 ; BY cle ; RUN ;
PROC SORT DATA = bagging_out ; BY cle ; RUN ;

DATA compscores ;
MERGE validout    (IN=a KEEP = cle _1 assur_caravane)
	  arbre_valid (IN=b KEEP = cle proba )
      modele8     (IN=c KEEP = cle proba8) 
      bagging_out (IN=d KEEP = cle proba RENAME=(proba=proba_bagging)) ;
BY cle ;
IF a AND b AND c AND d ;
LABEL _1 = 'ADL' proba = 'CHAID' proba8 = 'LOGIT' proba_bagging = 'BAGGING' ;
RUN ;

ODS HTML ;
ODS GRAPHICS ON ;
PROC CORR DATA = compscores PEARSON SPEARMAN PLOTS = (MATRIX SCATTER) ;
VAR _1 proba proba8 proba_bagging ;
RUN ;
ODS GRAPHICS OFF ;
ODS HTML CLOSE ;

/* AUC des quatre modèles sur l'échantillon de validation */
%AUC2(compscores,assur_caravane,_1); /* AUC = 0,745 */
%AUC2(compscores,assur_caravane,proba); /* AUC = 0,737 */
%AUC2(compscores,assur_caravane,proba8); /* AUC = 0,744 */
%AUC2(compscores,assur_caravane,proba_bagging); /* AUC = 0,759 environ */



/* -------------------------------------------------------------------- */
/* Modèles retenus pour chaque méthode
/* Recalcul sur l'échantillon de validation de 4000 clients (voir la section 2.29)
/* CODE SAS NON PRESENTE DANS L'OUVRAGE */
/* -------------------------------------------------------------------- */

DATA validation ;
 SET &ASSURANC..assurancetic_validation ;
RUN ;

PROC DISCRIM DATA=apprent METHOD=normal POOL=yes crossvalidate all canonical
OUT= scores OUTSTAT=statdescr TESTDATA=validation TESTOUT=adl_validation ;
CLASS assur_caravane ;
PRIORS prop;
VAR mt_auto pouvoir_achat mt_bateau mt_RC niv_etud_bas marie ;
RUN;

DATA arbre_validation ;
   SET validation ;
   %INC "&C\chaid.code" ;
   IF into_ = 0 THEN proba = 1-post_ ; ELSE proba = post_ ;
run;

DATA logit_validation ;
 SET validation ;
 nbpoints = SUM (40*(mt_auto >= 6),22*(mt_incendie = 3),30*(mt_incendie = 4),
  16*(revenu_moyen > 3),14*(niv_etud_bas <= 2)) ;
 logit = -2.9411 + (mt_auto >= 6) * 1.4911
- (mt_incendie <= 2) *	1.1303
- (mt_incendie >= 5) *	1.1303
- (mt_incendie = 3) * 0.2967
- (revenu_moyen <= 3) * 0.6029
+ (niv_etud_bas <= 2) * 0.5460 ;
score_logit = exp(logit) / (1 + exp(logit));
RUN ;

%Bagging (apprent, validation , assur_caravane , &varmontant &varsociodemo , type_client cluster , 50, 100);

PROC SORT DATA = adl_validation ; BY cle ; RUN ;
PROC SORT DATA = arbre_validation ; BY cle ; RUN ;
PROC SORT DATA = logit_validation ; BY cle ; RUN ;
PROC SORT DATA = bagging_out ; BY cle ; RUN ;

DATA compscores_valid ;
MERGE adl_validation    (IN=a KEEP = cle _1 assur_caravane)
      arbre_validation  (IN=b KEEP = cle proba )
      logit_validation  (IN=c KEEP = cle nbpoints score_logit)
      bagging_out       (IN=d KEEP = cle proba RENAME=(proba=proba_bagging)) ;
BY cle ;
IF a AND b AND c AND d ;
LABEL _1 = 'ADL' proba = 'CHAID' proba8 = 'LOGIT' proba_bagging = 'BAGGING' ;
RUN ;

ODS HTML ;
ODS GRAPHICS ON ;
PROC CORR DATA = compscores_valid PEARSON SPEARMAN PLOTS = (MATRIX SCATTER) ;
VAR _1 proba nbpoints score_logit proba_bagging ;
RUN ;
ODS GRAPHICS OFF ;
ODS HTML CLOSE ;

/* AUC des quatre modèles sur l'échantillon de validation */
%AUC2(compscores_valid,assur_caravane,_1); /* AUC = 0,702 */
%AUC2(compscores_valid,assur_caravane,proba); /* AUC = 0,682 */
%AUC2(compscores_valid,assur_caravane,nbpoints); /* AUC = 0,714 */
%AUC2(compscores_valid,assur_caravane,score_logit); /* AUC = 0,714 */
%AUC2(compscores_valid,assur_caravane,proba_bagging); /* AUC = 0,722 environ */


/* -------------------------------------------------------------------- */
/* Evaluation des résultats comme dans le concours TIC de 2000
/* Sur l'échantillon de validation de 4000 clients (voir la section 2.29)
/* CODE SAS NON PRESENTE DANS L'OUVRAGE */
/* -------------------------------------------------------------------- */

PROC SORT DATA=compscores OUT=meilleurs ;
BY DESCENDING /* score_logit */ proba_bagging   ;
RUN;

DATA meilleurs ;
 SET meilleurs ;
 IF _n_ <= 800 ;
 run;

PROC FREQ DATA=meilleurs ;
TABLE assur_caravane ;
RUN ;
/* les 800 meilleurs scorés par régression logistique contiennent 119 souscripteurs */
/* les 800 meilleurs scorés par bagging CHAID contiennent environ 117 souscripteurs */
/* pour mémoire, le concours TIC avait un 1er à 121, un 2e à 115 et plusieurs 3e ex aequo 
à 112 souscripteurs */



/* ==================================================================== */
/* SECTION 2.24
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* COURBE DE LIFT ET COURBE ROC
/* -------------------------------------------------------------------- */


%MACRO Courbelift (table, variableScore, variableY, ref , quantiles) ;

PROC SQL NOPRINT ;
		SELECT ROUND(MEAN(&variableY = &ref)*100,5)
			INTO : probaEvent
		FROM &table ;
QUIT ;

%PUT Scores : &variableScore ;

%LET nb_score = %EVAL(%SYSFUNC(COUNTC(%SYSFUNC(COMPBL(&variableScore)),' ')) + 1);

%PUT Nombre de scores traités : &nb_score ;

%DO i = 1 %TO &nb_score ;

%LET score = %SCAN(&variableScore,&i);

PROC RANK DATA = &table GROUPS = &quantiles OUT = quantiles_tmp DESCENDING ;
VAR &score ;
RUN ;

PROC FREQ DATA = quantiles_tmp NOPRINT ;
TABLE &score * &variableY / OUT=pct&i OUTPCT ;
RUN ;

DATA lift&i (KEEP = fct_score lift modele);
 SET pct&i END = fin;
 WHERE &variableY = &ref AND pct_col > .;
 fct_score = (&score + 1)*(100/&quantiles);

 liftM + pct_col ;
 lift = liftM ;
 LENGTH modele $20. ;
 modele = "MODELE &i : &score" ;
 OUTPUT ;
 lift = MIN(fct_score * 100 / &probaEvent, 100) ;
 modele = "PARFAIT  " ;
 OUTPUT ;
 lift = fct_score ;
 modele = "ALEATOIRE" ;
 OUTPUT ;

 IF fin THEN DO ;
	fct_score = 0 ;
	lift = 0 ;
	modele = "MODELE &i : &score " ;
	OUTPUT ;
	modele = "PARFAIT  " ;
	OUTPUT ;
	modele = "ALEATOIRE " ;
	OUTPUT ;
	END ;

RUN ;

DATA roc&i (KEEP = vpos fpos modele);
 SET pct&i END = fin;
 WHERE pct_col > .;
 
 RETAIN vpos fpos ;
 IF &variableY = &ref THEN DO;
   vposM + pct_col ;
   vpos = vposM ;
 END ;
 IF &variableY NE &ref THEN DO;
   fposM + pct_col ;
   fpos = fposM ;
 END ;
 LENGTH modele $20. ;
 modele = "MODELE &i : &score" ;
 IF MOD(_n_,2) = 0 THEN OUTPUT ;

 IF fin THEN DO ;
	vpos = 0 ;
	fpos = 0 ;
	modele = "MODELE &i : &score " ;
	OUTPUT ;
	modele = "ALEATOIRE " ;
	OUTPUT ;
	END ;
 IF fin THEN DO ;
	vpos = 100 ;
	fpos = 100 ;
	modele = "ALEATOIRE " ;
	OUTPUT ;
	END ;

	RUN ;


%IF &i = 1 %THEN %DO ;
 DATA lift_tmp ;
  SET lift&i ;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = lift_tmp
  DATA = lift&i ;
 RUN ;
%END ;

%IF &i = 1 %THEN %DO ;
 DATA roc_tmp ;
  SET roc&i ;
 RUN ;
%END ;
%ELSE %DO ;
 PROC APPEND BASE = roc_tmp
  DATA = roc&i ;
 RUN ;
%END ;

%END ;


PROC SORT DATA=lift_tmp;
BY  fct_score ;
RUN ;

PROC SORT DATA=roc_tmp;
BY  fpos ;
RUN ;

GOPTIONS RESET=all; QUIT;
SYMBOL i = join ;
/*SYMBOL1 v=DOT i=JOIN c=BLACK;
SYMBOL2 v=SQUARE i=JOIN c=BLACK;
SYMBOL3 v=TRIANGLE i=JOIN c=BLACK;
SYMBOL4 v=CIRCLE i=JOIN c=BLACK;
SYMBOL5 v=DIAMOND i=JOIN c=BLACK;
SYMBOL6 v=STAR i=JOIN c=BLACK;*/
PROC GPLOT DATA = lift_tmp ;
   TITLE 'Courbe de lift' ;
   PLOT lift * fct_score = modele  ;
   LABEL lift = "% positifs" fct_score = "% population par score décroissant" ;
RUN ;
PROC GPLOT DATA = roc_tmp ;
   TITLE 'Courbe ROC' ;
   PLOT vpos * fpos = modele  ;
   LABEL vpos = "% vrais positifs" fpos = "% faux positifs" ;
RUN ;
QUIT ;


%MEND Courbelift ;


%Courbelift (compscores , _1 proba proba8 proba_bagging , assur_caravane , 1 , 20 ) ;



/* ==================================================================== */
/* SECTION 2.26
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* COURBE ROC
/* -------------------------------------------------------------------- */


DATA roc ;
 SET roc8 ;
 BY _step_ ;
 OUTPUT ;
IF last._step_ THEN DO ;
_sensit_ = 0 ; _1mspec_ = 0 ; OUTPUT ;
END ;
RUN;

SYMBOL1 i=join v=none c=black;
PROC GPLOT DATA=roc ;
  /*   WHERE _step_ IN (1 2); */
     TITLE 'Courbe ROC' ;
     PLOT _sensit_*_1mspec_=1 / VAXIS=0 TO 1 BY .1 ;
RUN;
QUIT;



/* ==================================================================== */
/* SECTION 2.27
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* CALCUL DE LA GRILLE DE SCORE
/* -------------------------------------------------------------------- */


/*
PROC SQL  ;
CREATE TABLE coeff_tmp
AS SELECT Variable , ClassVal0, Estimate , Estimate - MIN(Estimate) AS delta_coeff FROM coeff_logit GROUP BY Variable ;
SELECT SUM(delta_coeff) INTO : total_poids FROM coeff_tmp 
WHERE delta_coeff IN (SELECT delta_coeff FROM coeff_tmp GROUP BY Variable HAVING delta_coeff = MAX(delta_coeff)) ;
QUIT ;
%PUT &total_poids ;
*/


PROC SORT DATA=coeff_logit OUT=coeff_tmp ;
BY Variable Estimate ;
RUN ;

DATA coeff_tmp2 ;
 SET coeff_tmp END=fin ;
BY Variable ;
RETAIN min max delta_tot 0;
IF first.Variable THEN DO min = Estimate ; max = 0 ; END ;
delta_coeff = SUM(Estimate , - min) ;
IF delta_coeff > max THEN max = delta_coeff ;
IF last.Variable THEN delta_tot = SUM(delta_tot , max) ;
IF fin THEN CALL SYMPUT('total_poids',delta_tot) ;
RUN ;

DATA grille (DROP = min delta_tot delta_coeff) ;
 SET coeff_tmp2 ;
nbpoints = ROUND (100 * (Estimate - min) / &total_poids) ;
RUN ;

PROC PRINT DATA=grille ;
RUN ;


DATA modele8 ;
 SET modele8 ;
 nbpoints = SUM (40*(mt_auto >= 6),22*(mt_incendie = 3),30*(mt_incendie = 4),
  16*(revenu_moyen > 3),14*(niv_etud_bas <= 2)) ;
RUN ;

GOPTIONS RESET=all; QUIT;
PROC GPLOT DATA = modele8 ;
PLOT nbpoints * proba8 proba8*nbpoints ;
RUN ;
QUIT ;

PROC CORR DATA = modele8 PEARSON SPEARMAN ;
VAR proba8 score_logit nbpoints ;
RUN ;

%AUC(modele8,assur_caravane,nbpoints,cible,0);


PROC RANK DATA = modele8 GROUPS = 10 OUT = deciles_score ;
VAR proba8 nbpoints ;
RUN ;

PROC FREQ DATA = deciles_score ;
TABLES nbpoints * proba8 / MISSING NOCOL NOROW NOPERCENT ;
RUN;



/* ==================================================================== */
/* SECTION 2.28
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* DECOUPAGE DE LA GRILLE DE SCORE
/* -------------------------------------------------------------------- */


PROC FREQ DATA = deciles_score ;
TABLES nbpoints * assur_caravane / NOCOL NOPERCENT ;
RUN;

%Distrivar(modele8,assur_caravane,nbpoints);

PROC FORMAT ;
VALUE nbpoints
0-45  = ' 0 à 45 points'
46-60 = '46 à 60 points'
61-95 = '61 à 95 points'
96-high = ' > 95 points';
RUN ;

PROC FREQ DATA = modele8 ;
TABLES nbpoints * assur_caravane ;
FORMAT nbpoints nbpoints. ;
RUN;


/* ==================================================================== */
/* SECTION 2.29
/* ==================================================================== */


/* FERMETURE DU FICHIER ODS CONTENANT LES RESULTATS */

ODS RTF CLOSE ;



/* © 2009 www.editionstechnip.com ETUDE DE CAS EN STATISTIQUE DECISIONNELLE par Stéphane Tufféry */

