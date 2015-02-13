# -*- coding: utf-8 -*-

require_relative 'poem'
require_relative 'ruby2xml'

$aragon_metadata = {
  :collection => 'La Diane française', :author_lastname => 'Aragon',
  :author_firstname => 'Louis', :poem_date => 1946, :author_birthdate => 1897,
  :author_deathdate => 1982, :dtd_s => 'poemefr0'
}

$aragon_0 =
  Poem::new($aragon_metadata,"Il n'y a pas d'amour heureux",
            ["Rien n'est jamais acquis à l'homme Ni sa force",
             "Ni sa faiblesse ni son cœur Et quand il croit",
             "Ouvrir ses bras son ombre est celle d'une croix",
             "Et quand il croit serrer son bonheur il le broie",
             "Sa vie est un étrange et douloureux divorce",
             "\tIl n'y a pas d'amour heureux","",
             "Sa vie Elle ressemble à ces soldats sans armes", 
             "Qu'on avait habillés pour un autre destin",
             "À quoi peut leur servir de se lever matin", 
             "Eux qu'on retrouve au soir désœuvrés incertains",
             "Dites ces mots Ma vie Et retenez vos larmes",
             "\tIl n'y a pas d'amour heureux","",
             "Mon bel amour mon cher amour ma déchirure",
             "Je te porte dans moi comme un oiseau blessé",
             "Et ceux-là sans savoir nous regardent passer",
             "Répétant après moi les mots que j'ai tressés",
             "Et qui pour tes grands yeux tout aussitôt moururent", 
             "\tIl n'y a pas d'amour heureux","",
             "Le temps d'apprendre à vivre il est déjà trop tard", 
             "Que pleurent dans la nuit nos cœurs à l'unisson", 
             "Ce qu'il faut de malheur pour la moindre chanson",
             "Ce qu'il faut de regrets pour payer un frisson", 
             "Ce qu'il faut de sanglots pour un air de guitare", 
             "\tIl n'y a pas d'amour heureux","",
             "Il n'y a pas d'amour qui ne soit à douleur",
             "Il n'y a pas d'amour dont on ne soit meurtri",
             "Il n'y a pas d'amour dont on ne soit flétri",
             "Et pas plus que de toi l'amour de la patrie",
             "Il n'y a pas d'amour qui ne vive de pleurs", 
             "\tIl n'y a pas d'amour heureux",
             "\tMais c'est notre amour à tous les deux"])

$aragon_1 =
  Poem::new($aragon_metadata,'La délaissée',
            ["Ne t'en va pas mon cœur ma vie",
             "Sans toi le ciel perd ses couleurs",
             "Désert des champs jardin sans fleurs","\tNe t'en va pas",
             "Ne t'en va pas où va le vent",
             "Sans toi tous les oiseaux s'envolent",
             "Et toutes les nuits sont folles","\tNe t'en va pas",
             "Ne t'en va pas où se perd l'eau",
             "Méprisant le bonheur des verres",
             "Et l'univers des arbres verts","\tNe t'en va pas",
             "Ne t'en va pas comme le sang",
             "Qui saute à la main qui me blesse",
             "Ma chère force et ma faiblesse","\tNe t'en va pas",
             "Ne t'en va pas où fuit le feu",
             "Quand la paille à peine défaille",
             "Qu'elle est cendre pour qu'il s'en aille","\tNe t'en va pas",
             "Ne t'en va pas dans les nuées","Mon bel aigle ami des orages",
             "Je peux mourir de ton courage","\tNe t'en va pas",
             "Ne t'en va pas chez l'ennemi",
             "Qui t'a pris ta terre et tes armes",
             "Crois en la mémoire des larmes","\tNe t'en va pas",
             "Ne t'en va pas c'est félonie",
             "Ces discours ces chansons ces fêtes",
             "Homme sachez ce que vous faites","\tNe t'en va pas",
             "Ne t'en va pas où l'on te dit",
             "Avec des grands mots pour enseigne",
             "Quand c'est la blessure qui saigne","\tNe t'en va pas",
             "Ne t'en va pas chez le tyran","Forger sa puissance toi-même",
             "Et des fers pour ceux que tu aimes","\tNe t'en va pas",
             "Ne t'en va pas Prends ton fusil",
             "Siffle ton chien chasse les ombres",
             "Chasseur, chasseur tu es le nombre","\tNe t'en va pas","",
             "\tPrends ton fusil"])

$aragon_2 =
  Poem::new($aragon_metadata,'Du poète à son parti',
            ["Mon parti m'a rendu mes yeux et ma mémoire", 
             "Je ne savais plus rien de ce qu'un enfant sait", 
             "Que mon sang fût si rouge et mon cœur fût français", 
             "Je savais seulement que la nuit était noire", 
             "Mon parti m'a rendu mes yeux et ma mémoire","",
             "Mon parti m'a rendu le sens de l'épopée", 
             "Je vois Jeanne filer Roland sonne le cor", 
             "C'est le temps des héros qui renaît au Vercors", 
             "Les plus simples des mots font le bruit des épées", 
             "Mon parti m'a rendu le sens de l'épopée","",
             "Mon parti m'a rendu les couleurs de la France", 
             "Mon parti mon parti merci de tes leçons", 
             "Et depuis ce temps-là tout me vient en chansons", 
             "La colère et l'amour la joie et la souffrance", 
             "Mon parti m'a rendu les couleurs de la France"])

$aragon_3 =
  Poem::new($aragon_metadata,"D'une petite fille massacrée",
            ["Vous pourrez revenir ce sera vainement",
             "Surenchérir l'enfer et la bête féroce",
             "Vous pourrez enfoncer la porte avec vos crosses","\tAllemands",
             "","Vous n'éveillerez pas cette enfant Elle est morte", 
             "Avant d'avoir ouvert tout à fait ses grands yeux", 
             "Rien ne la tirera du rêve merveilleux","\tQui l'emporte","",
             "Dans ses cheveux défaits elle dort On croirait", 
             "Vraiment qu'elle va respirer qu'elle respire", 
             "Dans ses petites mains la nuit met son empire","\tEn secret","",
             "Elle ne porte plus le poids de sa mémoire",
             "La rose pour mourir a simplement pâli", 
             "Doucement doucement doucement elle oublie","\tVivre et voir"])

prefix = 'aragon-generated-'
suffix = '.xml'

[$aragon_0,$aragon_1,$aragon_2,$aragon_3].each_with_index do |aragon,index|
  Ruby2xml.ruby2xml(aragon,prefix + index.to_s + suffix)
end
