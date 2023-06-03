# Lox_Scanner

basic scanner which parses text and spits out lox tokens. 
Code works by 
1) breaking line into seperate chars
2) using char iterator
3) using line iterator on char iterator
4) parsing each line.




for example

class Breakfast {
  cook() {
    print "Eggs a-fryin'!";
  }

  serve(who) {
    print "Enjoy your breakfast, " + who + ".";
  }
}

var breakfast = Breakfast();
breakfast.serve("our favourite patron");


will produce these tokens

Class=Class

Identifier=Breakfast

CurlyOpenBrace=

Identifier=cook

open_bracket=

Close_Bracket=

CurlyOpenBrace=

Print=Print

Quotes="Eggs a-fryin'!"

CurlyCloseBrace=

Identifier=serve

open_bracket=

Identifier=who

Close_Bracket=

CurlyOpenBrace=

Print=Print

Quotes="Enjoy your breakfast, "

Plus=

Identifier=who

Plus=

Quotes="."

CurlyCloseBrace=

CurlyCloseBrace=

Var=Var

Identifier=breakfast

Equal=

Identifier=Breakfast

open_bracket=

Close_Bracket=

Identifier=breakfast

dot=

Identifier=serve

open_bracket=

Quotes="our favourite patron"

Close_Bracket=



