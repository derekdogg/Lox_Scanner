# Lox_Scanner

basic scanner which parses text and spits out lox tokens. 
Code works by 
1) loading complete text into one long string
2) using char iterator
3) using line iterator on char iterator
4) parsing each line.


simple example hooked to a button.

procedure TForm1.Button2Click(Sender: TObject);
var
  Scanner : TScanner;
  I : integer;
  Token : TToken;
begin
  Memo2.lines.clear;

  Scanner.Init(Memo1.Lines.Text);
  Scanner.Scan;
  Memo2.Lines.BeginUpdate;
  for i := 0 to Scanner.TokenCount-1  do
  begin
     token := Scanner.Tokens[i];
     Memo2.Lines.Add(TTokenName[token.kind] + '=' + token.text);
  end;

  Memo2.Lines.EndUpdate;
end;




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



