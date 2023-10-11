unit CompilerRules;

interface

uses
  LoxTypes;


type

  TCompilerRules = record
    FRules : TParseRules;
    procedure SetRule(const TokenKind : TTokenKind; const ParseRule : pParseRule);
    function GetRule(const TokenKind : TTokenKind) : pParseRule;
  public
    property Rule[const TokenKind : TTokenKind] : pParseRule read getRule write setRule; default;
  end;
 

implementation 
 

procedure TCompilerRules.SetRule(const TokenKind : TTokenKind; const ParseRule : pParseRule);
begin
  assert(assigned(ParseRule), 'parse rule is nil');
  FRules[TokenKind] := ParseRule;
end;


function TCompilerRules.GetRule(const TokenKind : TTokenKind) : pParseRule;
begin
  result := FRules[TokenKind];
end;


end.
