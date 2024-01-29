unit FmValueStack;

interface

uses
  Windows,  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, values,LoxTypes, StdCtrls, arrays, ComCtrls;


 type
  TForm2 = class(TForm)
    btnTokenizer: TButton;
    MemoEdit: TMemo;
    lvWords: TListView;
    Button2: TButton;
    EdtTokenCount: TEdit;
    lblTokenCount: TLabel;

    procedure btnTokenizerClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FCount : integer;






  public
    { Public declarations }

  end;



var
  Form2: TForm;



implementation

uses
  strUtils,DateUtils,newScanner, TokenArray, compiler;

{$R *.dfm}


 

procedure TForm2.btnTokenizerClick(Sender: TObject);
var
  Tokenizer : TTokenizer;
  i : integer;
  token : TToken;
  Item : TListItem;
  Iterator : TTokenIterator;
begin

  lvWords.Items.Clear;
  Tokenizer.Parse(memoEdit.Lines.text);
  edtTokenCount.Text := inttostr(Tokenizer.TokenCount);


  Iterator.Init(Tokenizer.Tokens,Tokenizer.TokenCount);

  While Iterator.MoveNext <> -1 do
  begin
    token := Iterator.Current;
    Item := lvWords.Items.Add;
    Item.Caption := token.Text;
    Item.SubItems.Add(IntToStr(token.Line));
    Item.SubItems.Add(IntToStr(token.Pos));
    Item.SubItems.Add(token.KindToStr);
  end;

end;
 

 procedure TForm2.Button2Click(Sender: TObject);
 begin
   memoedit.Lines.clear;
 end;



end.





