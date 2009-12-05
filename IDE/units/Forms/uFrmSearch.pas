unit uFrmSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSearchDialog = class(TForm)
    lbSearchTitle: TLabel;
    cbSearchText: TComboBox;
    gbOptions: TGroupBox;
    MainPanel: TPanel;
    cbCaseSensitive: TCheckBox;
    cbWholeWords: TCheckBox;
    cbRegularExpression: TCheckBox;
    rgSearchDirection: TRadioGroup;
    rgAreaSelection: TRadioGroup;
    rgSearchContent: TRadioGroup;
    lbTitle: TLabel;
    btOk: TButton;
    btCancel: TButton;
    lbTitleBlue: TLabel;
    cbListSearch: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbSearchTextKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbSearchTextKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
    function GetListCompleteResult: boolean;
    function GetSearchBackwards: boolean;
    function GetSearchCaseSensitive: boolean;
    function GetSearchFromCursor: boolean;
    function GetSearchInSelection: boolean;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    function GetSearchWholeWords: boolean;
    procedure SetSearchBackwards(Value: boolean);
    procedure SetSearchCaseSensitive(Value: boolean);
    procedure SetSearchFromCursor(Value: boolean);
    procedure SetSearchInSelection(Value: boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(Value: string);
    procedure SetSearchWholeWords(Value: boolean);
    procedure SetSearchRegularExpression(const Value: boolean);  
    procedure SetListCompleteResult(const Value: boolean);
    function GetSearchRegularExpression: boolean;
  public
    { Public-Deklarationen }
    property SearchBackwards: boolean         read GetSearchBackwards           write SetSearchBackwards;
    property SearchCaseSensitive: boolean     read GetSearchCaseSensitive       write SetSearchCaseSensitive;
    property SearchFromCursor: boolean        read GetSearchFromCursor          write SetSearchFromCursor;
    property SearchInSelectionOnly: boolean   read GetSearchInSelection         write SetSearchInSelection;
    property SearchText: string               read GetSearchText                write SetSearchText;
    property SearchTextHistory: string        read GetSearchTextHistory         write SetSearchTextHistory;
    property SearchWholeWords: boolean        read GetSearchWholeWords          write SetSearchWholeWords;
    property SearchRegularExpression: boolean read GetSearchRegularExpression   write SetSearchRegularExpression;
    property ListCompleteSearch: boolean      read GetListCompleteResult        write SetListCompleteResult;
  end;

implementation

{$R *.dfm}

{ TSearchDialog }

function TSearchDialog.GetSearchBackwards: boolean;
begin                 
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TSearchDialog.GetSearchCaseSensitive: boolean;
begin                     
  Result := cbCaseSensitive.Checked;
end;

function TSearchDialog.GetSearchFromCursor: boolean;
begin                    
  Result := rgSearchContent.ItemIndex = 0;
end;

function TSearchDialog.GetSearchInSelection: boolean;
begin                           
  Result := rgAreaSelection.ItemIndex = 1;
end;

function TSearchDialog.GetSearchRegularExpression: boolean;
begin                
  Result := cbRegularExpression.Checked;
end;

function TSearchDialog.GetSearchText: string;
begin               
  Result := cbSearchText.Text;
end;

function TSearchDialog.GetSearchTextHistory: string;
var i: integer;
begin
  Result := '';
  for i := 0 to cbSearchText.Items.Count - 1 do
  begin
    if i >= 10 then
      break;
    if i > 0 then
      Result := Result + #13#10;
    Result := Result + cbSearchText.Items[i];
  end;
end;

function TSearchDialog.GetSearchWholeWords: boolean;
begin
  Result := cbWholeWords.Checked;
end;

procedure TSearchDialog.SetSearchBackwards(Value: boolean);
begin
  if value then
     rgSearchDirection.ItemIndex := 1
  else
     rgSearchDirection.ItemIndex := 0;
end;

procedure TSearchDialog.SetSearchCaseSensitive(Value: boolean);
begin
  cbCaseSensitive.Checked := value;
end;

procedure TSearchDialog.SetSearchFromCursor(Value: boolean);
begin
  if value then
     rgSearchContent.ItemIndex := 0
  else
     rgSearchContent.ItemIndex := 1;
end;

procedure TSearchDialog.SetSearchInSelection(Value: boolean);
begin
  if value then
     rgAreaSelection.ItemIndex := 1
  else
     rgAreaSelection.ItemIndex := 0;
end;

procedure TSearchDialog.SetSearchRegularExpression(const Value: boolean);
begin
  cbRegularExpression.Checked := Value;
end;

procedure TSearchDialog.SetSearchText(Value: string);
begin
  cbSearchText.Text := value;
end;

procedure TSearchDialog.SetSearchTextHistory(Value: string);
begin                    
  cbSearchText.Items.Text := Value;
end;

procedure TSearchDialog.SetSearchWholeWords(Value: boolean);
begin
  cbWholeWords.Checked := value;
end;

procedure TSearchDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var s: string;
    i: integer;
begin
  if ModalResult = mrOK then
  begin
    s := cbSearchText.Text;
    if s <> '' then
    begin
      i := cbSearchText.Items.IndexOf(s);
      if i > -1 then
      begin
        cbSearchText.Items.Delete(i);
        cbSearchText.Items.Insert(0, s);
        cbSearchText.Text := s;
      end else
        cbSearchText.Items.Insert(0, s);
    end;
  end;
end;

function TSearchDialog.GetListCompleteResult: boolean;
begin
  result := cbListSearch.Checked;
end;

procedure TSearchDialog.SetListCompleteResult(const Value: boolean);
begin              
  cbListSearch.Checked := value;
end;

procedure TSearchDialog.cbSearchTextKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then
  begin
    key := 0;
    ModalResult := mrOk;
  end;
end;

procedure TSearchDialog.cbSearchTextKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
     Key := #0;
end;

end.
