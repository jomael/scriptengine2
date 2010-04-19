unit uFrmAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls;

type
  TfrmAbout = class(TForm)
    imgAbout: TImage;
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TfrmAbout.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ModalResult := mrOk;
end;

procedure TfrmAbout.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin                  
  ModalResult := mrOk;
end;

procedure TfrmAbout.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Owner is TForm then
     Params.WndParent := TForm(Owner).Handle;
end;

end.
