unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Registry, Unit2;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonSelectPath: TButton;
    ButtonInstall: TButton;
    ButtonUninstall: TButton;
    EditPath: TEdit;
    LabelCaption: TLabel;
    LabelSelectGDPath: TLabel;
    OpenDialog: TOpenDialog;
    procedure ButtonInstallClick(Sender: TObject);
    procedure ButtonSelectPathClick(Sender: TObject);
    procedure ButtonUninstallClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ButtonSelectPathClick(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    EditPath.Text := OpenDialog.FileName;
  end;
end;

procedure TFormMain.ButtonInstallClick(Sender: TObject);
var
  Path: String;
begin
  Path := EditPath.Text;
  if (Length(Path) <> 0) and FileExists(Path) then begin
    FormInstall.GDPath := Path;
		FormInstall.ShowModal;
  end else begin
  	MessageDlg('Error', 'Invalid path. Please ensure that you have selected the Geometry Dash file', mtError, [mbOK], 0);
  end;
end;

procedure TFormMain.ButtonUninstallClick(Sender: TObject);
var
  Path: String;
begin
  Path := EditPath.Text;
  if (Length(Path) <> 0) and FileExists(Path) then begin
    Path := ExtractFilePath(EditPath.Text);
    if FileExists(Path + 'libExtensions.dll.bak') then begin
    	try
  			DeleteFile(Path + 'libExtensions.dll');
  			RenameFile(Path + 'libExtensions.dll.bak', Path + 'libExtensions.dll');
        MessageDlg('Inforamiton', 'GDH has been uninstalled', mtInformation, [mbOK], 0);
  		except
  		  on E: Exception do
  		  begin
  		  	MessageDlg('Error', E.Message, mtError, [mbOK], 0);
  		  end;
  		end;
    end else begin
    	MessageDlg('Error', 'GDH is not installed', mtError, [mbOK], 0);
    end;
  end else begin
  	MessageDlg('Error', 'Invalid path. Please ensure that you have selected the Geometry Dash file', mtError, [mbOK], 0);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Registry: TRegistry;
  SteamInstallPath: string;
  GDInstallPath: String;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('SOFTWARE\WOW6432Node\Valve\Steam') then
    begin
      SteamInstallPath := Registry.ReadString('InstallPath');
      Registry.CloseKey;

      GDInstallPath := SteamInstallPath + '\steamapps\common\Geometry Dash\GeometryDash.exe';
      if FileExists(GDInstallPath) then EditPath.Text := GDInstallPath;
    end
  finally
    Registry.Free;
  end;
end;

end.

