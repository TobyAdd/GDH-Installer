unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Registry, Unit2;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonSelectPath: TButton;
    ButtonUninstall: TButton;
    ButtonInstall: TButton;
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
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
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

procedure TMainForm.ButtonSelectPathClick(Sender: TObject);
begin
	if OpenDialog.Execute then begin
    EditPath.Text := OpenDialog.FileName;
  end;
end;

procedure TMainForm.ButtonInstallClick(Sender: TObject);
var
  Path: String;
begin
  Path := EditPath.Text;
  if (Length(Path) <> 0) and FileExists(Path) then begin
    if not FileExists(ExtractFilePath(Path) + '\Geode.dll') then begin
    	MessageDlg('Error', 'Geode is not installed, make sure it is installed', mtError, [mbOK], 0);
      Exit;
    end;

    FormInstall.GDPath := Path;
		FormInstall.ShowModal;
  end else begin
  	MessageDlg('Error', 'Invalid path. Please ensure that you have selected the Geometry Dash file', mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.ButtonUninstallClick(Sender: TObject);
var
  Path: String;
begin
  Path := EditPath.Text;
  if (Length(Path) <> 0) and FileExists(Path) then begin
    Path := ExtractFilePath(EditPath.Text);
    	if FileExists(Path + 'geode\mods\tobyadd.gdh.geode') then begin
    	  DeleteFile(Path + 'geode\mods\tobyadd.gdh.geode');
    	  MessageDlg('Inforamiton', 'GDH has been uninstalled', mtInformation, [mbOK], 0);
    	end else begin
    		MessageDlg('Error', 'GDH is not installed', mtError, [mbOK], 0);
    	end;
    	Exit;
  end;
end;

end.

