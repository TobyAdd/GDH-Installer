unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, fphttpclient, opensslsockets, jsonparser, fpjson, Clipbrd, Zipper;

type

  { TFormInstall }

  TFormInstall = class(TForm)
    LabelCaption: TLabel;
    LabelInfo: TLabel;
    ProgressBarDownload: TProgressBar;
    procedure FormShow(Sender: TObject);
  private
    type
    TMyThread = class(TThread)
    protected
      procedure Execute; override;
    end;
  public
    GDPath: String;
  end;

var
  FormInstall: TFormInstall;


implementation

{$R *.lfm}

{ TFormInstall }

function FPHTTPClientGet(URL: string): String;
var
  HttpClient: TFPHttpClient;
begin
  Result := '';
  HttpClient := TFPHttpClient.Create(nil);
  HttpClient.AddHeader('User-Agent', 'Mozilla/5.0');
  try
    try
      Result := HttpClient.Get(URL);
    except
      Result := 'Error';
    end;
  finally
    HttpClient.Free;
  end;
end;

function FPHTTPClientDownload(const URL, FileName: string): boolean;
var
  HTTPClient: TFPHttpClient;
  FileStream: TFileStream;
begin
  HTTPClient := TFPHttpClient.Create(nil);
  HTTPClient.AllowRedirect := true;
  try
    try
      FileStream := TFileStream.Create(FileName, fmCreate);
      try
        HTTPClient.Get(URL, FileStream);
        Result := True;
      finally
        FileStream.Free;
      end;
    except
      Result := False;
    end;
  finally
    HTTPClient.Free;
  end;
end;

function UnzipArchive(const ZipFileName, OutputFolder: string): boolean;
var
  ZipFile: TUnZipper;
begin
  try
    ZipFile := TUnZipper.Create;
    try
      ZipFile.FileName := ZipFileName;
      ZipFile.OutputPath := OutputFolder;
      ZipFile.Examine;
      ZipFile.UnZipAllFiles;
      Result := True;
    finally
      ZipFile.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error', E.Message, mtError, [mbOK], 0);
  		Result := False;
    end;
  end;
end;

procedure TFormInstall.TMyThread.Execute;
var
  Content: String;
  JSONData: TJSONData;
  DownloadURL: String;
  Temp: String;
begin
  FormInstall.LabelInfo.Caption := 'Downloading the latest GDH release metadata...';
	Content := FPHTTPClientGet('https://api.github.com/repos/TobyAdd/GDH/releases/latest');
  if Content = 'Error' then begin
    MessageDlg('Error', 'Failed to fetch release information. Make sure you are connected to the internet', mtError, [mbOK], 0);
    FormInstall.Close;
  end else begin
    FormInstall.LabelInfo.Caption := 'Getting download link...';
    JSONData := GetJSON(Content);
    DownloadURL := JSONData.FindPath('assets[0].browser_download_url').AsString;
    Sleep(500); //Too fast

    FormInstall.LabelInfo.Caption := 'Downloading GDH...';
    Temp := GetTempDir;
    if FPHTTPClientDownload(DownloadURL, Temp + 'GDH.zip') then begin
    	FormInstall.LabelInfo.Caption := 'Installing GDH...';
      Sleep(1500); //Too fast again
      if UnzipArchive(Temp + 'GDH.zip', ExtractFilePath(FormInstall.GDPath)) then begin
        FormInstall.ProgressBarDownload.Style := pbstNormal;
        FormInstall.ProgressBarDownload.Position := 100;
        FormInstall.LabelInfo.Caption := 'Successfully installed';
      	MessageDlg('Information', 'GDH successfully installed', mtInformation, [mbOK], 0);
      end else begin
      	MessageDlg('Error', 'Failed to install GDH', mtError, [mbOK], 0);
      end;
    end else begin
      MessageDlg('Error', 'Failed to download GDH', mtError, [mbOK], 0);
    end;
  end;
  FormInstall.Close;
end;

procedure TFormInstall.FormShow(Sender: TObject);
var
  MyThread: TMyThread;
begin
  FormInstall.ProgressBarDownload.Style := pbstMarquee;
  FormInstall.ProgressBarDownload.Position := 0;

  MyThread := TMyThread.Create(True);
  MyThread.Start;
end;

end.

