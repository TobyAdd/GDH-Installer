unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Windows, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, lazfileutils, fphttpclient, opensslsockets, jsonparser, fpjson, Clipbrd, Zipper, FileUtil;

type

  { TFormInstall }

  TFormInstall = class(TForm)
    LabelCaption: TLabel;
    LabelInfo: TLabel;
    MemoLogs: TMemo;
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
    Geode: Boolean;
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

function ZipArchive(const baseDir, zipFilename: string): Boolean;
var
  zip: TZipper;
  filesList: TStringList;
  i: Integer;
  relativePath: String;
begin
  Result := False;

  zip := TZipper.Create;
  try
    zip.Filename := zipFilename;
    filesList := TStringList.Create;
    try
      FindAllFiles(filesList, baseDir, '*.*', true);
      for i := 0 to filesList.Count - 1 do
      begin
        relativePath := CreateRelativePath(filesList[i], baseDir);
        zip.Entries.AddFileEntry(filesList[i], relativePath);
      end;
    finally
      filesList.Free;
    end;

    zip.ZipAllFiles;
    zip.SaveToFile(zipFilename);
    Result := True;
  finally
    zip.Free;
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

procedure ExtractResourceToFile(const ResourceName, FileName: string);
var
  ResStream: TResourceStream;
  FileStream: TFileStream;
begin
  ResStream := TResourceStream.Create(HInstance, ResourceName, Windows.RT_RCDATA);
  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    try
      FileStream.CopyFrom(ResStream, 0);
    finally
      FileStream.Free;
    end;
  finally
    ResStream.Free;
  end;
end;

procedure TFormInstall.TMyThread.Execute;
var
  Content: String;
  JSONData: TJSONData;
  DownloadURL: String;
  Temp: String;
  JSONData2: TJSONObject;
  JSONString: String;
  JSONFile: TextFile;
begin
  FormInstall.LabelInfo.Caption := 'Downloading the latest GDH release metadata...';
  Content := FPHTTPClientGet('https://api.github.com/repos/TobyAdd/GDH/releases/latest');
  if Content = 'Error' then
  begin
    MessageDlg('Error', 'Failed to fetch release information. Make sure you are connected to the internet', mtError, [mbOK], 0);
    FormInstall.Close;
  end
  else
  begin
    FormInstall.LabelInfo.Caption := 'Getting download link...';
    JSONData := GetJSON(Content);
    DownloadURL := JSONData.FindPath('assets[0].browser_download_url').AsString;
    Sleep(500); //Too fast

    FormInstall.LabelInfo.Caption := 'Downloading GDH...';
    Temp := GetTempDir;
    if FPHTTPClientDownload(DownloadURL, Temp + 'GDH.zip') then
    begin
      if FormInstall.Geode = True then
      begin
        FormInstall.LabelInfo.Caption := 'Faking GDH as Geode extension...';
        while FormInstall.Height < 390 do
        begin
          FormInstall.Height := FormInstall.Height + Round((395 - FormInstall.Height) / 10);
          FormInstall.Position := poDefault;
          FormInstall.Position := poMainFormCenter;
          Sleep(10);
        end;

        FormInstall.MemoLogs.Lines.Add('Making temp folder for GDH...');
        if DirectoryExists(Temp + 'GDH') then
        begin
          FormInstall.MemoLogs.Lines.Add('Deleting the previous temp version of GDH...');
          DeleteDirectory(Temp + 'GDH', false);
        end;
        CreateDir(Temp + 'GDH');

        FormInstall.MemoLogs.Lines.Add('Unzipping GDH.zip...');
        Sleep(1500);
        if UnzipArchive(Temp + 'GDH.zip', Temp + 'GDH') then
        begin
          FormInstall.MemoLogs.Lines.Add('Successfully unpacked!');

          FormInstall.MemoLogs.Lines.Add('Copying GDH folder to the Geometry Dash directory...');
          Sleep(500);
          if CopyDirTree(Temp + 'GDH\GDH', ExtractFilePath(FormInstall.GDPath) + 'GDH', [cffOverwriteFile, cffPreserveTime, cffCreateDestDirectory]) then
          begin
            FormInstall.MemoLogs.Lines.Add('Successfully copied!');

            FormInstall.MemoLogs.Lines.Add('Creating folder for building mod...');
            Sleep(200);
            CreateDir(Temp + 'GDH\Geode');

            FormInstall.MemoLogs.Lines.Add('Copying GDH.dll...');
            Sleep(200);
            CopyFile(Temp + 'GDH\GDH.dll', Temp + 'GDH\Geode\tobyadd.gdh.dll');

            FormInstall.MemoLogs.Lines.Add('Generating mod.json...');
            Sleep(1000);

            JSONData2 := TJSONObject.Create([
                'geode', '2.0.0',
                'gd', '2.204',
                'version', 'v6.6.6',
                'id', 'tobyadd.gdh',
                'name', 'GDH',
                'developer', 'TobyAdd',
                'description', 'Loaded on crutches under Geode.',
                'repository', 'https://github.com/TobyAdd/GDH',
                'resources', TJSONObject.Create,
                'early-load', True
              ]);

            JSONString := JSONData2.FormatJSON;

            AssignFile(JSONFile, Temp + 'GDH\Geode\mod.json');
            Rewrite(JSONFile);
            Write(JSONFile, JSONString);
            CloseFile(JSONFile);

            FormInstall.MemoLogs.Lines.Add('Copying logo.png...');
            Sleep(200);
            ExtractResourceToFile('LOGO', Temp + 'GDH\Geode\logo.png');

            FormInstall.MemoLogs.Lines.Add('Building Geode file...');
            Sleep(3000);
            if ZipArchive(Temp + 'GDH\Geode', ExtractFilePath(FormInstall.GDPath) + 'geode\mods\tobyadd.gdh.geode') then
            begin
              FormInstall.MemoLogs.Lines.Add('Successfully installed!');
              FormInstall.ProgressBarDownload.Style := pbstNormal;
              FormInstall.ProgressBarDownload.Position := 100;
              FormInstall.LabelInfo.Caption := 'Successfully installed';
              MessageDlg('Information', 'GDH successfully installed', mtInformation, [mbOK], 0);
            end
            else
            begin
              MessageDlg('Error', 'Failed to build Geode file', mtError, [mbOK], 0);
            end;
          end
          else
          begin
            MessageDlg('Error', 'Failed to copy GDH folder to Geometry Dash directory', mtError, [mbOK], 0);
          end;
        end
        else
        begin
          MessageDlg('Error', 'Failed to unzip GDH.zip', mtError, [mbOK], 0);
        end;

        while FormInstall.Height > 115 do
        begin
          FormInstall.Height := FormInstall.Height - Round((FormInstall.Height - 110) / 10);
          FormInstall.Position := poDefault;
          FormInstall.Position := poMainFormCenter;
          Sleep(10);
        end;

        Sleep(500);
      end
      else
      begin
        FormInstall.LabelInfo.Caption := 'Installing GDH...';
        Sleep(1500); //Too fast again
        if UnzipArchive(Temp + 'GDH.zip', ExtractFilePath(FormInstall.GDPath)) then
        begin
          FormInstall.ProgressBarDownload.Style := pbstNormal;
          FormInstall.ProgressBarDownload.Position := 100;
          FormInstall.LabelInfo.Caption := 'Successfully installed';
          MessageDlg('Information', 'GDH successfully installed', mtInformation, [mbOK], 0);
        end
        else
        begin
          MessageDlg('Error', 'Failed to install GDH', mtError, [mbOK], 0);
        end;
      end;
    end
    else
    begin
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
  FormInstall.Height := 110;
  FormInstall.Position := poDefault;
	FormInstall.Position := poMainFormCenter;
  FormInstall.MemoLogs.Lines.Clear;

  MyThread := TMyThread.Create(True);
  MyThread.Start;
end;

end.

